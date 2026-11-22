###########################################################
### Twig Capture requests -> nightly Twig Capture runs
###
### Users flag a neuron in CRANTb_meta (request_twigcapture = TRUE). Each
### night this script:
###   1. refreshes the served catalog's index.json / neurons.json, so the
###      review tab's neuron filters follow SeaTable;
###   2. sets in_twigcapture: TRUE where the row's root_id is a neuron the
###      merge tool can show (directly, or as the latest root of a catalog
###      neuron);
###   3. for each requested neuron not yet in the tool and not already in
###      flight, submits Twig Capture (twigcapture
###      scripts/run_twigcapture_crantb_cpu.sbatch), a few roots per job;
###   4. clears request_twigcapture once the neuron is in the tool.
### Until those two columns exist it only does step 1.
###
### COLLISIONS. Several of these, or of their jobs, can overlap: a night's
### jobs can still be queued when the next night starts, cron can fire
### twice, someone can run it by hand. So:
###   - one dispatcher at a time: Slurm --dependency=singleton in the O2
###     wrapper, plus a lock (<state>/.dispatch.lock) for runs outside Slurm.
###     A second dispatcher exits without doing anything.
###   - a ledger (<state>/ledger.csv) records which job owns which root. A
###     root whose job is still queued or running is not submitted again; one
###     whose job ended without a result is retried, up to TC_MAX_ATTEMPTS.
###   - the runner claims each root before computing it, and rebuilds the
###     catalog under a lock, so even overlapping jobs never compute one
###     neuron twice or overwrite each other's index.
###   - SeaTable writes touch only request_twigcapture and in_twigcapture,
###     by row _id, so they cannot clobber the nightly update's columns.
### Locks are directories (atomic on NFS) shared with the twigcapture repo:
### see its scripts/locks.sh for how stale locks are detected and broken.
###
### Settings (environment, with defaults below): TC_REPO, TC_PY, TC_OUT_DIR,
### TC_STATE_DIR, TC_PARTITION, TC_ROOTS_PER_JOB, TC_MAX_PER_NIGHT,
### TC_MAX_ATTEMPTS, and TC_DRY_RUN=1 to report without submitting or
### writing to SeaTable.
###########################################################
suppressMessages({
  library(crantr)
  library(jsonlite)
})

cfg <- list(
  repo = Sys.getenv("TC_REPO", "/n/data3_vast/hms/neurobio/htem2/users/ab714/twigcapture"),
  py = Sys.getenv("TC_PY", "/n/data3_vast/hms/neurobio/htem2/users/ab714/envs/tc_crantb/bin/python"),
  # The catalog the review tab serves (radagast /twigcapture_crantb_test/).
  out_dir = Sys.getenv("TC_OUT_DIR", "/n/data3_vast/hms/neurobio/htem2/twig_capture/crantb_test_aedes_v2.1"),
  state = Sys.getenv("TC_STATE_DIR", "/n/data3_vast/hms/neurobio/htem2/twig_capture/crantb_requests"),
  partition = Sys.getenv("TC_PARTITION", "short"),
  per_job = as.integer(Sys.getenv("TC_ROOTS_PER_JOB", "4")),
  max_night = as.integer(Sys.getenv("TC_MAX_PER_NIGHT", "24")),
  max_attempts = as.integer(Sys.getenv("TC_MAX_ATTEMPTS", "2")),
  dry_run = nzchar(Sys.getenv("TC_DRY_RUN"))
)
REQUEST_COL <- "request_twigcapture"
IN_COL <- "in_twigcapture"
LOCKS <- file.path(cfg$repo, "scripts", "locks.sh")
RUNNER <- file.path(cfg$repo, "scripts", "run_twigcapture_crantb_cpu.sbatch")
LEDGER <- file.path(cfg$state, "ledger.csv")
LEDGER_COLS <- c("root_id", "job_id", "roots_file", "submitted", "attempts", "status", "updated")

# This process, as the lock owner: the Slurm job, else this R process.
OWNER <- if (nzchar(Sys.getenv("SLURM_JOB_ID"))) {
  paste0("slurm:", Sys.getenv("SLURM_JOB_ID"))
} else {
  sprintf("pid:%s:%d", system("hostname -s", intern = TRUE), Sys.getpid())
}
locks_sh <- function(...) {
  system2("bash", c(LOCKS, ...), env = paste0("LOCK_OWNER=", OWNER))
}
now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
truthy <- function(x) tolower(as.character(x)) %in% c("true", "1", "yes", "t")

# Is a Slurm job possibly still queued or running? Unknown counts as yes.
job_alive <- function(job) {
  if (is.na(job) || !nzchar(job)) return(FALSE)
  system2("bash", c("-c", shQuote(sprintf('source %s; lock_owner_alive slurm:%s', LOCKS, job)))) == 0
}

read_ledger <- function() {
  if (!file.exists(LEDGER)) {
    return(as.data.frame(setNames(replicate(length(LEDGER_COLS), character(0), simplify = FALSE), LEDGER_COLS)))
  }
  utils::read.csv(LEDGER, colClasses = "character")
}
write_ledger <- function(ledger) {
  tmp <- paste0(LEDGER, ".tmp")
  utils::write.csv(ledger[, LEDGER_COLS], tmp, row.names = FALSE)
  file.rename(tmp, LEDGER)
}

# Roots with a result file in the catalog (<root>_<YYYYmmdd>_<HHMMSS>.json).
catalog_roots <- function() {
  f <- list.files(cfg$out_dir, pattern = "^[0-9]+_[0-9]{8}_[0-9]{6}\\.json$")
  unique(sub("_.*", "", f))
}

# SeaTable roots the merge tool can show: each catalog neuron's own root, or
# the latest root neurons.json matched it to.
tool_roots <- function() {
  path <- file.path(cfg$out_dir, "neurons.json")
  if (!file.exists(path)) return(catalog_roots())
  nj <- jsonlite::fromJSON(path, simplifyVector = FALSE)$neurons
  unique(c(catalog_roots(), vapply(names(nj), function(k) {
    s <- nj[[k]]$seatable_root
    if (is.null(s)) k else as.character(s)
  }, character(1))))
}

refresh_catalog <- function() {
  message("### twigcapture: refreshing index.json and neurons.json ###")
  cmd <- sprintf("%s %s %s && %s %s --dataset crantb %s",
                 cfg$py, file.path(cfg$repo, "scripts", "make_results_index.py"), shQuote(cfg$out_dir),
                 cfg$py, file.path(cfg$repo, "scripts", "make_seatable_meta.py"), shQuote(cfg$out_dir))
  rc <- locks_sh("run", file.path(cfg$out_dir, ".index.lock"), "1800", "--", "bash", "-c", shQuote(cmd))
  if (rc != 0) message("catalog refresh did not complete (exit ", rc, "); continuing")
}

update_rows <- function(ids, col, value) {
  if (!length(ids)) return(invisible())
  message(sprintf("setting %s = %s on %d row(s)%s", col, value, length(ids),
                  if (cfg$dry_run) " [dry run: not written]" else ""))
  if (cfg$dry_run) return(invisible())
  df <- data.frame(`_id` = ids, check.names = FALSE)
  df[[col]] <- rep(value, length(ids))
  crant_table_update_rows(df = df, table = "CRANTb_meta", base = "CRANTb",
                          append_allowed = FALSE, chunksize = 1000)
}

submit <- function(roots, attempts_before) {
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  dir.create(file.path(cfg$state, "roots"), showWarnings = FALSE, recursive = TRUE)
  chunks <- split(roots, ceiling(seq_along(roots) / cfg$per_job))
  out <- list()
  for (k in seq_along(chunks)) {
    rf <- file.path(cfg$state, "roots", sprintf("req_%s_%02d.txt", stamp, k))
    writeLines(chunks[[k]], rf)
    args <- c("--parsable", "-p", cfg$partition, "--job-name=tc_crantb_req",
              paste0("--chdir=", cfg$repo),
              paste0("--output=", file.path(cfg$repo, "logs", Sys.getenv("USER"), "tc_crantb_req_%j.out")),
              paste0("--export=ALL,TWIGCAPTURE_PROJECT=", cfg$repo, ",ROOTS=", rf, ",OUT_DIR=", cfg$out_dir),
              RUNNER)
    if (cfg$dry_run) {
      message("[dry run] sbatch ", paste(args, collapse = " "), "   roots: ", paste(chunks[[k]], collapse = " "))
      next
    }
    job <- tryCatch(system2("sbatch", args, stdout = TRUE), error = function(e) character(0))
    job <- sub(";.*", "", job[length(job)])
    if (!length(job) || !grepl("^[0-9]+$", job)) {
      message("!! sbatch failed for ", rf, "; these roots will be retried next night")
      next
    }
    message(sprintf("submitted job %s: %s", job, paste(chunks[[k]], collapse = " ")))
    out[[k]] <- data.frame(root_id = chunks[[k]], job_id = job, roots_file = rf,
                           submitted = now(), attempts = as.character(attempts_before[chunks[[k]]] + 1),
                           status = "submitted", updated = now())
  }
  do.call(rbind, out)
}

main <- function() {
  refresh_catalog()

  cols <- names(crant_table_query(sql = "SELECT * FROM CRANTb_meta", limit = 1))
  has_req <- REQUEST_COL %in% cols
  has_in <- IN_COL %in% cols
  # Testing before the columns exist: with TC_DRY_RUN, TC_TEST_REQUESTS (root
  # ids) stands in for request_twigcapture, and both columns are simulated.
  test_req <- strsplit(Sys.getenv("TC_TEST_REQUESTS"), "[, ]+")[[1]]
  test_req <- test_req[nzchar(test_req)]
  simulate <- cfg$dry_run && length(test_req) > 0
  if (simulate) message("[dry run] simulating requests for: ", paste(test_req, collapse = " "))
  if (!has_req && !has_in && !simulate) {
    message(sprintf("### twigcapture: CRANTb_meta has no %s / %s columns yet; nothing to dispatch ###",
                    REQUEST_COL, IN_COL))
    return(invisible())
  }
  sel <- c("`_id`", "root_id", if (has_req) REQUEST_COL, if (has_in) IN_COL)
  ac <- crant_table_query(sql = sprintf("SELECT %s FROM CRANTb_meta", paste(sel, collapse = ", ")))
  if (simulate) {
    ac[[REQUEST_COL]] <- as.character(ac$root_id) %in% test_req
    if (!has_in) ac[[IN_COL]] <- NA
    has_req <- has_in <- TRUE
  }
  ac$root_id <- as.character(ac$root_id)
  ac$root_id[ac$root_id %in% c("", "0")] <- NA
  in_tool <- tool_roots()

  # in_twigcapture: only rows whose value changes are written.
  if (has_in) {
    new <- !is.na(ac$root_id) & ac$root_id %in% in_tool
    changed <- new != truthy(ac[[IN_COL]])
    message(sprintf("### twigcapture: %d row(s) in the tool; %d to update ###", sum(new), sum(changed)))
    update_rows(ac$`_id`[changed & new], IN_COL, TRUE)
    update_rows(ac$`_id`[changed & !new], IN_COL, FALSE)
  }
  if (!has_req) return(invisible())

  # Settle last nights' jobs: done if the root now has a result, failed if
  # its job is over without one.
  ledger <- read_ledger()
  have <- catalog_roots()
  for (i in which(ledger$status == "submitted")) {
    if (ledger$root_id[i] %in% have) {
      ledger$status[i] <- "done"; ledger$updated[i] <- now()
    } else if (!job_alive(ledger$job_id[i])) {
      ledger$status[i] <- "failed"; ledger$updated[i] <- now()
      message(sprintf("job %s ended without a result for %s (attempt %s)",
                      ledger$job_id[i], ledger$root_id[i], ledger$attempts[i]))
    }
  }

  req <- ac[truthy(ac[[REQUEST_COL]]) & !is.na(ac$root_id), c("_id", "root_id")]
  message(sprintf("### twigcapture: %d requested row(s), %d neuron(s) ###",
                  nrow(req), length(unique(req$root_id))))
  satisfied <- req$root_id %in% in_tool
  update_rows(req$`_id`[satisfied], REQUEST_COL, FALSE)
  todo <- unique(req$root_id[!satisfied])

  # SeaTable's root ids are refreshed twice a day, so a requested root may
  # already be retired. Run the neuron's latest root instead; a split (more
  # than one latest root) is left for a person.
  if (length(todo)) {
    latest <- tryCatch(crant_islatest(todo), error = function(e) rep(TRUE, length(todo)))
    for (i in which(!latest)) {
      l <- tryCatch(as.character(crant_latestid(todo[i])), error = function(e) character(0))
      if (length(l) == 1 && !is.na(l)) {
        message(sprintf("%s is retired; running its latest root %s", todo[i], l))
        todo[i] <- l
      } else {
        message(sprintf("%s is retired and has %d latest roots; skipped", todo[i], length(l)))
        todo[i] <- NA
      }
    }
    todo <- unique(todo[!is.na(todo) & !(todo %in% in_tool)])
  }

  attempts <- tapply(as.integer(ledger$attempts), ledger$root_id, max)
  prev <- setNames(ifelse(todo %in% names(attempts), attempts[todo], 0L), todo)
  prev[is.na(prev)] <- 0L
  in_flight <- todo %in% ledger$root_id[ledger$status == "submitted"]
  gave_up <- !in_flight & prev >= cfg$max_attempts
  if (any(in_flight)) message(sprintf("%d neuron(s) already queued or running", sum(in_flight)))
  if (any(gave_up)) message(sprintf("%d neuron(s) failed %d times and are not retried: %s",
                                    sum(gave_up), cfg$max_attempts, paste(todo[gave_up], collapse = " ")))
  go <- todo[!in_flight & !gave_up]
  if (length(go) > cfg$max_night) {
    message(sprintf("%d to run; %d tonight, the rest on later nights", length(go), cfg$max_night))
    go <- go[seq_len(cfg$max_night)]
  }
  if (length(go)) {
    new_rows <- submit(go, prev)
    if (!is.null(new_rows)) ledger <- rbind(ledger, new_rows[, LEDGER_COLS])
  } else {
    message("nothing new to submit")
  }
  if (!cfg$dry_run) write_ledger(ledger)
}

dir.create(cfg$state, showWarnings = FALSE, recursive = TRUE)
lock_dir <- file.path(cfg$state, ".dispatch.lock")
if (locks_sh("acquire", lock_dir, "0") != 0) {
  message("### twigcapture: another dispatcher holds ", lock_dir, "; exiting ###")
  quit(save = "no", status = 0)
}
tryCatch(main(), finally = locks_sh("release", lock_dir))
message("### twigcapture: dispatch complete ###")
