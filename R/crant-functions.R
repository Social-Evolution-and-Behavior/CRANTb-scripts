# hidden
append_status <- function(status, update){
  status=paste(c(status,update),collapse=",")
  update.col<-paste(sort(unique(unlist(strsplit(status,split=",|, ")))),collapse=",")
  gsub("^,| ","",update.col)
}

# hidden
subtract_status <- function(status, update){
  satuses <- sort(unique(unlist(strsplit(status,split=",|, "))))
  satuses <- sort(unique(setdiff(satuses,update)))
  update.col<-paste0(satuses,collapse=",")
  gsub("^,| ","",update.col)
}

# Get the root_id stored for a given metric prefix in root_id_processed
# e.g. get_processed_rootid("l2_576460752653449509,synapses_576460752653449509", "l2")
# returns "576460752653449509"
get_processed_rootid <- function(root_id_processed, prefix) {
  if (is.na(root_id_processed) || root_id_processed == "") return(NA_character_)
  entries <- unlist(strsplit(root_id_processed, split = ",|, "))
  pattern <- paste0("^", prefix, "_")
  match <- grep(pattern, entries, value = TRUE)
  if (length(match) == 0) return(NA_character_)
  sub(pattern, "", match[1])
}

# Set/update the root_id for a given metric prefix in root_id_processed
# e.g. set_processed_rootid("l2_OLD123,synapses_OLD456", "l2", "NEW789")
# returns "l2_NEW789,synapses_OLD456"
set_processed_rootid <- function(root_id_processed, prefix, root_id) {
  if (is.na(root_id_processed) || root_id_processed == "") {
    return(paste0(prefix, "_", root_id))
  }
  entries <- unlist(strsplit(root_id_processed, split = ",|, "))
  pattern <- paste0("^", prefix, "_")
  entries <- entries[!grepl(pattern, entries)]
  entries <- c(entries, paste0(prefix, "_", root_id))
  entries <- sort(unique(entries))
  paste(entries, collapse = ",")
}

# Check which neurons need metric updates based on root_id_processed tags
# Returns logical vector: TRUE = needs update
needs_metric_update <- function(root_ids, root_id_processed, prefix) {
  sapply(seq_along(root_ids), function(i) {
    rid <- root_ids[i]
    processed <- root_id_processed[i]
    if (is.na(rid) || rid == "" || rid == "0") return(FALSE)
    stored <- get_processed_rootid(processed, prefix)
    if (is.na(stored)) return(TRUE)
    stored != as.character(rid)
  })
}

# Remove stale local files for neurons whose root_id has changed.
# For each neuron that needs updating, deletes the file saved under
# the old root_id (extracted from root_id_processed).
# dir: directory containing the files
# ext: file extension (e.g. ".swc", ".csv")
# Returns character vector of deleted file paths (invisibly)
cleanup_stale_files <- function(root_ids, root_id_processed, prefix, dir, ext = ".csv") {
  deleted <- character(0)
  for (i in seq_along(root_ids)) {
    rid <- root_ids[i]
    processed <- root_id_processed[i]
    if (is.na(rid) || rid == "" || rid == "0") next
    old_rid <- get_processed_rootid(processed, prefix)
    if (is.na(old_rid)) next
    if (old_rid == as.character(rid)) next
    # Old root_id differs from current — delete the stale file
    old_file <- file.path(dir, paste0(old_rid, ext))
    if (file.exists(old_file)) {
      file.remove(old_file)
      deleted <- c(deleted, old_file)
    }
  }
  if (length(deleted) > 0) {
    message(sprintf("Cleaned up %d stale files from %s", length(deleted), dir))
  }
  invisible(deleted)
}

