###########################################################
### Sync backbone_proofread CAVE table with seatable
###
### Adds neurons with "BACKBONE_PROOFREAD" in status
### (but not "NOT_BACKBONE_PROOFREAD") to the CAVE table.
### Removes annotations for neurons no longer marked as
### backbone proofread.
###########################################################
source("R/crant-startup.R")

local({

# User ID for CAVE annotations — set to your own CAVE user ID
cave_user_id <- 92L

###########################
### Read current state  ###
###########################

# Query seatable for all neurons with position and status
message("### crantb: querying seatable ###")
ac <- crant_table_query(
  sql = "SELECT _id, root_id, supervoxel_id, position, status FROM CRANTb_meta"
)
ac[ac == ""] <- NA
ac[ac == "0"] <- NA
ac <- ac %>%
  dplyr::filter(!is.na(root_id), !is.na(position)) %>%
  dplyr::distinct(root_id, .keep_all = TRUE)

# Determine which neurons should be backbone proofread
# Must have BACKBONE_PROOFREAD in status, but NOT NOT_BACKBONE_PROOFREAD
ac$is_backbone_proofread <- sapply(ac$status, function(s) {
  if (is.na(s)) return(FALSE)
  entries <- unlist(strsplit(s, split = ",|, "))
  entries <- trimws(entries)
  "BACKBONE_PROOFREAD" %in% entries & !("NOT_BACKBONE_PROOFREAD" %in% entries)
})

should_be_proofread <- ac %>% dplyr::filter(is_backbone_proofread)
should_not_be_proofread <- ac %>% dplyr::filter(!is_backbone_proofread)

message(sprintf("Seatable: %d neurons marked BACKBONE_PROOFREAD, %d not",
                nrow(should_be_proofread), nrow(should_not_be_proofread)))

# Read current CAVE backbone_proofread table
message("### crantb: reading CAVE backbone_proofread table ###")
cave_bp <- crant_backbone_proofread()

if (nrow(cave_bp) > 0) {
  # Convert CAVE positions to comparable strings
  cave_bp$position_str <- sapply(cave_bp$pt_position, function(p) {
    paste(as.integer(unlist(p)), collapse = ",")
  })
  cave_bp$pt_root_id <- as.character(cave_bp$pt_root_id)
  message(sprintf("CAVE backbone_proofread: %d existing annotations", nrow(cave_bp)))
} else {
  cave_bp$position_str <- character(0)
  cave_bp$pt_root_id <- character(0)
  message("CAVE backbone_proofread: 0 existing annotations")
}

# Normalise seatable positions for comparison (strip spaces)
should_be_proofread$position_clean <- gsub(" ", "", should_be_proofread$position)
should_not_be_proofread$position_clean <- gsub(" ", "", should_not_be_proofread$position)

##############################
### Determine what to add  ###
##############################

# Neurons that should be in CAVE but aren't: match by position
positions_in_cave <- cave_bp$position_str
to_add <- should_be_proofread %>%
  dplyr::filter(!(position_clean %in% positions_in_cave))

message(sprintf("%d neurons to ADD to backbone_proofread", nrow(to_add)))

#################################
### Determine what to remove  ###
#################################

# Annotations in CAVE whose position matches a neuron that should NOT be proofread
# (or whose position no longer appears in the seatable at all as proofread)
proofread_positions <- should_be_proofread$position_clean
to_remove <- cave_bp %>%
  dplyr::filter(!(position_str %in% proofread_positions))

message(sprintf("%d annotations to REMOVE from backbone_proofread", nrow(to_remove)))

##############################
### Add new annotations    ###
##############################

if (nrow(to_add) > 0) {
  message("### crantb: adding annotations to CAVE backbone_proofread ###")

  np <- reticulate::import("numpy")
  client <- crant_cave_client()
  stage <- client$annotation$stage_annotations("backbone_proofread")

  n_added <- 0
  for (i in seq_len(nrow(to_add))) {
    rid <- to_add$root_id[i]
    pos_str <- to_add$position_clean[i]
    pos <- as.integer(unlist(strsplit(pos_str, ",")))

    tryCatch({
      # Validate that position returns a valid root_id
      valid_id <- crant_xyz2id(pos, rawcoords = TRUE)
      if (valid_id == "0" || is.na(valid_id)) {
        message(sprintf("  Skipping %s: position does not return valid root_id", rid))
        next
      }

      stage$add(
        valid = TRUE,
        pt_position = np$array(pos),
        user_id = cave_user_id,
        valid_id = as.numeric(valid_id),
        proofread = TRUE
      )
      client$annotation$upload_staged_annotations(stage)
      stage$clear_annotations()
      n_added <- n_added + 1
    }, error = function(e) {
      message(sprintf("  Error adding %s: %s", rid, e$message))
      tryCatch(stage$clear_annotations(), error = function(e2) NULL)
    })

    if (i %% 50 == 0) message(sprintf("  Added %d/%d", i, nrow(to_add)))
  }

  message(sprintf("Added %d annotations to backbone_proofread", n_added))

  # Pause for CAVE to ingest new annotations (406 error if queried too soon)
  wait_secs <- max(n_added * 0.5, 5)
  message(sprintf("Waiting %.0fs for CAVE to ingest new annotations...", wait_secs))
  Sys.sleep(wait_secs)
}

################################
### Remove old annotations   ###
################################

if (nrow(to_remove) > 0) {
  message("### crantb: removing annotations from CAVE backbone_proofread ###")

  client <- crant_cave_client()
  # Convert annotation IDs from bit64::integer64 to regular R integer.
  # Arrow returns CAVE IDs as integer64, but reticulate misinterprets the
  # underlying double storage as tiny floats instead of proper integers.
  annotation_ids <- as.integer(to_remove$id)

  tryCatch({
    result <- client$annotation$delete_annotation("backbone_proofread", annotation_ids)
    message(sprintf("Removed %d annotations from backbone_proofread", length(result)))
  }, error = function(e) {
    message(sprintf("Error removing annotations: %s", e$message))
    message("Attempting one-by-one removal...")
    n_removed <- 0
    for (aid in annotation_ids) {
      tryCatch({
        client$annotation$delete_annotation("backbone_proofread", aid)
        n_removed <- n_removed + 1
      }, error = function(e2) {
        message(sprintf("  Failed to remove annotation %s: %s", aid, e2$message))
      })
    }
    message(sprintf("Removed %d/%d annotations", n_removed, length(annotation_ids)))
  })
}

##############################
### Verify final state     ###
##############################

message("### crantb: verifying CAVE backbone_proofread table ###")
cave_bp_final <- NULL
for (attempt in 1:5) {
  Sys.sleep(10)
  cave_bp_final <- tryCatch(
    crant_backbone_proofread(),
    error = function(e) {
      message(sprintf("  Verification attempt %d failed: %s", attempt, e$message))
      NULL
    }
  )
  if (!is.null(cave_bp_final)) break
  message(sprintf("  Retrying in 10s (attempt %d/5)...", attempt))
}
if (!is.null(cave_bp_final)) {
  message(sprintf("CAVE backbone_proofread now has %d annotations", nrow(cave_bp_final)))
} else {
  message("Could not verify CAVE table (may still be ingesting)")
}
message(sprintf("Seatable has %d neurons marked BACKBONE_PROOFREAD", nrow(should_be_proofread)))

message("### crantb: backbone proofread sync complete ###")

})
