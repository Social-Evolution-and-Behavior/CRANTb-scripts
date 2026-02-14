#####################################
### Update CRANT meta in seatable ###
#####################################
source("R/crant-startup.R")

# save current version of the table
message("### crantb: saving seatable snapshot ###")
ac <- crant_table_query()
dir.create(file.path(crant.meta.save.path,"snapshots"), showWarnings = FALSE)
current_datetime <- Sys.time()
datetime_string <- format(current_datetime, "%Y-%m-%d")
readr::write_csv(ac, file.path(file.path(crant.meta.save.path,"snapshots"),paste0(datetime_string,"_crantb_seatable.csv")))

# run IDs update
message("### crantb: updating root ids in crant table for crantb ###")
crantr::crant_table_updateids()

# run ngl link update for issue neurons, decommissioned for now
#message("### crantb: updating ngl links in crant table for crantb ###")
#crantr:::crant_table_update_tracing()

# run L2 skeleton download for new/changed neurons
message("### crantb: downloading L2 skeletons ###")
source("R/crant-l2-download.R")

# run L2 metrics (nodes, cable length) and push to seatable
message("### crantb: calculating L2 metrics ###")
source("R/crant-l2-metrics.R")

# run synapse counts and push to seatable
message("### crantb: pulling synapse counts ###")
source("R/crant-synapses.R")

# sync backbone_proofread CAVE table with seatable status
message("### crantb: syncing backbone proofread CAVE table ###")
source("R/crant-backbone-proofread.R")

