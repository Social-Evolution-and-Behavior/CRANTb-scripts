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

# run ngl link update for issue neurons
message("### crantb: updating ngl links in crant table for crantb ###")
crantr:::crant_table_update_tracing()

