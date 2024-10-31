#####################################
### Update CRANT meta in seatable ###
#####################################
source("R/crant-startup.R")

# save current version of the table
ac <- cranttable_query()
dir.create(file.path(crant.meta.save.path,"snapshots"), showWarnings = FALSE)
current_datetime <- Sys.time()
datetime_string <- format(current_datetime, "%Y-%m-%d")
readr::write_csv(ac, file.path(file.path(crant.meta.save.path,"snapshots"),paste0(datetime_string,"_crantb_seatable.csv")))

# run IDs update
crantr::crant_table_updateids()

# run ngl link update for issue neurons
crantr:::crant_table_update_tracing()

