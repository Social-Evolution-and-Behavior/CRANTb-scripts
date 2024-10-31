####################################
### Update CRANT IDS in seatable ###
####################################
source("banc/banc-startup.R")
crantr::crant_table_updateids()
crantr:::crant_table_annotate()
