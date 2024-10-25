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

