####################
##### IDENTITY #####
####################

# What machine are we using?
user<-Sys.info()["user"]
machine<-"o2"

###################
##### OPTIONS #####
#################### 

# google account authentication
options(gargle_oauth_email="alexander.shakeel.bates@gmail.co.uk")
options(gargle_oob_default=TRUE)
options(pillar.sigfig=15)

#####################
##### LIBRARIES #####
#####################

# load required libraires
options(scipen = 999)
library(crantr)
hemibrainr:::suppress(library(bancr))
hemibrainr:::suppress(library(hemibrainr))
hemibrainr:::suppress(library(malevnc))
hemibrainr:::suppress(library(nat.nblast))
hemibrainr:::suppress(library(fafbseg))
hemibrainr:::suppress(library(jsonlite))
hemibrainr:::suppress(library(foreach))
hemibrainr:::suppress(library(nat.jrcbrains))
hemibrainr:::suppress(library(doMC))
hemibrainr:::suppress(library(doParallel))
hemibrainr:::suppress(library(progressr))
hemibrainr:::suppress(library(googledrive))
hemibrainr:::suppress(library(elmr))
hemibrainr:::suppress(library(dplyr))
hemibrainr:::suppress(library(tidyverse))
hemibrainr:::suppress(library(bit64))
hemibrainr:::suppress(library(reticulate))
hemibrainr:::suppress(library(RSQLite))
hemibrainr:::suppress(library(plyr))
hemibrainr:::suppress(library(slackr))
hemibrainr:::suppress(library(ggforce))
hemibrainr:::suppress(library(natcpp))
hemibrainr:::suppress(library(lubridate))
hemibrainr:::suppress(library(googlesheets4))
hemibrainr:::suppress(library(doSNOW))
hemibrainr:::suppress(library(fs))
hemibrainr:::suppress(library(purrr))
hemibrainr:::suppress(library(dplyr))
hemibrainr:::suppress(library(processx))
hemibrainr:::suppress(register_saalfeldlab_registrations())

# Source other custom functions
source("crant/crant-functions.R")

# get some nifty functions for easy use
`%dopar%` <- foreach::`%dopar%`
`%:%` <- foreach::`%:%`
load_assign <- hemibrainr:::load_assign
overlap_score_delta <- hemibrainr:::overlap_score_delta
check_package_available <- hemibrainr:::check_package_available
nullToNA <- hemibrainr:::nullToNA

################################
##### PARRALLEL PROCESSING #####
################################

# Cores
numCores.possible <- ceiling(ceiling(parallel::detectCores()))
numCores <- 10

# Register cores
if(is.na(numCores.possible)){
  numCores <- 1
}

#################
##### PATHS #####
#################

# Our intended working directory
wd <- "/home/ab714/CRANTb-R/"

# Data storage for crant
crant_data_storage <- "/n/data1/hms/neurobio/wilson/crant"
crant.save.path <- "/n/data1/hms/neurobio/wilson/crant"
rclone.path <- file.path(crant_data_storage,"googledrive")
crant.obj.save.path <- file.path(crant.save.path,"obj")
crant.swc.save.path <- file.path(crant.save.path,"swc")
crant.l2swc.save.path <- file.path(crant.save.path,"l1")
crant.split.save.path <- file.path(crant.save.path,"split")
crant.l2split.save.path <- file.path(crant.save.path,"l2split")
crant.synapses.save.path <- file.path(crant.save.path,"synapses")
crant.metrics.save.path <- file.path(crant.save.path,"metrics")
crant.meta.save.path <- file.path(crant.save.path,"meta")
crant.nt.save.path <- file.path(crant.save.path,"nt")
rda.dir <- file.path(crant.save.path,"crant","rda")
images.dir <- file.path(crant.save.path,"crant","images")
crant.connectivity.save.path <- file.path(crant.save.path,"connectivity")

# crant NBLAST results
crantsynapses <- file.path(crant.save.path,"synapses_full.csv")
crant.nblast.save.path <- file.path(crant.save.path,"matching")
crant.deform.save.path <- file.path(crant.save.path,"deformetrica")
crant.nblast.mirror.save.path <- file.path(crant.nblast.save.path,"mirror")

# manual matching results
crant.mirror.correct.match.path <- file.path(crant.nblast.mirror.save.path,"correct")

# temporary directory in case we need one
tmpdir <- file.path(crant_data_storage,"rtmp")

######################
##### PARAMETERS #####
######################

# Splitting parameters
polypre <- TRUE
split <- "synapses"
mode <- "centrifugal"
identifier <- paste(ifelse(polypre,"polypre","pre"),mode,split,sep="_")

# Neuronlist save method
dbClass <- "ZIP"
zip <- ifelse(dbClass=="ZIP",TRUE,FALSE)

# SQL column types
crant.col.types <- readr::cols(
  .default = readr::col_character(),
  cleft_segid  = readr::col_character(),
  centroid_x = readr::col_number(),
  centroid_y = readr::col_number(),
  centroid_z = readr::col_number(),
  bbox_bx = readr::col_number(),
  bbox_by = readr::col_number(),
  bbox_bz = readr::col_number(),
  bbox_ex = readr::col_number(),
  bbox_ey = readr::col_number(),
  bbox_ez = readr::col_number(),
  presyn_segid = readr::col_character(),
  postsyn_segid  = readr::col_character(),
  presyn_x = readr::col_integer(),
  presyn_y = readr::col_integer(),
  presyn_z = readr::col_integer(),
  postsyn_x = readr::col_integer(),
  postsyn_y = readr::col_integer(),
  postsyn_z = readr::col_integer(),
  clefthash = readr::col_number(),
  partnerhash = readr::col_integer(),
  size = readr::col_number(),
  l2_root = readr::col_number(),
  l2_nodes = readr::col_number(),
  l2_segments = readr::col_number(),
  l2_branchpoints = readr::col_number(),
  l2_endpoints = readr::col_number(),
  l2_cable_length = readr::col_number(),
  l2_n_trees = readr::col_number(),
  nb = readr::col_number(),
  score = readr::col_number(),
  hemibrain_nblast = readr::col_number(),
  fafb_nblast = readr::col_number(),
  manc_nblast = readr::col_number(),
  X = readr::col_number(),
  Y = readr::col_number(),
  Z = readr::col_number(),
  x = readr::col_number(),
  y = readr::col_number(),
  z = readr::col_number(),
  dcv_density = readr::col_number(),
  dcv_count = readr::col_integer(),
  acetylcholine= readr::col_number() , 
  glutamate= readr::col_number() , 
  gaba= readr::col_number() , 
  glycine= readr::col_number() , 
  dopamine= readr::col_number() , 
  serotonin= readr::col_number() , 
  octopamine= readr::col_number() , 
  tyramine= readr::col_number() , 
  histamine= readr::col_number() , 
  nitric_oxide= readr::col_number() ,
  `allatostatin-a`= readr::col_number() , 
  `allatostatin-c`= readr::col_number() , 
  amnesiac= readr::col_number() , 
  bursicon= readr::col_number() , 
  capability= readr::col_number() , 
  ccap= readr::col_number() , 
  ccha1= readr::col_number() , 
  cnma= readr::col_number() , 
  corazonin= readr::col_number() , 
  darc1= readr::col_number() , 
  dh31= readr::col_number() , 
  dh331= readr::col_number() , 
  dh44= readr::col_number() , 
  dilp2= readr::col_number() , 
  dilp3= readr::col_number() , 
  dilp5= readr::col_number() , 
  dnpf= readr::col_number() , 
  drosulfakinin= readr::col_number() , 
  eclosion_hormone= readr::col_number() , 
  fmrf= readr::col_number() , 
  fmrfa= readr::col_number() , 
  hugin= readr::col_number() ,
  itp= readr::col_number() , 
  leucokinin= readr::col_number() , 
  mip= readr::col_number() , 
  myosuppressin= readr::col_number() , 
  myosupressin= readr::col_number() , 
  natalisin= readr::col_number() , 
  negative= readr::col_number() , 
  neuropeptide= readr::col_number() , 
  neuropeptides= readr::col_number() , 
  npf= readr::col_number() , 
  nplp1= readr::col_number() , 
  orcokinin= readr::col_number() , 
  pdf= readr::col_number() , 
  proctolin= readr::col_number() , 
  sifamide= readr::col_number() , 
  snpf= readr::col_number() , 
  space_blanket= readr::col_number() , 
  tachykinin= readr::col_number() , 
  trissin= readr::col_number()
)

###################
##### REMOTES #####
###################

# Rclone
rclone <- FALSE

#########################
##### ANNOUNCEMENTS #####
#########################

# Messages for debugging
message("R_MAX_VSIZE: ", Sys.getenv("R_MAX_VSIZE"))
message(".libPaths: ", print(.libPaths()))
message("crantr: ", packageVersion("crantr"))
print("##### SESSION INFO #####")
print(sessionInfo())
print("##### SESSION INFO #####")

# # Find Blender with python
# reticulate::py_run_string("import os")
# reticulate::py_run_string("from distutils.spawn import find_executable")
# reticulate::py_run_string("blender_executable <- find_executable('blender')")
# reticulate::py_run_string("print('Blender executable:', blender_executable)")





