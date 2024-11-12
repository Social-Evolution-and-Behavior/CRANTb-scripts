#####################################
### Update CRANT meta in seatable ###
#####################################

# Install/login, if you have not
##remotes::install_github('natverse/crantr')
##remotes::install_github('natverse/fafbseg@asb-dev')
##fafbseg::simple_python("full")
##fafbseg::simple_python('none', pkgs='cloud-volume~=8.32.1')
##crant_set_token()
##crant_table_login()

# Libraries
library(crantr)
library(ggplot2)

# Modify this line to choose another class of neuron
chosen.class="olfactory_projection_neuron"

# get meta data, will one dya be available via CAVE tables
ac <- crant_table_query()

# have a look at it!
View(ac)

# filter to get our IDs
pn.meta <- ac %>%
  dplyr::filter(cell_class==chosen.class)
  
# get our ids
pn.ids <- unique(pn.meta$root_id)

# update these IDs to their most current versions, they change after each proofreading edit
pn.ids <- crant_latestid(pn.ids)

# fetch
pn.meshes <- crant_read_neuron_meshes(pn.ids)

# plot brain
crant_view()
plot3d(crantb.surf, col = "lightgrey", alpha = 0.1)

# ggplot
crant_ggneuron(pn.meshes , volume = crantb.surf)
