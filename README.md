# CRANTb-R

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

![crantb_example_neuron_ggplot](https://github.com/Social-Evolution-and-Behavior/CRANTb-R/blob/main/inst/images/crantb_example_neuron_ggplot.png?raw=true)

## Overview

This repository hosts code for a pipeline designed to process ant connectome data. The pipeline facilitates the analysis of the CRANT (Clonal Raider ANT) connectome data sets. One of its tasks is to update meta data in our seatable, used for work-in-progress annotations in CRANTb, relevant code is in the subfolder `o2/` and runs on a daily basis on [O2](https://harvardmed.atlassian.net/wiki/spaces/O2/overview) at Harvard Medical School.

## Meta data management in seatable

![ant_table](https://github.com/Social-Evolution-and-Behavior/CRANTb-R/blob/main/inst/images/ant_table.png?raw=true)

Seatable is a powerful way to make collaborative annotations in this connectome dataset and we encourage you to use it rather than keeping your own google sheets or similar to track neurons.
It works similarly to google sheets, but has better filter views, data type management, programmatic access, etc. 
It should work in the browser and as an [app](https://seatable.io/en).

See our seatable [here](https://cloud.seatable.io/workspace/62919/dtable/CRANTb/?tid=0000&vid=0000).
If this link does not work you can request access by contacting Lindsey Lopes.

Each row is a `CRANTb` neuron. If you hover your tool-tip over the **i** icon in each column header, you can see what that column records.
Each neuron is identified by a 16-digit integer `root_id`, which is modified each time the neuron is edited.
As `CRANTb` is an active project, this happens frequently so our seatable needs to keep track of changes, which it does on a daily schedule.

The update logic is `position` (voxel space) -> `supervoxel_id` -> `root_id`.
If `position` and `supervoxel_id` are missing, `root_id` is updated directly but this is longer. 
It will also take the most up to date `root_id` with the most number of voxels from the previous root_id, so if a neuron is split this could be the incorrect choice. 
Updating from the `position` gives you the neuron at that position, regardless of size, merges or splits.
Best practice is probably to add position always if you can, and `root_id` also if you want. 
You may want to add only `root_id` if you want to track neuron but do not yet have a good position. 
A good position is a point in the neuron you expect not to change during proofreading, e.g. the first branch point in the neuron where it splits from the primary neurite into axon and dendrite.

![ant_table_ids](https://github.com/Social-Evolution-and-Behavior/CRANTb-R/blob/main/inst/images/ant_table_ids.png?raw=true)

You can access the seatable programmatically using the `crantr`, if you have access.

You will first need to obtain your authorised login credentials, you only need to do this once:

```r
crant_table_set_token(user="MY_EMAIL_FOR_SEATABLE.com",
               pwd="MY_SEATABLE_PASSWORD",
               url="https://cloud.seatable.io/")
```

And then you may read the data, and make nice plots from it!

```r
# Read BANC meta seatable
ac <- crant_table_query()
```

You can also update rows automatically. Be careful when doing this. If you want to be sure not to mess something up, 
you can take a 'snapshot' of the seatable before you edit it in the browser, which will save a historical version.

You can then change column in `ac`, keeping their names, as youl ike. Then to update via R:

```r
# Update
crant_table_update_rows(base="CRANTb"", 
                     table = "CRANTb_meta", 
                     df = bc.new, 
                     append_allowed = FALSE, 
                     chunksize = 100)
```

You can also make a quick, simpler update, replacing one column's entries with a given `update` for a set of root IDs.

```r
crant_table_annotate(root_ids = c("576460752667713229",
                               "576460752662519193",
                               "576460752730083020",
                               "576460752673660716",
                               "576460752662521753"),
                  update = "lindsey_lopes",
                  overwrite = FALSE,
                  append = FALSE,
                  column = "user_annotator")
```

To update, you must have the seatable identifier for each column in `ac.new`, i.e. an `_id` column.

This method is good for bulk uploads/changes.
