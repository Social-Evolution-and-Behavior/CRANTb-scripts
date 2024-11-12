# CRANTb-Scripts

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

![crantb_example_neuron_ggplot](https://github.com/Social-Evolution-and-Behavior/CRANTb-R/blob/main/inst/images/crantb_example_neuron_ggplot.png?raw=true)

## Overview

This repository hosts code for scripts designed to process ant connectome data. You can share scripts that showcase cool analyses or underlie your research projects. By contributing, we can build a set of useful materials to help get other community members off the ground.

You can add:
- R scripts in the `CRANTb-scripts/R/` directory (R tools are found [here](https://github.com/flyconnectome/crantr?tab=readme-ov-file))
- Python scripts in the `CRANTb-scripts/python/` directory (Python tools are found [here](https://github.com/Social-Evolution-and-Behavior/crantpy))
- Images produced from your scripts in the `CRANTb-scripts/inst/images/` directory
- Lightweight data used by your scripts in the `CRANTb-scripts/inst/extdata/` directory
- Linux scripts related to the HMS data pipeline in the `CRANTb-scripts/o2/` directory

## Metadata Management in Seatable

![ant_table](https://github.com/Social-Evolution-and-Behavior/CRANTb-R/blob/main/inst/images/ant_table.png?raw=true)

We use Seatable, a powerful collaborative annotation tool, to manage metadata for the CRANTb connectome dataset. We encourage you to use Seatable rather than maintaining your own Google Sheets or similar.

Seatable works similarly to Google Sheets, but offers better filter views, data type management, and programmatic access. You can access it through the browser or the [Seatable app](https://seatable.io/en).

You can find our Seatable [here](https://cloud.seatable.io/workspace/62919/dtable/CRANTb/?tid=0000&vid=0000). If the link doesn't work, you can request access by contacting Lindsey Lopes.

Each row in the Seatable represents a CRANTb neuron, identified by a unique 16-digit integer `root_id`. As the CRANTb project is active, the `root_id` can change frequently due to edits and updates to the neurons.

The update logic is as follows:
1. `position` (voxel space) -> `supervoxel_id` -> `root_id`
2. If `position` and `supervoxel_id` are missing, `root_id` is updated directly, but this is less reliable.
3. The Seatable will take the most up-to-date `root_id` with the most number of voxels from the previous `root_id`. This means that if a neuron is split, the chosen `root_id` may not be correct.

Best practices:
- Add the `position` whenever possible, as it ensures you get the neuron at that specific location, regardless of its size, merges, or splits.
- Also add the `root_id` if you want to track a specific neuron.
- Use only `root_id` alone if you don't have a good position for the neuron yet, but want to track it.
- A good position is a point on the neuron that is unlikely to change during proofreading, such as the first branch point where the neuron splits into the axon and dendrite.

![ant_table_ids](https://github.com/Social-Evolution-and-Behavior/CRANTb-R/blob/main/inst/images/ant_table_ids.png?raw=true)

You can access the Seatable programmatically using the `crantr` package, if you have the necessary access credentials.

```r
remotes::github_install('flyconnectome/crantr')
library(crantr)

# Set your Seatable login credentials
crant_table_set_token(user="MY_EMAIL_FOR_SEATABLE",
                     pwd="MY_SEATABLE_PASSWORD",
                     url="https://cloud.seatable.io/")

# Read the Seatable data
ac <- crant_table_query()
```

You can also update rows in the Seatable automatically, but be careful when doing so. It's a good idea to take a "snapshot" of the Seatable before making any changes, to preserve a historical version.

```r
# Update rows
crant_table_update_rows(base="CRANTb", 
                       table = "CRANTb_meta", 
                       df = ac.new, 
                       append_allowed = FALSE, 
                       chunksize = 100)
```

For simpler updates, you can replace the entries in a specific column for a set of `root_ids`:

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

## O2 Data Pipeline

The O2 data pipeline facilitates the analysis of the CRANT (Clonal Raider ANT) connectome data sets. Relevant commands and scripts are in the `o2/` directory. One of its tasks is to update the metadata in our Seatable, used for work-in-progress annotations in CRANTb. The pipeline runs on a daily basis on [O2](https://harvardmed.atlassian.net/wiki/spaces/O2/overview) at Harvard Medical School.
