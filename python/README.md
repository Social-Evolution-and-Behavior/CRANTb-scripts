# CRANTb-scripts -- python

These scripts show examples of working with CRANTb data using pthon.

We want to interact with 1. the segmentation and synaptic data via the [CAVE client](https://caveclient.readthedocs.io/en/latest/index.html), 2. CAVE tables which are a convenient way of tracking neuron IDs relative to labels and 3. [our seatable](https://cloud.seatable.io/workspace/62919/dtable/CRANTb/?tid=0000&vid=0000) which is a user friendly way to curating data annotations, from where they can (sometimes) be pushed to a CAVE table.

CAVE tables offer better ID tracking and version control than seatable. We can do things like track proofread neurons, cell labels we wish to share / are confident in, NBLAST and other scores, seeds from seed planes, etc. They are easy to read from. However, CAVE tables are not always intuitive and harder to work with casually. They are captured in version snapshots - 'materialisations' - of the dataset.

Seatable is better for annotators as a single consolidated place to curate annotations and more fluidly workshop annotations. It has a more user friendly GUI. However, it is worse for versioning and lags in terms of neuron tracking.

You will need to [install](https://caveconnectome.github.io/CAVEclient/installation/) the python CAVE client.

You will need to [install](https://developer.seatable.io/clients/python_api/) the python seatable client.

## Functionality

They can handle:

- Integration with [our CAVE tables](https://proofreading.zetta.ai/info/) for storing various annotation information.
- Interacting with our our [seatable](https://cloud.seatable.io/workspace/62919/dtable/CRANTb/?tid=0000&vid=0000)
- L2 skeleton querying and conversion to [SWC format](http://www.neuronland.org/NLMorphologyConverter/MorphologyFormats/SWC/Spec.html)

## Future functionality

Eventually they will also handle:

- Mesh querying 
- Synapse querying
- Live connectivity analysis

## Authentication

To use these data, you will need to authenticate yourself with both CAVE and seatable.

[For seatable](https://seatable.io/en/docs/seatable-api/erzeugen-eines-api-tokens/), this involves obtaining your user API token and making it available as a system variable called `CRANTTABLE_TOKEN`. This variable should be readable by `os.getenv('CRANTTABLE_TOKEN')`

[For CAVE](https://caveclient.readthedocs.io/en/latest/guide/authentication.html), this involves saving your CAVE token as a `~/.cloudvolume/secrets/cave-secret.json`. This does not need to be exposed as a system variable. Specifically, it would be best to indicate the data origin by saving it as `~/.cloudvolume/secrets/data.proofreading.zetta.ai-cave-secret.json`, which would enable you to work in multiple CAVE projects more easily.

## Key concepts

Important concepts/tools include:

- The [**L2 cache**](https://caveclient.readthedocs.io/en/latest/guide/l2cache.html) which links supervoxel trees
- The [**chunked graph**](https://caveclient.readthedocs.io/en/latest/guide/chunkedgraph.html) that underlies our segmentation efforts
- CAVE table [**schema**](https://caveconnectome.github.io/CAVEclient/tutorials/schemas/)
- CAVE [**materialisations**](https://caveconnectome.github.io/CAVEclient/tutorials/materialization/) of the dataset
- [**navis**](https://navis-org.github.io/navis/) for neuroanatomical analyses and plotting

