# This script shows how to read a given neuron as a L2 meshparty skeleton and convert to SWC

# Load libraries
import caveclient
import pcg_skel
import pandas as pd
import numpy as np

# Connect to the correct CAVE client
client = caveclient.CAVEclient(
   datastack_name="kronauer_ant",
   server_address="https://proofreading.zetta.ai"
)

# Choose a neuron to work with, best way: read seatable/CAVE table
root_id = 576460752653449509

# Get skeleton!
skel = pcg_skel.pcg_skeleton(root_id=root_id, client=client)

##############################################
### Optionally, get skeleton in SWC format ###
##############################################

# Extract vertices and edges
vertices = skel.vertices
edges = skel.edges

# Create a dictionary to store node information
node_info = {}

# Initialize all nodes as endpoints (type 6)
for i, coord in enumerate(vertices):
    node_info[i] = {
        'PointNo': i + 1,  # SWC index (1-based)
        'type': 6,   # Default to endpoint
        'X': coord[0],
        'Y': coord[1],
        'Z': coord[2],
        'W': 1.0,  # Default W (radius), adjust if you have actual data
        'Parent': -1  # Default parent to -1 (root)
    }

# Update node types and parent information based on edges
for edge in edges:
    parent, child = edge
    node_info[parent]['type'] = 3  # Set as basal dendrite, adjust as needed
    node_info[child]['type'] = 3   # Set as basal dendrite, adjust as needed
    node_info[child]['Parent'] = parent + 1  # Set parent (convert to 1-based index)

# Find the root node (the one without a parent or with parent -1)
root = [n for n, info in node_info.items() if info['Parent'] == -1][0]
node_info[root]['type'] = 1  # Set root type

# Create DataFrame
df = pd.DataFrame.from_dict(node_info, orient='index')

# Reorder columns to match desired SWC format
df = df[['PointNo', 'type', 'X', 'Y', 'Z', 'W', 'Parent']]

# Sort by node index
df = df.sort_values('PointNo')

# write as .SWC file if you want!
df.to_csv('neuron.swc', index=False)



