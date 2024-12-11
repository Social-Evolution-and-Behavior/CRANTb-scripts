#######################################################
### *** DO NOT RE_RUN THIS CODE - EXAMPLE ONLY! *** ###
#######################################################
# This script created the backbone_proofread table in CRANT CAVE: https://data.proofreading.zetta.ai/annotation/views/aligned_volume/kronauer_ant/table/backbone_proofread

# Load libraries
import navis
import caveclient
import seatable_api
import os
import numpy as np
import pandas as pd
import datetime

############################
### information for CAVE ###
############################

# Read in data to make cable from
## You must choose a schema: https://globalv1.daf-apis.com/schema/views/
dataset="kronauer_ant"
voxel_resolution=[8, 8, 42]
schema="proofreading_boolstatus_user"
name="backbone_proofread"
description="Cells that have had their backbone proofread, meaning there are no major false merge errors in the object and all the major branches of the backbone have been extended so no significant parts of the neuron are missing. valid_id contains the segment ID of the neuron at the time the annotation was made. user_id contains the CAVE user ID of the user who created the annotation.."

########################
### connect for CAVE ###
########################

# Get/Initialize the CAVE client
client = caveclient.CAVEclient(datastack_name=dataset)
navis.utils.eval_param(name, name='name', allowed_types=(str, ))
navis.utils.eval_param(schema, name='schema', allowed_types=(str, ))
navis.utils.eval_param(description, name='description', allowed_types=(str, ))
navis.utils.eval_param(voxel_resolution,
                       name='voxel_resolution',
                       allowed_types=(list, np.ndarray))
                       
###############################
### create empty CAVE table ###
###############################

# Check it looks good
if isinstance(voxel_resolution, np.ndarray):
    voxel_resolution = voxel_resolution.flatten().tolist()

if len(voxel_resolution) != 3:
    raise ValueError('`voxel_resolution` must be list of [x, y, z], got '
                     f'{len(voxel_resolution)}')

resp = client.annotation.create_table(table_name=name,
                                      schema_name=schema,
                                      description=description,
                                      voxel_resolution=voxel_resolution)

if resp.content.decode() == name:
    print(f'Table "{resp.content.decode()}" successfully created.')
else:
    print('Something went wrong, check response.')
    return resp

# Change permissions
client.annotation.update_metadata(table_name=name,
                                  read_permission="PUBLIC")
                                  
################################
### information for seatable ###
################################

# Configuration seatable variables
server_url = "https://cloud.seatable.io/"  # Replace with your SeaTable server URL
api_token = os.getenv('CRANTTABLE_TOKEN')  # Replace with your API token
workspace_id = "62919"                     # Your workspace ID
base_name="CRANTb"

# Login to seatable
ac=seatable_api.Account(login_name=[],password=[],server_url=server_url)
ac.token=api_token

# Initialize the Base object
base=ac.get_base(workspace_id=workspace_id,base_name=base_name)
base.auth()

##########################
### read from seatable ###
##########################

# Execute the SQL query to retrieve data
query = "SELECT root_id, supervoxel_id, position, proofread FROM CRANTb_meta"
query_results = base.query(query)

# Convert results to a pandas DataFrame
df = pd.DataFrame(query_results)  # Adjust 'results' based on the API response

#################################################
### dataframe manipulation into schema format ###
#################################################

# Filter the DataFrame for rows where 'proofread' is True
filtered_df = df[df['proofread'] == True]

# Display the filtered DataFrame
print(filtered_df)

# Define a mapping of the columns you want to retain and their new names
new_column_names = {
    'proofread': 'proofread',
    'position': 'pt_position',
    'root_id': 'id'
}

# Select and rename the columns in the specified order
filtered_df_renamed = filtered_df[list(new_column_names.keys())].rename(columns=new_column_names)

# Add the 'user_id' column with constant value and set it in desired position
filtered_df_renamed['user_id'] = 92 # this is Alex Bate's user ID, please use your own!
filtered_df_renamed = filtered_df_renamed[['proofread', 'pt_position', 'user_id', 'id']]

# Function to convert string to a list of integers
def convert_position_to_list(position_str):
    # Split the string by commas, convert each segment to an integer, and return as a list
    return [int(x.strip()) for x in position_str.split(',')]

# Apply the conversion function to the 'pt_position' column
filtered_df_renamed['pt_position'] = filtered_df_renamed['pt_position'].apply(convert_position_to_list)

# duplicate id as valid_id
filtered_df_renamed['id'] = filtered_df_renamed['id'].astype('int64')
filtered_df_renamed['valid_id'] = filtered_df_renamed['id']

# Display the modified DataFrame
print(filtered_df_renamed)

################################
### stage our new table data ###
################################

# Upload to CAVE table
stage = client.annotation.stage_annotations(name)
update_stage = client.annotation.stage_annotations(name, update=True)
update_stage.add_dataframe(filtered_df_renamed)

##############################
### wrute to our new table ###
##############################

# upload to CAVE
client.annotation.post_annotation_df(table_name=name,df=filtered_df_renamed,position_columns=['pt_position'])
#client.annotation.upload_staged_annotations(update_stage)

# Check this has worked
all_tables = client.annotation.get_tables()
print(all_tables)
proofed = client.materialize.live_live_query(name, timestamp = datetime.datetime.now(datetime.timezone.utc))
print(proofed)
