# This script shows how to read particular rows from seatable using a SQL query

# Load libraries
import seatable_api
import os
import numpy as np
import pandas as pd

# Configuration seatable variables
server_url = "https://cloud.seatable.io/"  # Replace with your SeaTable server URL
api_token = os.getenv('CRANTTABLE_TOKEN')  # Replace with your API token, save it and export it as a system variable called CRANTTABLE_TOKEN
workspace_id = "62919"                     # Your workspace ID, same or everyone on project
base_name="CRANTb"

# Login to seatable
ac=seatable_api.Account(login_name=[],password=[],server_url=server_url)
ac.token=api_token

# Initialize the Base object
base=ac.get_base(workspace_id=workspace_id,base_name=base_name)
base.auth()

# Execute the SQL query to retrieve data
query = "SELECT root_id, supervoxel_id, position, proofread, flow, super_class, cell_class, cell_type FROM CRANTb_meta"
query_results = base.query(query)

# Convert results to a pandas DataFrame
df = pd.DataFrame(query_results)  # Adjust 'results' based on the API response

# Filter the DataFrame for rows where 'proofread' is True
filtered_df = df[df['proofread'] == True]

# Display the filtered DataFrame
print(filtered_df)
