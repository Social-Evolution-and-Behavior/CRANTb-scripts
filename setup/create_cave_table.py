# Load libraries
import navis
import caveclient

# Read in data to make cable from
# Schema: https://globalv1.daf-apis.com/schema/views/

# Get/Initialize the CAVE client
client = get_cave_client(dataset=dataset)

navis.utils.eval_param(name, name='name', allowed_types=(str, ))
navis.utils.eval_param(schema, name='schema', allowed_types=(str, ))
navis.utils.eval_param(description, name='description', allowed_types=(str, ))
navis.utils.eval_param(voxel_resolution,
                       name='voxel_resolution',
                       allowed_types=(list, np.ndarray))

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
