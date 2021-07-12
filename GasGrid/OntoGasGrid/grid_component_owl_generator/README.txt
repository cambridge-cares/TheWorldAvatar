
closest_point_locate.py locates the closest grid connection to each grid component. This is then stored in the closest connection.csv file.
It then works out the closest node within the gas grid to each output, records this and stores it in a csv file to be appended manually to the list of offtakes.

This is then appended to the grid_component_data.csv file.

component_to_abox.py then takes this file and produces the correct abox in the folder components_abox.

