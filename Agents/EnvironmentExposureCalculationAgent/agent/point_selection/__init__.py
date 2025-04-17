from flask import abort
from agent.point_selection.point_selection import create_ponits_table_self_defined_area


def select_points(table_name:str, point_selection: str, data: dict) -> str:
    if point_selection == 'selected_points':
        points = data['points']  # list of points
        # todo: need to fix the input
    elif point_selection == 'region':
        # get points in a preset region
        region = data['region']
        # todo: need to add this function
    elif point_selection == 'self_defined_region':
        create_ponits_table_self_defined_area(data.get('lng_start'), data.get('lng_end'), data.get('lng_step'),
                            data.get('lat_start'), data.get('lat_end'), data.get('lat_step'), table_name)
    else:
        abort(400, description="Unsupported point selection method")
    return table_name