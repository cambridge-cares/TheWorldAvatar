from flask import abort
from agent.config.params import PointSelectionParam
from agent.point_selection.point_selection import create_points_table_postal_code, create_ponits_table_self_defined_area


def select_points(query_id:str, point_selection: str, data: dict):
    if point_selection == PointSelectionParam.SelectedPoints.value:
        pass
    elif point_selection == PointSelectionParam.Region.value:
        pass
    elif point_selection == PointSelectionParam.SelfDefinedRegion.value:
        create_ponits_table_self_defined_area(data.get('lng_start'), data.get('lng_end'), data.get('lng_step'),
                            data.get('lat_start'), data.get('lat_end'), data.get('lat_step'), query_id)
    elif point_selection == PointSelectionParam.PostalCode.value:
        create_points_table_postal_code(query_id)
    else:
        abort(400, description="Unsupported point selection method")