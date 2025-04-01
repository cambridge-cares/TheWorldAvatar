from datetime import date
import json
from flask import abort, jsonify, request
import pandas as pd

from agent.boundary import create_buffer_around_points
from agent.exposure.intersect import Intersect
from agent.output.save_to_table import save_to_table
from agent.point_selection.point_selection import create_ponits_table_self_defined_area
from agent.utils.table_name_helper import TableNameHelper


def register_route(app):

    @app.route("/")
    def hello_world():
        return "<p>Hello, World! test</p>"

    @app.route("/calculate", methods=['POST'])
    def calculate():       
        point_selection = request.args.get('point_selection')
        boundary = request.args.get('boundary')
        algorithm = request.args.get('algorithm')

        if not point_selection or not boundary or not algorithm:
            return jsonify({"error": "Missing required parameters"}), 400

        data = request.get_json()
        
        table_name_helper = TableNameHelper(data, request.args)
        # step1: point selection
        select_points(table_name_helper.get_table_name(), point_selection, data)

        # step2: create boundary around points
        create_boundary(table_name_helper, boundary, data)

        # step3: exposure calculation
        res = exposure_calculation(table_name_helper, algorithm)

        # step4: (Optional) output result
        save_to_table(res, table_name_helper)
        output_format = data.get('output_format')
        if output_format:
            output = get_output(output_format)
        else:
            output = 'Result saved to database'
        
        return output
        
    @app.route("/export")
    def export():
        data = json.loads(request.get_json())
        return get_output(data['output_format'])

def select_points(table_name:str, point_selection: str, data: dict) -> str:
    if point_selection == 'selected_points':
        points = data['points']  # list of points
        # todo: need to fix the input
        # create_points_table(points)
    elif point_selection == 'region':
        # get points in a preset region
        region = data['region']
        # todo: need to add this function
        # create_points_table_region()
    elif point_selection == 'self_defined_region':
        lng_start = data.get('lng_start')
        lng_end = data.get('lng_end')
        lng_step = data.get('lng_step')
        lat_start = data.get('lat_start')
        lat_end = data.get('lat_end')
        lat_step = data.get('lat_step')

        create_ponits_table_self_defined_area(lng_start, lng_end, lng_step,
                            lat_start, lat_end, lat_step, table_name)
    else:
        abort(400, description="Unsupported point selection method")
    return table_name

def create_boundary(points_table_name: str, boundary: str, data:dict):
    if boundary == 'buffer':
        boundary_radius = float(data['boundary_radius'])
        create_buffer_around_points(points_table_name, boundary_radius)
    else:
        abort(400, description="Unsupported buffer method")

def exposure_calculation(table_name_helper:TableNameHelper, algorithm: str) -> pd.DataFrame:
    if algorithm == 'intersect':
        return Intersect(table_name_helper).intersect()
    else:
        abort(400, description="Unsupported calculation algorithm method")

def get_output(output_format: str):
    if output_format == 'csv':
        pass
    elif output_format == 'json':
        pass
    else:
        abort(400, description="Unsupported output format")