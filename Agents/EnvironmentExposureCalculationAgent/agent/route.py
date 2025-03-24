from datetime import date
import json
from flask import abort, jsonify, request

from agent.boundary import create_buffer_around_points
from agent.exposure.intersect import intersect
from agent.point_selection.point_selection import create_ponits_table_self_defined_area


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
        # step1: point selection
        points_table_name = select_points(point_selection, data)

        # step2: create boundary around points
        create_boundary(points_table_name, boundary, data)

        # step3: exposure calculation
        exposure_calculation(points_table_name, algorithm)

        # step4: (Optional) output result
        output_format = data['output_format']
        if output_format:
            get_output(output_format)
        

    @app.route("/export")
    def export():
        data = json.loads(request.get_json())
        return get_output(data['output_format'])


def select_points(point_selection: str, data: dict) -> str:
    if point_selection == 'selected_points':
        points = data['points']  # list of points
        # todo: need to fix the input
        table_name = 'points_selected_points'
        # create_points_table(points)
    elif point_selection == 'region':
        # get points in a preset region
        region = data['region']
        # todo: need to add this function
        table_name = 'points_' + region
        # create_points_table_region()
    elif point_selection == 'self_defined_region':
        lng_start = data.get('lng_start')
        lng_end = data.get('lng_end')
        lng_step = data.get('lng_step')
        lat_start = data.get('lat_start')
        lat_end = data.get('lat_end')
        lat_step = data.get('lat_step')

        table_name = 'points_self_defined_region'
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


def exposure_calculation(points_table_name: str, algorithm: str):
    if algorithm == 'intersect':
        intersect(points_table_name)
    else:
        abort(400, description="Unsupported calculation algorithm method")

def get_output(output_format: str):
    if output_format == 'csv':
        pass
    elif output_format == 'json':
        pass
    else:
        abort(400, description="Unsupported output format")