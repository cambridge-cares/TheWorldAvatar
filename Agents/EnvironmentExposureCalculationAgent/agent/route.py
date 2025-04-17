from flask import jsonify, request

from agent.boundary import create_boundary
from agent.exposure import exposure_calculation
from agent.output import get_output
from agent.output.save_to_table import save_to_table
from agent.point_selection import select_points
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
            output = get_output(output_format, res, table_name_helper)
        else:
            output = 'Result saved to database'
        
        return output
        