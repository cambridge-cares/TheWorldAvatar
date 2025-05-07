from flask import jsonify, request, send_file

from agent.boundary import create_boundary
from agent.config.params import OutputFormatParam
from agent.exposure import exposure_calculation
from agent.output.output import get_output
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
        select_points(table_name_helper.get_table_name(),
                      point_selection, data)

        # step2: create boundary around points
        create_boundary(table_name_helper, boundary, data)

        # step3: exposure calculation
        exposure_calculation(table_name_helper, algorithm)

        # step4: (Optional) output result
        save_to_table(table_name_helper)
        output_format = data.get('output_format')
        output = get_output(output_format, point_selection, table_name_helper)

        if output_format == OutputFormatParam.CSV.value:
            return send_file(output,         
                             mimetype='application/zip',
                             as_attachment=True,
                             download_name='exposure.zip')

        return output
