from flask import jsonify, request, send_file

from agent.boundary import create_boundary
from agent.config.params import OutputFormatParam
from agent.exposure import exposure_calculation
from agent.output.output import get_output
from agent.output.postprocessing import postprocess_result
from agent.point_selection import select_points
from agent.utils.table_name_helper import QueryIdHelper


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

        query_id = QueryIdHelper(data, request.args).get_query_id()
        # step1: point selection
        select_points(query_id,
                      point_selection, data)

        # step2: create boundary around points
        create_boundary(query_id, boundary, data)

        # step3: exposure calculation
        exposure_calculation(query_id, algorithm)

        # step4: result output
        postprocess_result(query_id)
        output_format = data.get('output_format')
        output = get_output(output_format, point_selection, query_id)

        if output_format == OutputFormatParam.CSV.value:
            return send_file(output,         
                             mimetype='application/zip',
                             as_attachment=True,
                             download_name='exposure.zip')

        return output
