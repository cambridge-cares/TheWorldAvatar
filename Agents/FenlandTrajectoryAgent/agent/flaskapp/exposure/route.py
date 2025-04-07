from flask import Blueprint, request, jsonify
from agent.exposure_calculator.exposure_utilities import ExposureUtils

exposure_bp = Blueprint('exposure_bp', __name__)
exposure_util = ExposureUtils()

@exposure_bp.route('/exposure', methods=['POST'])
def calculate_exposure():
    data = request.json
    trajectoryIRIs = data.get("trajectoryIRIs", [])
    exposure_radius = data.get("exposure_radius", 100)
    DataIRIs = data.get("DataIRIs", [])

    final_results = []
    for trajectoryIRI in trajectoryIRIs:
        try:
            trajectory_df = exposure_util.fetch_trajectory_data_from_db(trajectoryIRI)
        except Exception as e:
            return jsonify({"error": str(e)}), 500
        reference_time = None
        if not trajectory_df.empty:
            reference_time = trajectory_df.iloc[0]['trajectory_start_time']

        for env_data_iri in DataIRIs:
            try:
                env_df, domain_label = exposure_util.fetch_env_data(env_data_iri, reference_time=reference_time)
                _, feature_type = exposure_util.get_domain_and_featuretype(env_data_iri)
            except Exception as e:
                return jsonify({"error": str(e)}), 500

            calc_res = exposure_util.exposure_calculation(
                trajectory_df=trajectory_df,
                env_df=env_df,
                domain_label=domain_label,
                feature_type=feature_type,
                exposure_radius=exposure_radius,
                trajectory_iri=trajectoryIRI,
            )
            calc_res["trajectoryIRI"] = trajectoryIRI
            final_results.append(calc_res)

    return jsonify(final_results)

@exposure_bp.route('/exposure/simplified', methods=['POST'])
def calculate_exposure_simplified():
    """
    workflow is packaged in ExposureUtils.calculate_exposure_simplified_util() 
    """
    data = request.json
    try:
        results = exposure_util.calculate_exposure_simplified_util(data)
    except Exception as e:
        return jsonify({"error": str(e)}), 500
    return jsonify(results)

