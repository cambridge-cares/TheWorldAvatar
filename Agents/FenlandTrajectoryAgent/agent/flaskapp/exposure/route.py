from flask import Blueprint, request, jsonify
from agent.exposure_calculator.exposure_utilities import ExposureUtils
import logging
logger = logging.getLogger("exposure")
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

            normalized_columns = [col.lower().replace(" ", "_") for col in trajectory_df.columns]
            has_trip_index = "trip_index" in normalized_columns

            if has_trip_index:
                trip_index_col = [col for col in trajectory_df.columns if col.lower().replace(" ", "_") == "trip_index"][0]
                trip_df = trajectory_df[trajectory_df[trip_index_col].fillna(0).astype(int) > 0]
                grouped = trip_df.groupby(trip_index_col)
                logger.info(f"[TRIP FILTER] Found Trip index column: '{trip_index_col}', total trips: {grouped.ngroups}")

                if grouped.ngroups == 0:
                    logger.info(f"[TRIP FILTER] No trips detected for {trajectoryIRI}, fallback to full trajectory.")
                    calc_res = exposure_util.exposure_calculation(
                        trajectory_df=trajectory_df,
                        env_df=env_df,
                        env_data_iri=env_data_iri,
                        domain_label=domain_label,
                        feature_type=feature_type,
                        exposure_radius=exposure_radius,
                        trajectory_iri=trajectoryIRI,
                    )
                    calc_res["trajectoryIRI"] = trajectoryIRI
                    final_results.append(calc_res)
                else:
                    logger.info(f"[TRIP FILTER] Detected {grouped.ngroups} trips for {trajectoryIRI}, computing exposure per trip.")
                    combined_result = {
                        "envFeatureName": domain_label,
                        "type": feature_type,
                        "count": 0,
                        "trajectoryIRI": trajectoryIRI
                    }
                    if feature_type.upper() == "AREA":
                        combined_result["totalArea"] = 0.0

                    for trip_index, sub_df in grouped:
                        trip_res = exposure_util.exposure_calculation(
                            trajectory_df=sub_df,
                            env_df=env_df,
                            env_data_iri=env_data_iri,
                            domain_label=domain_label,
                            feature_type=feature_type,
                            exposure_radius=exposure_radius,
                            trajectory_iri=trajectoryIRI,
                        )
                        combined_result["count"] += trip_res.get("count", 0)
                        if feature_type.upper() == "AREA" and "totalArea" in trip_res:
                            combined_result["totalArea"] += trip_res.get("totalArea", 0.0)

                    final_results.append(combined_result)

            else:
                logger.info(f"[TRIP FILTER] No Trip index column found for {trajectoryIRI}, computing exposure on full trajectory.")
                calc_res = exposure_util.exposure_calculation(
                    trajectory_df=trajectory_df,
                    env_df=env_df,
                    env_data_iri=env_data_iri,
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

@exposure_bp.route('/trip_detection', methods=['POST'])
def trip_detection():
    """
    Endpoint for trip detection on a trajectory.
    The endpoint will:
    1. Detect trips in the trajectory
    2. Add necessary columns to the trajectory table
    3. Update the table with detection results
    """
    data = request.json
    try:
        result = exposure_util.detect_trips_util(data.get("trajectoryIRI"))
        return jsonify(result)
    except ValueError as e:
        return jsonify({"error": str(e)}), 400
    except Exception as e:
        return jsonify({"error": str(e)}), 500