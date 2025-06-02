from flask import Blueprint, request
from agent.utils.ts_client import TimeSeriesClient
from twa import agentlogging
import pandas as pd
from pandas._libs.tslibs.np_datetime import OutOfBoundsDatetime
from agent.trip.utilities import wgs_to_utm_code
from agent.trip.trip_detection import detect_trips, CH_TRIP_INDEX, CH_VISIT_INDEX
from py4j.java_gateway import JavaObject
from agent.trip.kg_client import KgClient
from agent.utils.baselib_gateway import baselib_view, jpsBaseLibGW

logger = agentlogging.get_logger('dev')

ROUTE = "/process_trajectory"

process_trajectory_bp = Blueprint('process_trajectory_bp', __name__)


@process_trajectory_bp.route(ROUTE, methods=['POST'])
def api():
    logger.info('Received request to process trajectory')
    iri = request.args['iri']
    upperbound = request.args.get('upperbound')
    lowerbound = request.args.get('lowerbound')

    time_series_client = TimeSeriesClient(iri)
    kg_client = KgClient()

    # convert upperbound and lowerbound into the correct types from string
    if upperbound is not None:
        upperbound = convert_input_time_for_timeseries(
            upperbound, kg_client, iri)

    if lowerbound is not None:
        lowerbound = convert_input_time_for_timeseries(
            lowerbound, kg_client, iri)

    logger.info('Querying time series data')
    time_series_trajectory = time_series_client.get_time_series(
        data_iri_list=[iri], lowerbound=lowerbound, upperbound=upperbound)

    if (time_series_trajectory.getTimes().isEmpty()):
        message = 'Time series data is empty'
        logger.error(message)
        return message

    dataframe, utm_code = convert_time_series_to_dataframe(
        time_series_trajectory, iri)

    columns = {
        "utc_date": "utc_date",
        "lat": "lat",
        "lon": "lon",
        "utm_n": "utm_n",
        "utm_e": "utm_e"
    }

    logger.info('Running trip detection code')

    try:
        detected_gps, _, _, _, _ = detect_trips(
            dataframe,
            iri,
            columns,
            interpolate_helper_func=None,
            code=utm_code
        )
    except Exception as ex:
        err_msg = 'Failed to run trip processing code: ' + str(ex)
        logger.error(err_msg)
        raise

    trip, visit = kg_client.get_trip_and_visit(iri)

    if trip is None:
        logger.info('Trip does not exist, instantiating')
        trip, visit = kg_client.instantiate_trip_and_visit()
        time_series_iri = kg_client.get_time_series_iri(iri)
        time_series_client.add_columns(time_series_iri=time_series_iri, data_iri=[
            trip, visit], class_list=[baselib_view.java.lang.Integer.TYPE] * 2)

    # py4j requires python native int, pandas array won't work
    trip_list_int = [int(x) for x in detected_gps[CH_TRIP_INDEX]]
    visit_list_int = [int(x) for x in detected_gps[CH_VISIT_INDEX]]

    # create time series object for trip and visit data for upload to time series database
    time_series_trip_visit = time_series_client.create_time_series(times=time_series_trajectory.getTimes(
    ), data_iri_list=[trip, visit], values=[trip_list_int, visit_list_int])

    # upload to database
    time_series_client.add_time_series(time_series=time_series_trip_visit)

    return 'Added trip and visit data'


def convert_time_series_to_dataframe(time_series, point_iri: str):
    original_time_list = time_series.getTimes()

    # convert timestamps from TWA time series into pandas timestamps
    if isinstance(original_time_list[0], JavaObject):
        # assume something like Instant
        time_string_list = [time.toString() for time in original_time_list]
        timestamps = pd.to_datetime(time_string_list)
    else:
        # probably epoch
        try:
            # OutOfBoundsDatetime exception might be thrown here
            pd.to_datetime(original_time_list[0], unit='s')
            timestamps = pd.to_datetime(original_time_list, unit='s')
        except OutOfBoundsDatetime:
            # assume milliseconds
            timestamps = pd.to_datetime(original_time_list, unit='ms')

    postgis_point_list = time_series.getValuesAsPoint(point_iri)

    lat, lon = zip(*[(p.getY(), p.getX()) for p in postgis_point_list])

    utm_code = wgs_to_utm_code(lat[0], lon[0])

    return pd.DataFrame({'utc_date': timestamps, 'lat': lat, 'lon': lon}), utm_code


def convert_input_time_for_timeseries(time, kg_client: KgClient, point_iri: str):
    # assumes time is in seconds or milliseconds, if an exception is thrown,
    # queries the time class from KG (e.g. java.time.Instant) and use the
    # parse method to parse time into the correct Java object
    try:
        # assume epoch seconds
        return int(time)
    except (ValueError, TypeError):
        # lots of trial and error done to get Java reflection to work correctly!
        class_name = kg_client.get_java_time_class(point_iri)
        time_clazz = baselib_view.java.lang.Class.forName(class_name)

        char_class = baselib_view.java.lang.Class.forName(
            "java.lang.CharSequence")
        param_types = jpsBaseLibGW.gateway.new_array(
            baselib_view.java.lang.Class, 1)
        param_types[0] = char_class

        java_string = baselib_view.java.lang.String(time)
        object_class = baselib_view.java.lang.Object
        args_array = jpsBaseLibGW.gateway.new_array(object_class, 1)
        args_array[0] = java_string

        return time_clazz.getMethod("parse", param_types).invoke(None, args_array)
