from flask import Blueprint, request
from agent.utils.ts_client import TimeSeriesClient
from twa import agentlogging
import pandas as pd
from pandas._libs.tslibs.np_datetime import OutOfBoundsDatetime
from agent.trip.utilities import wgs_to_utm_code
from agent.trip.trip_detection import detect_trips
from py4j.java_gateway import JavaObject

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

    with time_series_client.connect() as conn:
        time_series = time_series_client.get_time_series(
            data_iri_list=[iri], lowerbound=lowerbound, upperbound=upperbound, conn=conn)

    if (time_series.getTimes().isEmpty()):
        message = 'Time series data is empty'
        logger.error(message)
        return message

    dataframe, utm_code = convert_time_series_to_dataframe(time_series, iri)

    columns = {
        "utc_date": "utc_date",
        "lat": "lat",
        "lon": "lon",
        "utm_n": "utm_n",
        "utm_e": "utm_e"
    }

    detected_gps, incidents_table, _, _, _ = detect_trips(
        dataframe,
        iri,
        columns,
        interpolate_helper_func=None,
        code=utm_code
    )

    return iri


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

    return pd.DataFrame({'utc_date': timestamps, 'lat': lat, 'lon': lon, 'original_time_list': original_time_list}), utm_code
