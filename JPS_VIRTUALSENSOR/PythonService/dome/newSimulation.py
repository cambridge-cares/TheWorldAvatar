from flask import Blueprint, request, jsonify
import agentlogging
import os
import requests

ROUTE = "/newSimulation"

get_simulation_geojson_bp = Blueprint('get_simulation_geojson_bp', __name__)
logger = agentlogging.get_logger("dev")

# PARAMETER


SRID = '4326'
nx = '200'
ny = '200'
STACK_NAME = os.getenv('STACK_NAME')
HTTP_DI = f"http://{STACK_NAME}-dispersion-interactor:8080/DispersionInteractor/"
HTTP_SHIP = f"http://{STACK_NAME}-ship-input-agent:8080/ShipInputAgent/"


@get_simulation_geojson_bp.route(ROUTE, methods=['GET'])
def api():
    logger.info("Received request to initialise simulation")
    lon = float(request.args["LON"])
    lat = float(request.args["LAT"])
    dlon = float(request.args["dLON"])
    dlat = float(request.args["dLAT"])
    label = request.args["label"]

    polygon = getpolygon(lon, lat, dlon, dlat)

    # initialise simulation if it doesn't exist. currently check by label
    derivation_iri = initialise_simulation(polygon, label)
    logger.info(f'Initialised derivation with IRI: {derivation_iri}')
    # load ship data from relational database
    load_ship = callhttp(HTTP_SHIP, f'load-rdb?derivation={derivation_iri}', 'post')
    logger.info(f'Number of ships loaded: {load_ship.json()["numNewShips"]}')
    return jsonify({'numShip': load_ship.json()["numNewShips"], 'derivation_iri': derivation_iri}), 200


def getpolygon(lon, lat, dlon, dlat):
    lon_min = lon-dlon
    lon_max = lon+dlon
    lat_min = lat-dlat
    lat_max = lat+dlat
    return (f'POLYGON(({lon_min} {lat_min},{lon_max} {lat_min},{lon_max} {lat_max},{lon_min} {lat_max},{lon_min} {lat_min}))')


def callhttp(http_endpoint, custom_request, method):
    response = getattr(requests, method)(http_endpoint+custom_request)
    if response.status_code != 200:
        logger.info(f'Call to {http_endpoint+custom_request} returns with the following response:')
        logger.warn(response.content)
    return response


def calldi(custom_request, method):
    return callhttp(HTTP_DI, custom_request, method)


def getmetadata():
    return calldi('GetDispersionSimulations', 'get').json()


def initialise_simulation(polygon, label):
    metadata = getmetadata()
    label = label.replace(' ', '%20')
    try:
        derivation_iri = metadata[label]['derivationIri']
        logger.info("Found derivation by label.")
    except Exception as e:
        logger.info(f'Cannot find existing derivation: {e}, try to create it.')
        url_init = f'InitialiseSimulation?ewkt=SRID={SRID};{polygon}&nx={nx}&ny={ny}&label={label}'
        response = calldi(url_init, 'post')
        derivation_iri = response.json()['derivation']
    return derivation_iri
