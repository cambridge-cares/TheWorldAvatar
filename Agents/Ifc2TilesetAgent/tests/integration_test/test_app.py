"""
# Author: qhouyee #

An integration test suite for the entire app.
"""
# Standard import
import os

# Third party import
import pytest

# Self import
from . import testconsts as C


def test_default(flaskapp):
    """
    Tests the GET request for default route
    """
    # Assert client is operational at route
    assert flaskapp.get("/").status_code == 200
    # Perform GET request
    response = flaskapp.get("/")
    assert response.data == bytes(C.DEFAULT_RESPONSE, 'utf-8')


def test_api_simple(initialise_client, flaskapp, gen_sample_ifc_file, sample_properties,
                    tileset_content, assert_asset_geometries):
    """
    Tests the POST request for the api route on a simple IFC model
    """
    # Inputs
    route = "/api"
    tileset = os.path.join("data", "tileset_bim.json")
    # Generate the test IFC triples
    kg_client = initialise_client
    kg_client.execute_update(C.insert_wall_query)
    kg_client.execute_update(C.insert_building_query)
    # Generate sample ifc file
    ifcpath = gen_sample_ifc_file("./data/ifc/wall.ifc", False)
    # Generate sample properties.config
    properties_path = sample_properties

    # Perform POST request
    response = flaskapp.post(route, json={"assetUrl": "./gltf"})

    try:
        # Assert that request has successfully occurred
        assert response.status_code == 200
        assert response.json["result"] == C.SUCCESSFUL_API_RESPONSE
        # Assert that the tileset and geometry files are generated
        assert os.path.isfile(tileset)
        assert_asset_geometries(C.expected_assets1)
        # Assert tileset content contains the assetUrl passed and gltf files
        content = tileset_content(tileset)
        assert content["root"]["content"]["uri"] == "./gltf/building.gltf"
        assert "children" not in content["root"]
    finally:
        os.remove(ifcpath)
        os.remove(properties_path)
        os.remove(tileset)


def test_api_complex(initialise_client, flaskapp, gen_sample_ifc_file, sample_properties,
                     tileset_content, assert_asset_geometries):
    """
    Tests the POST request for the api route on a complex IFC model
    """
    # Inputs
    route = "/api"
    tileset = os.path.join("data", "tileset_bim.json")
    tileset_solar = os.path.join("data", "tileset_solarpanel.json")
    # Generate the test IFC triples
    kg_client = initialise_client
    kg_client.execute_update(C.insert_assets_query)
    kg_client.execute_update(C.insert_building_query)
    # Generate sample ifc file
    ifcpath = gen_sample_ifc_file("./data/ifc/sample.ifc", True)
    # Generate sample properties.config
    properties_path = sample_properties
    # Perform POST request
    response = flaskapp.post(route, json={"assetUrl": "./gltf"})
    try:
        # Assert that request has successfully occurred
        assert response.status_code == 200
        assert response.json["result"] == C.SUCCESSFUL_API_RESPONSE
        # Assert that the tilesets and geometry files are generated
        assert os.path.isfile(tileset)
        assert os.path.isfile(tileset_solar)
        assert_asset_geometries(C.expected_assets2)
        # Assert tileset content contains the assetUrl passed and gltf files
        content = tileset_content(tileset)
        assert content["root"]["contents"][0]["uri"] == "./gltf/furniture.gltf"
        assert content["root"]["contents"][1]["uri"] == "./gltf/building.gltf"
        assert content["root"]["children"][0]["contents"][0]["uri"] == "./gltf/asset1.gltf"
        solar_content = tileset_content(tileset_solar)
        assert solar_content["root"]["content"]["uri"] == "./gltf/solarpanel.gltf"
    finally:
        os.remove(ifcpath)
        os.remove(properties_path)
        os.remove(tileset)
        os.remove(tileset_solar)


def test_api_wrong_request_type(flaskapp):
    """
    Tests that the wrong request returns the Method not allowed status code
    """
    # Inputs
    route = "/api"
    # Assert client gets status 405
    assert flaskapp.get(route).status_code == 405


def test_api_invalid_request(flaskapp):
    """
    Tests that invalid requests returns the Bad request status code
    """
    # Inputs
    route = "/api"
    # Send the POST request
    response = flaskapp.post(route, json={"asset url": "./gltf"})
    # Assert that request has failed with the right status and response
    assert response.status_code == 400
    assert response.json["data"] == C.INVALID_PARAM_API_RESPONSE


@pytest.mark.parametrize(
    "asseturl",
    [
        C.invalid_asseturl1,
        C.invalid_asseturl2,
        C.invalid_asseturl3,
        C.invalid_asseturl4,
        C.invalid_asseturl5,
        C.invalid_asseturl6,
    ]
)
def test_api_invalid_asserturl_param(asseturl, flaskapp):
    """
    Tests that invalid assetUrl params returns the Bad request status code
    """
    # Inputs
    route = "/api"
    expected_response = "`assetUrl` parameter <" + asseturl
    expected_response += "> is invalid. It must start with `.`, `..`, or `http://`, and must not end with `/`"
    # Send the POST request
    response = flaskapp.post(route, json={"assetUrl": asseturl})
    # Assert that request has failed with the right status and response
    assert response.status_code == 400
    assert response.json["data"] == expected_response
