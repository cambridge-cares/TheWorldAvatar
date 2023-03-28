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
from .testutils import init_kg_client


@pytest.mark.parametrize("expected_response", [C.DEFAULT_RESPONSE])
def test_default(expected_response, flaskapp):
    """
    Tests the GET request for default route
    """
    # Assert client is operational at route
    assert flaskapp.get("/").status_code == 200

    # Perform GET request
    response = flaskapp.get("/")
    assert response.data == bytes(expected_response, 'utf-8')


def assert_root_tile_compulsory_fields(tile: dict):
    expected_fields = {
        "geometricError": 512,
        "refine": "ADD"
    }
    assert expected_fields.items() <= tile.items()


@pytest.mark.parametrize(
    "init_assets, expected_assets, expected_response",
    [(["building", "wall"], ["building"], C.SUCCESSFUL_API_RESPONSE)]
)
def test_api_simple(init_assets, expected_assets, expected_response, kg_client, flaskapp, gen_sample_ifc_file,
                    sample_properties, tileset_content, assert_asset_geometries):
    """
    Tests the POST request for the api route on a simple IFC model
    """
    # Inputs
    route = "/api"
    tileset = os.path.join("data", "tileset_bim.json")

    init_kg_client(kg_client, init_assets)

    # Generate sample ifc file
    ifcpath = gen_sample_ifc_file("./data/ifc/sample.ifc")

    # Generate sample properties.config
    properties_path = sample_properties

    # Perform POST request
    response = flaskapp.post(route, json={"assetUrl": "./gltf"})

    try:
        # Assert that request has successfully occurred
        assert response.status_code == 200
        assert response.json["result"] == expected_response

        # Assert that the tileset and geometry files are generated
        assert os.path.isfile(tileset)
        assert_asset_geometries(expected_assets)

        # Assert tileset content contains the assetUrl passed and gltf files
        content = tileset_content(tileset)
        assert "root" in content

        root = content["root"]
        assert_root_tile_compulsory_fields(root)
        assert root["content"]["uri"] == "./gltf/building.gltf"
        assert "children" not in root
    finally:
        os.remove(ifcpath)
        os.remove(properties_path)
        os.remove(tileset)


@pytest.mark.parametrize(
    "init_assets, expected_assets, expected_response",
    [(
        ["building", "water_meter", "fridge", "chair", "table", "solar_panel"],
        ["building", "asset1", "asset2", "furniture", "solarpanel"],
        C.SUCCESSFUL_API_RESPONSE
    )]
)
def test_api_complex(init_assets, expected_assets, expected_response, kg_client, flaskapp, gen_sample_ifc_file,
                     sample_properties, tileset_content, assert_asset_geometries):
    """
    Tests the POST request for the api route on a complex IFC model
    """
    # Inputs
    route = "/api"
    tileset = os.path.join("data", "tileset_bim.json")
    tileset_solar = os.path.join("data", "tileset_solarpanel.json")

    # Generate the test IFC triples
    init_kg_client(kg_client, init_assets)

    # Generate sample ifc file
    ifcpath = gen_sample_ifc_file("./data/ifc/sample.ifc", assets=init_assets)

    # Generate sample properties.config
    properties_path = sample_properties

    # Perform POST request
    response = flaskapp.post(route, json={"assetUrl": "./gltf"})

    try:
        # Assert that request has successfully occurred
        assert response.status_code == 200
        assert response.json["result"] == expected_response

        # Assert that the tilesets and geometry files are generated
        assert os.path.isfile(tileset)
        assert os.path.isfile(tileset_solar)
        assert_asset_geometries(expected_assets)

        # Assert bim tileset content contains the assetUrl passed and gltf files
        content = tileset_content(tileset)
        assert "root" in content

        root = content["root"]
        assert_root_tile_compulsory_fields(root)
        assert root["contents"][0]["uri"] == "./gltf/furniture.gltf"
        assert root["contents"][1]["uri"] == "./gltf/building.gltf"
        assert root["children"][0]["contents"][0]["uri"] == "./gltf/asset1.gltf"

        # Assert solar tileset content contains the assetUrl passed and gltf files
        solar_content = tileset_content(tileset_solar)
        assert "root" in solar_content

        solar_root = solar_content["root"]
        assert_root_tile_compulsory_fields(solar_root)
        assert solar_root["content"]["uri"] == "./gltf/solarpanel.gltf"
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


@pytest.mark.parametrize("expected_response", [C.INVALID_PARAM_API_RESPONSE])
def test_api_invalid_request(expected_response, flaskapp):
    """
    Tests that invalid requests returns the Bad request status code
    """
    # Inputs
    route = "/api"

    # Send the POST request
    response = flaskapp.post(route, json={"asset url": "./gltf"})

    # Assert that request has failed with the right status and response
    assert response.status_code == 400
    assert response.json["data"] == expected_response


@pytest.mark.parametrize(
    "asset_url",
    [
        C.invalid_asseturl1,
        C.invalid_asseturl2,
        C.invalid_asseturl3,
        C.invalid_asseturl4,
        C.invalid_asseturl5,
        C.invalid_asseturl6,
    ]
)
def test_api_invalid_asserturl_param(asset_url, flaskapp):
    """
    Tests that invalid assetUrl params returns the Bad request status code
    """
    # Inputs
    route = "/api"
    expected_response = "`assetUrl` parameter <" + asset_url
    expected_response += "> is invalid. It must start with `.`, `..`, or `http://`, and must not end with `/`"

    # Send the POST request
    response = flaskapp.post(route, json={"assetUrl": asset_url})

    # Assert that request has failed with the right status and response
    assert response.status_code == 400
    assert response.json["data"] == expected_response
