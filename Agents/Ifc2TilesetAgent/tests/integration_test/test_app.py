"""
# Author: qhouyee #

An integration test suite for the entire app.
"""
# Standard import
import os

# Third party import
import pytest
import numpy as np

# Self import
from . import testconsts as C
from .testutils import init_kg_client, read_json_file


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
    assert "geometricError" in tile and tile["geometricError"] == 512
    assert "refine" in tile and tile["refine"] == "ADD"
    assert "boundingVolume" in tile and "box" in tile["boundingVolume"] and len(tile["boundingVolume"]["box"]) == 12


def assert_child_tile_compulsory_fields(tile: dict):
    assert "geometricError" in tile and tile["geometricError"] == 50
    assert "boundingVolume" in tile and "box" in tile["boundingVolume"] and len(tile["boundingVolume"]["box"]) == 12


@pytest.mark.parametrize(
    "init_assets, expected_assets, expected_bim_bbox",
    [(["building", "wall"], ["building"], [2.5, 0.1, 1.5, 2.5, 0, 0, 0, 0.1, 0, 0, 0, 1.5])]
)
def test_api_simple(init_assets, expected_assets, expected_bim_bbox, kg_client, flaskapp, gen_sample_ifc_file,
                    sample_properties, assert_asset_geometries):
    """
    Tests the POST request for the api route on a simple IFC model
    """
    # Inputs
    route = "/api"
    tileset_bim_file = os.path.join("data", "tileset_bim.json")

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
        assert response.json["result"] == C.SUCCESSFUL_API_RESPONSE

        # Assert that the tileset and geometry files are generated
        assert os.path.isfile(tileset_bim_file)
        assert_asset_geometries(expected_assets)

        # Assert tileset content contains the assetUrl passed and gltf files
        tileset_content = read_json_file(tileset_bim_file)
        assert "root" in tileset_content

        root = tileset_content["root"]
        assert_root_tile_compulsory_fields(root)
        assert np.allclose(root["boundingVolume"]["box"], expected_bim_bbox)
        assert root["content"] == {"uri": "./gltf/building.gltf"}
        assert "children" not in root
    finally:
        os.remove(ifcpath)
        os.remove(properties_path)
        os.remove(tileset_bim_file)


@pytest.mark.parametrize(
    "init_assets, expected_assets, expected_root_content, expected_child_contents, expected_bim_bbox, "
    "expected_asset_bbox, expected_solar_panel_bbox",
    [(
        ["building", "wall", "water_meter", "solar_panel"],
        ["building", "asset1", "solarpanel"],
        {"uri": "./gltf/building.gltf"},
        [dict(uri="./gltf/asset1.gltf",
              metadata={"class": "AssetMetaData",
                        "properties": {"name": C.sample_water_meter.label, "uid": C.sample_water_meter.ifc_id,
                                       "iri": C.base_namespace + C.sample_water_meter.iri}})],
        [2.5, 0.1, 1.5, 2.5, 0, 0, 0, 0.1, 0, 0, 0, 1.5],
        [0.5, 2.5, 0.5, 0.5, 0, 0, 0, 0.5, 0, 0, 0, 0.5],
        [1.5, 1.5, 6.25, 1.5, 0, 0, 0, 1.5, 0, 0, 0, 0.25]
    ), (
        ["building", "wall", "water_meter", "fridge", "chair", "table", "solar_panel"],
        ["building", "asset1", "asset2", "furniture", "solarpanel"],
        [{"uri": "./gltf/furniture.gltf"}, {"uri": "./gltf/building.gltf"}],
        [dict(uri=f"./gltf/asset{i + 1}.gltf",
              metadata={"class": "AssetMetaData",
                        "properties": {"name": e.label, "uid": e.ifc_id, "iri": C.base_namespace + e.iri}})
         for i, e in enumerate((C.sample_water_meter, C.sample_fridge))],
        [2.5, 1, 1.75, 2.5, 0, 0, 0, 1, 0, 0, 0, 1.75],
        [2.5, 1.5, 2.5, 2.5, 0, 0, 0, 1.5, 0, 0, 0, 2.5],
        [1.5, 1.5, 6.25, 1.5, 0, 0, 0, 1.5, 0, 0, 0, 0.25]
    )]
)
def test_api_complex(init_assets, expected_assets, expected_root_content, expected_child_contents, expected_bim_bbox,
                     expected_asset_bbox, expected_solar_panel_bbox, kg_client, flaskapp, gen_sample_ifc_file,
                     sample_properties, assert_asset_geometries):
    """
    Tests the POST request for the api route on a complex IFC model
    """
    # Inputs
    route = "/api"
    tileset_bim_file = os.path.join("data", "tileset_bim.json")
    tileset_solar_file = os.path.join("data", "tileset_solarpanel.json")

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
        assert response.json["result"] == C.SUCCESSFUL_API_RESPONSE

        # Assert that the tilesets and geometry files are generated
        assert os.path.isfile(tileset_bim_file)
        assert os.path.isfile(tileset_solar_file)
        assert_asset_geometries(expected_assets)

        # Assert bim tileset content contains the assetUrl passed and gltf files
        content = read_json_file(tileset_bim_file)
        assert "root" in content

        bim_root = content["root"]
        assert_root_tile_compulsory_fields(bim_root)
        assert np.allclose(bim_root["boundingVolume"]["box"], expected_bim_bbox)
        if isinstance(expected_root_content, list):
            assert bim_root["contents"] == expected_root_content
        else:
            assert bim_root["content"] == expected_root_content
        assert "children" in bim_root and isinstance(bim_root["children"], list) and len(bim_root["children"]) == 1

        child_tile = bim_root["children"][0]
        assert_child_tile_compulsory_fields(child_tile)
        assert np.allclose(child_tile["boundingVolume"]["box"], expected_asset_bbox)
        assert child_tile["contents"] == expected_child_contents

        # Assert solar tileset content contains the assetUrl passed and gltf files
        solar_content = read_json_file(tileset_solar_file)
        assert "root" in solar_content

        solar_root = solar_content["root"]
        assert_root_tile_compulsory_fields(solar_root)
        assert np.allclose(solar_root["boundingVolume"]["box"], expected_solar_panel_bbox)
        assert solar_root["content"] == {"uri": "./gltf/solarpanel.gltf"}
    finally:
        os.remove(ifcpath)
        os.remove(properties_path)
        os.remove(tileset_bim_file)
        os.remove(tileset_solar_file)


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
