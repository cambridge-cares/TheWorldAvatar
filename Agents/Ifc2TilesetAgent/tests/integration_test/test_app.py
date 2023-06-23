"""
# Author: qhouyee, picas9dan #

An integration test suite for the entire app.
"""

# Standard library imports
import os

# Third-party imports
import pytest
import numpy as np

# Self imports
from . import testconsts as C
from .testutils import init_kg_client, read_json_file, assert_assets_present


def test_default(flaskapp):
    # Arrange
    expected = (
        "<!DOCTYPE html>\n"
        "<html>\n"
        "<body>\n"
        "    <div>\n"
        "        The Ifc2Tileset agent offers the following functionality at the specified API endpoint:\n"
        "        <br><br>\n"
        "        (POST) request to convert IFC models to Cesium's 3D tilesets:\n"
        "        <br>\n"
        "        &nbsp&nbsp [this_url]/api\n"
        "        <br>\n"
        "        &nbsp&nbsp [this_url] is the host and port currently shown in the address bar\n"
        "    </div>\n"
        "</body>\n"
        "</html>"
    )

    # Act
    response = flaskapp.get("/")

    # Assert
    assert response.status_code == 200
    assert response.data == bytes(expected, "utf-8")


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
def test_api_simple(init_assets, expected_assets, expected_bim_bbox, kg_client, flaskapp, gen_sample_ifc_file):
    """Tests the POST request on an IFC model without assets."""
    # Arrange
    route = "/api"
    init_kg_client(kg_client, init_assets)
    gen_sample_ifc_file("./data/ifc/sample.ifc", assets=init_assets)

    # Act
    response = flaskapp.post(route, json={"assetUrl": "./glb"})

    # Assert
    assert response.status_code == 200
    assert response.json["result"] == C.SUCCESSFUL_API_RESPONSE

    # Assert that the tileset and geometry files are generated
    tileset_bim_file = os.path.join("data", "tileset_bim.json")
    assert os.path.isfile(tileset_bim_file)
    assert_assets_present(expected_assets)

    # Assert tileset content contains the assetUrl passed and geometry files
    tileset_content = read_json_file(tileset_bim_file)
    assert "root" in tileset_content

    root = tileset_content["root"]
    assert_root_tile_compulsory_fields(root)
    assert np.allclose(root["boundingVolume"]["box"], expected_bim_bbox)
    assert root["content"] == {"uri": "./glb/building.glb"}
    assert "children" not in root


@pytest.mark.parametrize(
    "init_assets, expected_assets, expected_root_kvs, expected_child_contents, expected_bim_bbox, "
    "expected_asset_bbox, expected_solar_panel_bbox",
    [(
        ["building", "wall", "water_meter", "solar_panel"],
        ["building", "asset1", "solarpanel"],
        dict(content={"uri": "./glb/building.glb"}),
        [dict(uri="./glb/asset1.glb",
              metadata={"class": "AssetMetaData",
                        "properties": {"name": C.sample_water_meter.label, "uid": C.sample_water_meter.ifc_id,
                                       "iri": C.base_namespace + C.sample_water_meter.iri}})],
        [2.5, 0.1, 1.5, 2.5, 0, 0, 0, 0.1, 0, 0, 0, 1.5],
        [0.5, 2.5, 0.5, 5.5, 0, 0, 0, 5.5, 0, 0, 0, 0.5],
        [1.5, 1.5, 6.25, 1.5, 0, 0, 0, 1.5, 0, 0, 0, 0.25]
    ), (
        ["building", "wall", "water_meter", "fridge", "chair", "table", "solar_panel"],
        ["building", "asset1", "asset2", "furniture", "solarpanel"],
        dict(contents=[{"uri": "./glb/furniture.glb"}, {"uri": "./glb/building.glb"}]),
        [dict(uri=f"./glb/asset{i + 1}.glb",
              metadata={"class": "AssetMetaData",
                        "properties": {"name": e.label, "uid": e.ifc_id, "iri": C.base_namespace + e.iri}})
         for i, e in enumerate((C.sample_water_meter, C.sample_fridge))],
        [2.5, 1, 1.75, 2.5, 0, 0, 0, 1, 0, 0, 0, 1.75],
        [2.5, 1.5, 2.5, 7.5, 0, 0, 0, 6.5, 0, 0, 0, 2.5],
        [1.5, 1.5, 6.25, 1.5, 0, 0, 0, 1.5, 0, 0, 0, 0.25]
    )]
)
def test_api_complex(init_assets, expected_assets, expected_root_kvs, expected_child_contents, expected_bim_bbox,
                     expected_asset_bbox, expected_solar_panel_bbox, kg_client, flaskapp, gen_sample_ifc_file):
    """Tests the POST request on an IFC model with assets and solar panel."""
    # Arrange
    route = "/api"
    init_kg_client(kg_client, init_assets)
    gen_sample_ifc_file("./data/ifc/sample.ifc", assets=init_assets)

    # Act
    response = flaskapp.post(route, json={"assetUrl": "./glb"})

    # Assert
    assert response.status_code == 200
    assert response.json["result"] == C.SUCCESSFUL_API_RESPONSE

    # Assert that the tilesets and geometry files are generated
    tileset_bim_file = os.path.join("data", "tileset_bim.json")
    tileset_solar_file = os.path.join("data", "tileset_solarpanel.json")
    assert os.path.isfile(tileset_bim_file)
    assert os.path.isfile(tileset_solar_file)
    assert_assets_present(expected_assets)

    # Assert bim tileset content contains the assetUrl passed and geometry files
    content = read_json_file(tileset_bim_file)
    assert "root" in content

    bim_root = content["root"]
    assert_root_tile_compulsory_fields(bim_root)
    assert np.allclose(bim_root["boundingVolume"]["box"], expected_bim_bbox)
    assert expected_root_kvs.items() <= bim_root.items()
    assert "children" in bim_root and isinstance(bim_root["children"], list) and len(bim_root["children"]) == 1

    child_tile = bim_root["children"][0]
    assert_child_tile_compulsory_fields(child_tile)
    assert np.allclose(child_tile["boundingVolume"]["box"], expected_asset_bbox)
    assert child_tile["contents"] == expected_child_contents

    # Assert solar tileset content contains the assetUrl passed and geometry files
    solar_content = read_json_file(tileset_solar_file)
    assert "root" in solar_content

    solar_root = solar_content["root"]
    assert_root_tile_compulsory_fields(solar_root)
    assert np.allclose(solar_root["boundingVolume"]["box"], expected_solar_panel_bbox)
    assert solar_root["content"] == {"uri": "./glb/solarpanel.glb"}


def test_api_no_building_structure_no_assets(kg_client, gen_sample_ifc_file, flaskapp):
    # Arrange
    route = "/api"
    init_kg_client(kg_client, ["building"])
    gen_sample_ifc_file("./data/ifc/sample.ifc")

    # Act
    response = flaskapp.post(route, json={"assetUrl": "./glb"})

    # Assert
    assert response.status_code == 200
    assert response.json["result"] == C.SUCCESSFUL_API_RESPONSE

    # Assert that no bim tileset is generated
    tileset_bim_file = os.path.join("data", "tileset_bim.json")
    assert not os.path.isfile(tileset_bim_file)


def test_api_no_building_structure_with_assets(kg_client, gen_sample_ifc_file, flaskapp):
    # Arrange
    route = "/api"
    init_assets = ["building", "water_meter"]
    init_kg_client(kg_client, init_assets)
    gen_sample_ifc_file("./data/ifc/sample.ifc", assets=init_assets)

    expected_assets = ["asset1"]
    expected_bim_bbox = [0.5, 2.5, 0.5, 0.5, 0, 0, 0, 0.5, 0, 0, 0, 0.5]
    expected_asset_bbox = [0.5, 2.5, 0.5, 5.5, 0, 0, 0, 5.5, 0, 0, 0, 0.5]
    expected_child_contents = [
        dict(uri="./glb/asset1.glb",
             metadata={"class": "AssetMetaData",
                       "properties": {"name": C.sample_water_meter.label, "uid": C.sample_water_meter.ifc_id,
                                      "iri": C.base_namespace + C.sample_water_meter.iri}})
    ]

    # Act
    response = flaskapp.post(route, json={"assetUrl": "./glb"})

    # Assert
    assert response.status_code == 200
    assert response.json["result"] == C.SUCCESSFUL_API_RESPONSE

    # Assert that the tilesets and geometry files are generated
    tileset_bim_file = os.path.join("data", "tileset_bim.json")
    assert os.path.isfile(tileset_bim_file)
    assert_assets_present(expected_assets)

    # Assert bim tileset content contains the assetUrl passed and geometry files
    content = read_json_file(tileset_bim_file)
    assert "root" in content

    bim_root = content["root"]
    assert_root_tile_compulsory_fields(bim_root)
    assert np.allclose(bim_root["boundingVolume"]["box"], expected_bim_bbox)
    assert "children" in bim_root and isinstance(bim_root["children"], list) and len(bim_root["children"]) == 1

    child_tile = bim_root["children"][0]
    assert_child_tile_compulsory_fields(child_tile)
    assert np.allclose(child_tile["boundingVolume"]["box"], expected_asset_bbox)
    assert child_tile["contents"] == expected_child_contents


def test_api_wrong_request_type(flaskapp):
    """Tests that the wrong request returns the Method not allowed status code."""
    # Arrange
    route = "/api"

    # Act & Assert
    assert flaskapp.get(route).status_code == 405


def test_api_invalid_request(flaskapp):
    """Tests that invalid requests returns the Bad request status code."""
    # Arrange
    route = "/api"
    expected_response = {"data": "Missing `assetUrl` parameter in request!"}

    # Act
    response = flaskapp.post(route, json={"asset url": "./glb"})

    # Assert
    assert response.status_code == 400
    assert response.json == expected_response


@pytest.mark.parametrize(
    "asset_url",
    ["./", "dir", "/dir/", "../../", "www.example.org", "http://www.example.com/ns/"]
)
def test_api_invalid_request_param(asset_url, flaskapp):
    """Tests that invalid assetUrl params returns the Bad request status code."""
    # Arrange
    route = "/api"
    expected_response = {"data": f"`assetUrl` parameter <{asset_url}> is invalid. "
                                 f"It must start with `.`, `..`, or `http://`, and must not end with `/`"}

    # Act
    response = flaskapp.post(route, json={"assetUrl": asset_url})

    # Assert
    assert response.status_code == 400
    assert response.json == expected_response


def test_api_no_ifc(flaskapp):
    # Arrange
    route = "/api"
    expected_response = {"data": "No ifc file is available at the ./data/ifc folder"}

    # Act
    response = flaskapp.post(route, json={"assetUrl": "./glb"})

    # Assert
    assert response.status_code == 400
    assert response.json == expected_response


def test_api_multi_ifc(flaskapp):
    # Arrange
    route = "/api"

    # Create multiple ifc files
    ifc_files = [os.path.join("data", "ifc", f"test{i}.ifc") for i in range(2)]
    for file in ifc_files:
        open(file, "x", encoding="utf-8").close()

    expected_response = {
        "data": "More than one IFC file is located at the ./data/ifc folder. Please place only ONE IFC file."
    }

    # Act
    response = flaskapp.post(route, json={"assetUrl": "./glb"})

    # Assert
    assert response.status_code == 400
    assert response.json == expected_response


def test_api_invalid_ifc(flaskapp):
    # Arrange
    route = "/api"

    # Create an empty ifc file, which is invalid
    ifc_file = os.path.join("data", "ifc", "test.ifc")
    open(ifc_file, "x", encoding="utf-8").close()

    expected_response = {"data": "IFC model validation fails. Cause: Unable to parse IFC SPF header"}

    # Act
    response = flaskapp.post(route, json={"assetUrl": "./glb"})

    # Assert
    assert response.status_code == 400
    assert response.json == expected_response
