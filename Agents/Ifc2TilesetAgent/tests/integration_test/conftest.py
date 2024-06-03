"""
# Author: qhouyee, picas9dan #

A module that provides all pytest fixtures and utility functions for all integration tests.
"""

# Standard library imports
import os
from typing import Iterable, Tuple

# Third-party imports
import pytest
import ifcopenshell
from ifcopenshell.api import run
import yaml

# Self imports
from agent import create_app
from agent.kgutils import KGClient
from . import testconsts as C

# ----------------------------------------------------------------------------------
# Session-scoped test fixtures
# (i.e. the fixture is destroyed at the end of the test session)
# ----------------------------------------------------------------------------------


@pytest.fixture(scope="session")
def endpoint():
    return C.KG_ENDPOINT


@pytest.fixture(scope="session")
def gen_sample_ifc_file():
    """
    A test function to generate two different sample IFC file based on requirements

    Argument:
    ifc_path - File path to the IFC file
    assets - An iterable of strings denoting assets to include in the ifc file, such as fridge, chair, solar panel etc.
    """

    def _gen_ifc_file(ifc_path: str, assets: Iterable[str] = ()):
        # Create a blank model
        model = ifcopenshell.file()
        # All projects must have one IFC Project element
        project = run("root.create_entity", model,
                      ifc_class="IfcProject", name="My Project")

        # To generate geometry, must assign units, defaults to metric units without args
        run("unit.assign_unit", model)

        # Create a geometry modelling context for storing 3D geometries
        context = run("context.add_context", model, context_type="Model")
        body = run("context.add_context", model, context_type="Model", context_identifier="Body",
                   target_view="MODEL_VIEW", parent=context)

        # Create a site, building, and storey
        site = run("root.create_entity", model,
                   ifc_class="IfcSite", name="My Site")
        building = run("root.create_entity", model,
                       ifc_class="IfcBuilding", name="Building A")
        storey = run("root.create_entity", model,
                     ifc_class="IfcBuildingStorey", name="Ground Floor")

        # Assign their relations
        run("aggregate.assign_object", model,
            relating_object=project, product=site)
        run("aggregate.assign_object", model,
            relating_object=site, product=building)
        run("aggregate.assign_object", model,
            relating_object=building, product=storey)

        ifc_building_elements = []
        ifc_furnishing_elements = []

        ifc_building_element_permissible_values = (
            "water_meter", "fridge", "solar_panel", "sewage_network")
        ifc_furnishing_element_permissible_values = ("chair", "table")

        for asset in assets:
            if asset == "building":
                pass
            elif asset == "wall":
                # Create a wall
                wall = run("root.create_entity", model, ifc_class="IfcWall", GlobalId=C.sample_wall.ifc_id,
                           Name=C.sample_wall.label)
                # Add body geometry in meters
                representation = run("geometry.add_wall_representation", model, context=body, length=5, height=3,
                                     thickness=0.2)
                # Assign body geometry to the wall
                run("geometry.assign_representation", model,
                    product=wall, representation=representation)
                # Place the wall on ground floor
                run("spatial.assign_container", model,
                    relating_structure=storey, product=wall)
            elif asset in ifc_building_element_permissible_values:
                ifc_building_elements.append(asset)
            elif asset in ifc_furnishing_element_permissible_values:
                ifc_furnishing_elements.append(asset)
            else:
                permissible_values = ("building", "wall") + ifc_building_element_permissible_values \
                    + ifc_furnishing_element_permissible_values
                raise ValueError(f"Unexpected argument `{asset}` for an asset value; must be either "
                                 f"{permissible_values}.")

        ifc_building_element_proxy_products = {
            asset: model.create_entity("IfcBuildingElementProxy", GlobalId=C.SAMPLE_ONTOBIM_ELEMENT_STORE[asset].ifc_id,
                                       Name=C.SAMPLE_ONTOBIM_ELEMENT_STORE[asset].label)
            for asset in ifc_building_elements
        }
        ifc_furnishing_element_products = {
            asset: model.create_entity("IfcFurnishingElement", GlobalId=C.SAMPLE_ONTOBIM_ELEMENT_STORE[asset].ifc_id,
                                       Name=C.SAMPLE_ONTOBIM_ELEMENT_STORE[asset].label)
            for asset in ifc_furnishing_elements
        }

        # Assign geometries to each element
        for asset, product in {**ifc_building_element_proxy_products, **ifc_furnishing_element_products}.items():
            _representation = _create_box(
                model, context, C.SAMPLE_ONTOBIM_GEOM_STORE[asset])
            run("geometry.assign_representation", model,
                product=product, representation=_representation)

        # Write out to a file
        model.write(ifc_path)
        return ifc_path

    return _gen_ifc_file


# ----------------------------------------------------------------------------------
# Module-scoped test fixtures
# (i.e. the fixture is destroyed during teardown of the last test in the module)
# ----------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------
# Function-scoped test fixtures
# (i.e. the fixture is destroyed at the end of each test)
# -----
@pytest.fixture(scope="function")
def kg_client():
    """
    Retrieves all the exposed endpoints for dockerised testing services
    """
    # Create KG Client for testing
    kg_client = KGClient(C.KG_ENDPOINT, C.KG_ENDPOINT)
    # Returns client for the test to continue running
    yield kg_client

    # Clean up operations at the end of the test
    clear_triplestore(kg_client)
    clear_loggers()


@pytest.fixture(scope='function')
def flaskapp():
    app = create_app()
    app.config.update({
        "TESTING": True,
    })
    # Create a sample yaml file with required properties for retrieval
    yaml_path = C.SAMPLE_YAML_PATH
    data = dict(
        query_endpoint=C.KG_ENDPOINT,
        update_endpoint=C.KG_ENDPOINT,
        bim_tileset_iri="",
        bim_tileset_name="",
        solar_panel_tileset_iri="",
        solar_panel_tileset_name="",
        sewage_tileset_iri="",
        sewage_tileset_name=""
    )
    with open(yaml_path, 'w') as outfile:
        yaml.dump(data, outfile)
    yield app.test_client()
    # Remove once test is over
    os.remove(yaml_path)


# ----------------------------------------------------------------------------------
# Helper functions
# ----------------------------------------------------------------------------------


def clear_loggers():
    """Remove handlers from all loggers. Adopted from
    https://github.com/pytest-dev/pytest/issues/5502#issuecomment-647157873"""
    import logging
    loggers = [logging.getLogger()] + \
        list(logging.Logger.manager.loggerDict.values())
    for logger in loggers:
        handlers = getattr(logger, 'handlers', [])
        for handler in handlers:
            logger.removeHandler(handler)


def clear_triplestore(kg_client: KGClient):
    """Delete all triples"""
    query_delete = """
        DELETE WHERE {?s ?p ?o}
        """
    kg_client.execute_update(query_delete)


Point = Tuple[float, float, float]


def _create_box(ifcfile: ifcopenshell.file, context, extreme_coordinates: Tuple[Point, Point]):
    unit_scale = ifcopenshell.util.unit.calculate_unit_scale(ifcfile)
    lower, upper = (tuple(x / unit_scale for x in p)
                    for p in extreme_coordinates)

    point1 = lower
    point2 = (upper[0], lower[1], lower[2])
    point3 = (upper[0], upper[1], lower[2])
    point4 = (lower[0], upper[1], lower[2])
    points = [point1, point2, point3, point4]

    ifc_axis2placement3d = _create_ifc_axis2placement3d(ifcfile)
    extrusion_direction = (0., 0., 1.)
    extrusion_length = upper[2] - lower[2]

    extrusion = _create_ifc_extruded_area_solid(ifcfile, points, ifc_axis2placement3d, extrusion_direction,
                                                extrusion_length)
    return ifcfile.createIfcShapeRepresentation(
        context,
        context.ContextIdentifier,
        "SweptSolid",
        [extrusion]
    )


# https://academy.ifcopenshell.org/posts/creating-a-simple-wall-with-property-set-and-quantity-information/
def _create_ifc_axis2placement3d(
        ifcfile: ifcopenshell.file,
        point: Point = (0., 0., 0.),
        direction1: Point = (0., 0., 1.),
        direction2: Point = (1., 0., 0.)
):
    """Creates an IfcAxis2Placement3D from Location, Axis and RefDirection specified as Python tuples"""
    point = ifcfile.createIfcCartesianPoint(point)
    direction1 = ifcfile.createIfcDirection(direction1)
    direction2 = ifcfile.createIfcDirection(direction2)
    axis2placement = ifcfile.createIfcAxis2Placement3D(
        point, direction1, direction2)
    return axis2placement


def _create_ifc_extruded_area_solid(
        ifcfile: ifcopenshell.file,
        points: Iterable[Point],
        ifc_axis2placement3d,
        extrusion_direction: Point,
        extrusion_length: float,
):
    """Creates an IfcExtrudedAreaSolid from a list of points, specified as Python tuples"""
    curve = ifcfile.createIfcPolyLine(
        [ifcfile.createIfcCartesianPoint(p) for p in points])
    ifc_closed_profile = ifcfile.createIfcArbitraryClosedProfileDef(
        "AREA", None, curve)
    ifc_extrusion_direction = ifcfile.createIfcDirection(extrusion_direction)

    return ifcfile.createIfcExtrudedAreaSolid(
        ifc_closed_profile,
        ifc_axis2placement3d,
        ifc_extrusion_direction,
        extrusion_length
    )
