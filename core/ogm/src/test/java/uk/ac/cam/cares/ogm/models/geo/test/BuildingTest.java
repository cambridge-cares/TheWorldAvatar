package uk.ac.cam.cares.ogm.models.geo.test;

import org.citydb.database.adapter.blazegraph.SchemaManagerAdapter;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;
import uk.ac.cam.cares.ogm.models.ModelAnnotation;
import uk.ac.cam.cares.ogm.models.geo.Building;
import uk.ac.cam.cares.ogm.models.geo.SurfaceGeometry;
import uk.ac.cam.cares.ogm.models.geo.ThematicSurface;

import java.lang.reflect.Field;
import java.math.BigInteger;
import java.net.URI;
import java.util.ArrayList;

import static org.junit.jupiter.api.Assertions.*;

public class BuildingTest {

    @Test
    public void testNewBuildingAnnotations() throws NoSuchFieldException, IllegalAccessException {

        Building building = new Building();

        assertEquals(SchemaManagerAdapter.BUILDING_GRAPH + "/", building.getClass().getAnnotation(ModelAnnotation.class).defaultGraphName());

        //Test field Annotations
        Field function = building.getClass().getDeclaredField("function");
        assertEquals(SchemaManagerAdapter.ONTO_FUNCTION, function.getAnnotation(FieldAnnotation.class).value());
        assertFalse(function.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", function.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, function.getAnnotation(FieldAnnotation.class).innerType());

        Field roofType = building.getClass().getDeclaredField("roofType");
        assertEquals(SchemaManagerAdapter.ONTO_ROOF_TYPE, roofType.getAnnotation(FieldAnnotation.class).value());
        assertFalse(roofType.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", roofType.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, roofType.getAnnotation(FieldAnnotation.class).innerType());

        Field buildingParentId = building.getClass().getDeclaredField("buildingParentId");
        assertEquals(SchemaManagerAdapter.ONTO_BUILDING_PARENT_ID, buildingParentId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(buildingParentId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", buildingParentId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, buildingParentId.getAnnotation(FieldAnnotation.class).innerType());

        Field buildingRootId = building.getClass().getDeclaredField("buildingRootId");
        assertEquals(SchemaManagerAdapter.ONTO_BUILDING_ROOT_ID, buildingRootId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(buildingRootId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", buildingRootId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, buildingRootId.getAnnotation(FieldAnnotation.class).innerType());

        Field classID = building.getClass().getDeclaredField("classID");
        assertEquals(SchemaManagerAdapter.ONTO_CLASS, classID.getAnnotation(FieldAnnotation.class).value());
        assertFalse(classID.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", classID.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, classID.getAnnotation(FieldAnnotation.class).innerType());

        Field classCodespace = building.getClass().getDeclaredField("classCodespace");
        assertEquals(SchemaManagerAdapter.ONTO_CLASS_CODESPACE, classCodespace.getAnnotation(FieldAnnotation.class).value());
        assertFalse(classCodespace.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", classCodespace.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, classCodespace.getAnnotation(FieldAnnotation.class).innerType());

        Field functionCodespace = building.getClass().getDeclaredField("functionCodespace");
        assertEquals(SchemaManagerAdapter.ONTO_FUNCTION_CODESPACE, functionCodespace.getAnnotation(FieldAnnotation.class).value());
        assertFalse(functionCodespace.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", functionCodespace.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, functionCodespace.getAnnotation(FieldAnnotation.class).innerType());

        Field lod0FootprintId = building.getClass().getDeclaredField("lod0FootprintId");
        assertEquals(SchemaManagerAdapter.ONTO_FOOTPRINT_ID, lod0FootprintId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lod0FootprintId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lod0FootprintId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lod0FootprintId.getAnnotation(FieldAnnotation.class).innerType());

        Field lod0RoofprintId = building.getClass().getDeclaredField("lod0RoofprintId");
        assertEquals(SchemaManagerAdapter.ONTO_ROOFPRINT_ID, lod0RoofprintId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lod0RoofprintId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lod0RoofprintId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lod0RoofprintId.getAnnotation(FieldAnnotation.class).innerType());

        Field lod1MultiSurfaceId = building.getClass().getDeclaredField("lod1MultiSurfaceId");
        assertEquals(SchemaManagerAdapter.ONTO_LOD1_MULTI_SURFACE_ID, lod1MultiSurfaceId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lod1MultiSurfaceId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lod1MultiSurfaceId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lod1MultiSurfaceId.getAnnotation(FieldAnnotation.class).innerType());

        Field lod2MultiSurfaceId = building.getClass().getDeclaredField("lod2MultiSurfaceId");
        assertEquals(SchemaManagerAdapter.ONTO_LOD2_MULTI_SURFACE_ID, lod2MultiSurfaceId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lod2MultiSurfaceId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lod2MultiSurfaceId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lod2MultiSurfaceId.getAnnotation(FieldAnnotation.class).innerType());

        Field lod3MultiSurfaceId = building.getClass().getDeclaredField("lod3MultiSurfaceId");
        assertEquals(SchemaManagerAdapter.ONTO_LOD3_MULTI_SURFACE_ID, lod3MultiSurfaceId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lod3MultiSurfaceId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lod3MultiSurfaceId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lod3MultiSurfaceId.getAnnotation(FieldAnnotation.class).innerType());

        Field lod4MultiSurfaceId = building.getClass().getDeclaredField("lod4MultiSurfaceId");
        assertEquals(SchemaManagerAdapter.ONTO_LOD4_MULTI_SURFACE_ID, lod4MultiSurfaceId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lod4MultiSurfaceId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lod4MultiSurfaceId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lod4MultiSurfaceId.getAnnotation(FieldAnnotation.class).innerType());

        Field lod1SolidId = building.getClass().getDeclaredField("lod1SolidId");
        assertEquals(SchemaManagerAdapter.ONTO_LOD1_SOLID_ID, lod1SolidId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lod1SolidId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lod1SolidId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lod1SolidId.getAnnotation(FieldAnnotation.class).innerType());

        Field lod2SolidId = building.getClass().getDeclaredField("lod2SolidId");
        assertEquals(SchemaManagerAdapter.ONTO_LOD2_SOLID_ID, lod2SolidId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lod2SolidId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lod2SolidId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lod2SolidId.getAnnotation(FieldAnnotation.class).innerType());

        Field lod3SolidId = building.getClass().getDeclaredField("lod3SolidId");
        assertEquals(SchemaManagerAdapter.ONTO_LOD3_SOLID_ID, lod3SolidId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lod3SolidId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lod3SolidId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lod3SolidId.getAnnotation(FieldAnnotation.class).innerType());

        Field lod4SolidId = building.getClass().getDeclaredField("lod4SolidId");
        assertEquals(SchemaManagerAdapter.ONTO_LOD4_SOLID_ID, lod4SolidId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lod4SolidId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lod4SolidId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lod4SolidId.getAnnotation(FieldAnnotation.class).innerType());

        Field lod1TerrainIntersection = building.getClass().getDeclaredField("lod1TerrainIntersection");
        assertEquals(SchemaManagerAdapter.ONTO_LOD1_TERRAIN_INTERSECTION, lod1TerrainIntersection.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lod1TerrainIntersection.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lod1TerrainIntersection.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lod1TerrainIntersection.getAnnotation(FieldAnnotation.class).innerType());

        Field lod2TerrainIntersection = building.getClass().getDeclaredField("lod2TerrainIntersection");
        assertEquals(SchemaManagerAdapter.ONTO_LOD2_TERRAIN_INTERSECTION, lod2TerrainIntersection.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lod2TerrainIntersection.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lod2TerrainIntersection.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lod2TerrainIntersection.getAnnotation(FieldAnnotation.class).innerType());

        Field lod3TerrainIntersection = building.getClass().getDeclaredField("lod3TerrainIntersection");
        assertEquals(SchemaManagerAdapter.ONTO_LOD3_TERRAIN_INTERSECTION, lod3TerrainIntersection.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lod3TerrainIntersection.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lod3TerrainIntersection.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lod3TerrainIntersection.getAnnotation(FieldAnnotation.class).innerType());

        Field lod4TerrainIntersection = building.getClass().getDeclaredField("lod4TerrainIntersection");
        assertEquals(SchemaManagerAdapter.ONTO_LOD4_TERRAIN_INTERSECTION, lod4TerrainIntersection.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lod4TerrainIntersection.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lod4TerrainIntersection.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lod4TerrainIntersection.getAnnotation(FieldAnnotation.class).innerType());

        Field lod2MultiCurve = building.getClass().getDeclaredField("lod2MultiCurve");
        assertEquals(SchemaManagerAdapter.ONTO_LOD2_MULTI_CURVE, lod2MultiCurve.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lod2MultiCurve.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lod2MultiCurve.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lod2MultiCurve.getAnnotation(FieldAnnotation.class).innerType());

        Field lod3MultiCurve = building.getClass().getDeclaredField("lod3MultiCurve");
        assertEquals(SchemaManagerAdapter.ONTO_LOD3_MULTI_CURVE, lod3MultiCurve.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lod3MultiCurve.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lod3MultiCurve.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lod3MultiCurve.getAnnotation(FieldAnnotation.class).innerType());

        Field lod4MultiCurve = building.getClass().getDeclaredField("lod4MultiCurve");
        assertEquals(SchemaManagerAdapter.ONTO_LOD4_MULTI_CURVE, lod4MultiCurve.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lod4MultiCurve.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lod4MultiCurve.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lod4MultiCurve.getAnnotation(FieldAnnotation.class).innerType());

        Field measuredHeight = building.getClass().getDeclaredField("measuredHeight");
        assertEquals(SchemaManagerAdapter.ONTO_MEASURED_HEIGHT, measuredHeight.getAnnotation(FieldAnnotation.class).value());
        assertFalse(measuredHeight.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", measuredHeight.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, measuredHeight.getAnnotation(FieldAnnotation.class).innerType());

        Field measuredHeightUnit = building.getClass().getDeclaredField("measuredHeightUnit");
        assertEquals(SchemaManagerAdapter.ONTO_MEASURED_HEIGHT_UNIT, measuredHeightUnit.getAnnotation(FieldAnnotation.class).value());
        assertFalse(measuredHeightUnit.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", measuredHeightUnit.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, measuredHeightUnit.getAnnotation(FieldAnnotation.class).innerType());

        Field objectClassId = building.getClass().getDeclaredField("objectClassId");
        objectClassId.setAccessible(true);
        assertEquals(objectClassId.get(building), building.OBJECT_CLASS_ID);
        assertEquals(SchemaManagerAdapter.ONTO_OBJECT_CLASS_ID, objectClassId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(objectClassId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", objectClassId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, objectClassId.getAnnotation(FieldAnnotation.class).innerType());

        Field roofTypeCodespace = building.getClass().getDeclaredField("roofTypeCodespace");
        assertEquals(SchemaManagerAdapter.ONTO_ROOF_TYPE_CODESPACE, roofTypeCodespace.getAnnotation(FieldAnnotation.class).value());
        assertFalse(roofTypeCodespace.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", roofTypeCodespace.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, roofTypeCodespace.getAnnotation(FieldAnnotation.class).innerType());

        Field storeyHeightsAboveGround = building.getClass().getDeclaredField("storeyHeightsAboveGround");
        assertEquals(SchemaManagerAdapter.ONTO_STOREY_HEIGHTS_ABOVE_GROUND, storeyHeightsAboveGround.getAnnotation(FieldAnnotation.class).value());
        assertFalse(storeyHeightsAboveGround.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", storeyHeightsAboveGround.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, storeyHeightsAboveGround.getAnnotation(FieldAnnotation.class).innerType());

        Field storeyHeightsBelowGround = building.getClass().getDeclaredField("storeyHeightsBelowGround");
        assertEquals(SchemaManagerAdapter.ONTO_STOREY_HEIGHTS_BELLOW_GROUND, storeyHeightsBelowGround.getAnnotation(FieldAnnotation.class).value());
        assertFalse(storeyHeightsBelowGround.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", storeyHeightsBelowGround.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, storeyHeightsBelowGround.getAnnotation(FieldAnnotation.class).innerType());

        Field storeyHeightsAgUnit = building.getClass().getDeclaredField("storeyHeightsAgUnit");
        assertEquals(SchemaManagerAdapter.ONTO_STOREY_HEIGHTS_AG_UNIT, storeyHeightsAgUnit.getAnnotation(FieldAnnotation.class).value());
        assertFalse(storeyHeightsAgUnit.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", storeyHeightsAgUnit.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, storeyHeightsAgUnit.getAnnotation(FieldAnnotation.class).innerType());

        Field storeyHeightsBgUnit = building.getClass().getDeclaredField("storeyHeightsBgUnit");
        assertEquals(SchemaManagerAdapter.ONTO_STOREY_HEIGHTS_BG_UNIT, storeyHeightsBgUnit.getAnnotation(FieldAnnotation.class).value());
        assertFalse(storeyHeightsBgUnit.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", storeyHeightsBgUnit.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, storeyHeightsBgUnit.getAnnotation(FieldAnnotation.class).innerType());

        Field storeysAboveGround = building.getClass().getDeclaredField("storeysAboveGround");
        assertEquals(SchemaManagerAdapter.ONTO_STOREYS_ABOVE_GROUND, storeysAboveGround.getAnnotation(FieldAnnotation.class).value());
        assertFalse(storeysAboveGround.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", storeysAboveGround.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, storeysAboveGround.getAnnotation(FieldAnnotation.class).innerType());

        Field storeysBelowGround = building.getClass().getDeclaredField("storeysBelowGround");
        assertEquals(SchemaManagerAdapter.ONTO_STOREYS_BELLOW_GROUND, storeysBelowGround.getAnnotation(FieldAnnotation.class).value());
        assertFalse(storeysBelowGround.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", storeysBelowGround.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, storeysBelowGround.getAnnotation(FieldAnnotation.class).innerType());

        Field usage = building.getClass().getDeclaredField("usage");
        assertEquals(SchemaManagerAdapter.ONTO_USAGE, usage.getAnnotation(FieldAnnotation.class).value());
        assertFalse(usage.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", usage.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, usage.getAnnotation(FieldAnnotation.class).innerType());

        Field usageCodespace = building.getClass().getDeclaredField("usageCodespace");
        assertEquals(SchemaManagerAdapter.ONTO_USAGE_CODESPACE, usageCodespace.getAnnotation(FieldAnnotation.class).value());
        assertFalse(usageCodespace.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", usageCodespace.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, usageCodespace.getAnnotation(FieldAnnotation.class).innerType());

        Field yearOfConstruction = building.getClass().getDeclaredField("yearOfConstruction");
        assertEquals(SchemaManagerAdapter.ONTO_YEAR_CONSTRUCTION, yearOfConstruction.getAnnotation(FieldAnnotation.class).value());
        assertFalse(yearOfConstruction.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", yearOfConstruction.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, yearOfConstruction.getAnnotation(FieldAnnotation.class).innerType());

        Field yearOfDemolition = building.getClass().getDeclaredField("yearOfDemolition");
        assertEquals(SchemaManagerAdapter.ONTO_YEAR_DEMOLITION, yearOfDemolition.getAnnotation(FieldAnnotation.class).value());
        assertFalse(yearOfDemolition.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", yearOfDemolition.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, yearOfDemolition.getAnnotation(FieldAnnotation.class).innerType());

        Field thematicSurfaces = building.getClass().getDeclaredField("thematicSurfaces");
        assertEquals(SchemaManagerAdapter.ONTO_BUILDING_ID, thematicSurfaces.getAnnotation(FieldAnnotation.class).value());
        assertTrue(thematicSurfaces.getAnnotation(FieldAnnotation.class).backward());
        assertEquals(SchemaManagerAdapter.THEMATIC_SURFACE_GRAPH + "/", thematicSurfaces.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(ThematicSurface.class, thematicSurfaces.getAnnotation(FieldAnnotation.class).innerType());

        Field OBJECT_CLASS_ID = building.getClass().getDeclaredField("OBJECT_CLASS_ID");
        assertEquals(BigInteger.valueOf(26), OBJECT_CLASS_ID.get(building));

    }

    @Test
    public void testNewBuildingMethods() throws NoSuchMethodException {

        Building building = new Building();

        assertNotNull(building.getClass().getDeclaredMethod("getFunction"));
        assertNotNull(building.getClass().getDeclaredMethod("setFunction", String.class));

        assertNotNull(building.getClass().getDeclaredMethod("getRoofType"));
        assertNotNull(building.getClass().getDeclaredMethod("setRoofType", String.class));

        assertNotNull(building.getClass().getDeclaredMethod("getBuildingParentId"));
        assertNotNull(building.getClass().getDeclaredMethod("setBuildingParentId", URI.class));

        assertNotNull(building.getClass().getDeclaredMethod("getBuildingRootId"));
        assertNotNull(building.getClass().getDeclaredMethod("setBuildingRootId", URI.class));

        assertNotNull(building.getClass().getDeclaredMethod("getClassID"));
        assertNotNull(building.getClass().getDeclaredMethod("setClassID", String.class));

        assertNotNull(building.getClass().getDeclaredMethod("getClassCodespace"));
        assertNotNull(building.getClass().getDeclaredMethod("setClassCodespace", String.class));

        assertNotNull(building.getClass().getDeclaredMethod("getFunctionCodespace"));
        assertNotNull(building.getClass().getDeclaredMethod("setFunctionCodespace", String.class));

        assertNotNull(building.getClass().getDeclaredMethod("getLod0FootprintId"));
        assertNotNull(building.getClass().getDeclaredMethod("setLod0FootprintId", SurfaceGeometry.class));

        assertNotNull(building.getClass().getDeclaredMethod("getLod0RoofprintId"));
        assertNotNull(building.getClass().getDeclaredMethod("setLod0RoofprintId", SurfaceGeometry.class));

        assertNotNull(building.getClass().getDeclaredMethod("getLod1MultiSurfaceId"));
        assertNotNull(building.getClass().getDeclaredMethod("setLod1MultiSurfaceId", SurfaceGeometry.class));

        assertNotNull(building.getClass().getDeclaredMethod("getLod2MultiSurfaceId"));
        assertNotNull(building.getClass().getDeclaredMethod("setLod2MultiSurfaceId", SurfaceGeometry.class));

        assertNotNull(building.getClass().getDeclaredMethod("getLod3MultiSurfaceId"));
        assertNotNull(building.getClass().getDeclaredMethod("setLod3MultiSurfaceId", SurfaceGeometry.class));

        assertNotNull(building.getClass().getDeclaredMethod("getLod4MultiSurfaceId"));
        assertNotNull(building.getClass().getDeclaredMethod("setLod4MultiSurfaceId", SurfaceGeometry.class));

        assertNotNull(building.getClass().getDeclaredMethod("getLod1SolidId"));
        assertNotNull(building.getClass().getDeclaredMethod("setLod1SolidId", SurfaceGeometry.class));

        assertNotNull(building.getClass().getDeclaredMethod("getLod2SolidId"));
        assertNotNull(building.getClass().getDeclaredMethod("setLod2SolidId", SurfaceGeometry.class));

        assertNotNull(building.getClass().getDeclaredMethod("getLod3SolidId"));
        assertNotNull(building.getClass().getDeclaredMethod("setLod3SolidId", SurfaceGeometry.class));

        assertNotNull(building.getClass().getDeclaredMethod("getLod4SolidId"));
        assertNotNull(building.getClass().getDeclaredMethod("setLod4SolidId", SurfaceGeometry.class));

        assertNotNull(building.getClass().getDeclaredMethod("getLod1TerrainIntersection"));
        assertNotNull(building.getClass().getDeclaredMethod("setLod1TerrainIntersection", URI.class));

        assertNotNull(building.getClass().getDeclaredMethod("getLod2TerrainIntersection"));
        assertNotNull(building.getClass().getDeclaredMethod("setLod2TerrainIntersection", URI.class));

        assertNotNull(building.getClass().getDeclaredMethod("getLod3TerrainIntersection"));
        assertNotNull(building.getClass().getDeclaredMethod("setLod3TerrainIntersection", URI.class));

        assertNotNull(building.getClass().getDeclaredMethod("getLod4TerrainIntersection"));
        assertNotNull(building.getClass().getDeclaredMethod("setLod4TerrainIntersection", URI.class));

        assertNotNull(building.getClass().getDeclaredMethod("getLod2MultiCurve"));
        assertNotNull(building.getClass().getDeclaredMethod("setLod2MultiCurve", URI.class));

        assertNotNull(building.getClass().getDeclaredMethod("getLod3MultiCurve"));
        assertNotNull(building.getClass().getDeclaredMethod("setLod3MultiCurve", URI.class));

        assertNotNull(building.getClass().getDeclaredMethod("getLod4MultiCurve"));
        assertNotNull(building.getClass().getDeclaredMethod("setLod4MultiCurve", URI.class));

        assertNotNull(building.getClass().getDeclaredMethod("getMeasuredHeight"));
        assertNotNull(building.getClass().getDeclaredMethod("setMeasuredHeight", Double.class));

        assertNotNull(building.getClass().getDeclaredMethod("getMeasuredHeightUnit"));
        assertNotNull(building.getClass().getDeclaredMethod("setMeasuredHeightUnit", String.class));

        assertNotNull(building.getClass().getDeclaredMethod("getObjectClassId"));
        assertNotNull(building.getClass().getDeclaredMethod("setObjectClassId", BigInteger.class));

        assertNotNull(building.getClass().getDeclaredMethod("getRoofTypeCodespace"));
        assertNotNull(building.getClass().getDeclaredMethod("setRoofTypeCodespace", String.class));

        assertNotNull(building.getClass().getDeclaredMethod("getStoreyHeightsAboveGround"));
        assertNotNull(building.getClass().getDeclaredMethod("setStoreyHeightsAboveGround", String.class));

        assertNotNull(building.getClass().getDeclaredMethod("getStoreyHeightsBelowGround"));
        assertNotNull(building.getClass().getDeclaredMethod("setStoreyHeightsBelowGround", String.class));

        assertNotNull(building.getClass().getDeclaredMethod("getStoreyHeightsAgUnit"));
        assertNotNull(building.getClass().getDeclaredMethod("setStoreyHeightsAgUnit", String.class));

        assertNotNull(building.getClass().getDeclaredMethod("getStoreyHeightsBgUnit"));
        assertNotNull(building.getClass().getDeclaredMethod("setStoreyHeightsBgUnit", String.class));

        assertNotNull(building.getClass().getDeclaredMethod("getStoreysAboveGround"));
        assertNotNull(building.getClass().getDeclaredMethod("setStoreysAboveGround", String.class));

        assertNotNull(building.getClass().getDeclaredMethod("getStoreysBelowGround"));
        assertNotNull(building.getClass().getDeclaredMethod("setStoreysBelowGround", String.class));

        assertNotNull(building.getClass().getDeclaredMethod("getUsage"));
        assertNotNull(building.getClass().getDeclaredMethod("setUsage", String.class));

        assertNotNull(building.getClass().getDeclaredMethod("getUsageCodespace"));
        assertNotNull(building.getClass().getDeclaredMethod("setUsageCodespace", String.class));

        assertNotNull(building.getClass().getDeclaredMethod("getYearOfConstruction"));
        assertNotNull(building.getClass().getDeclaredMethod("setYearOfConstruction", String.class));

        assertNotNull(building.getClass().getDeclaredMethod("getYearOfDemolition"));
        assertNotNull(building.getClass().getDeclaredMethod("setYearOfDemolition", String.class));

        assertNotNull(building.getClass().getDeclaredMethod("getThematicSurfaces"));
        assertNotNull(building.getClass().getDeclaredMethod("setThematicSurfaces", ArrayList.class));
    }
}
