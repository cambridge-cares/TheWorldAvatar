package uk.ac.cam.cares.ogm.models.geo.test;

import org.citydb.database.adapter.blazegraph.SchemaManagerAdapter;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;
import uk.ac.cam.cares.ogm.models.ModelAnnotation;
import uk.ac.cam.cares.ogm.models.geo.*;

import java.lang.reflect.Field;
import java.math.BigInteger;
import java.net.URI;
import java.util.ArrayList;

import static org.junit.jupiter.api.Assertions.*;

public class ThematicSurfaceTest {

    @Test
    public void testNewThematicSurfaceAnnotations() throws NoSuchFieldException {

        ThematicSurface thematicSurface = new ThematicSurface();

        assertEquals(SchemaManagerAdapter.THEMATIC_SURFACE_GRAPH + "/", thematicSurface.getClass().getAnnotation(ModelAnnotation.class).defaultGraphName());

        Field buildingId = thematicSurface.getClass().getDeclaredField("buildingId");
        assertEquals(SchemaManagerAdapter.ONTO_BUILDING_ID, buildingId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(buildingId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", buildingId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, buildingId.getAnnotation(FieldAnnotation.class).innerType());

        Field buildingInstallationId = thematicSurface.getClass().getDeclaredField("buildingInstallationId");
        assertEquals(SchemaManagerAdapter.ONTO_BUILDING_INSTALLATION_ID, buildingInstallationId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(buildingInstallationId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", buildingInstallationId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, buildingInstallationId.getAnnotation(FieldAnnotation.class).innerType());

        Field lod2MultiSurfaceId = thematicSurface.getClass().getDeclaredField("lod2MultiSurfaceId");
        assertEquals(SchemaManagerAdapter.ONTO_LOD2_MULTI_SURFACE_ID, lod2MultiSurfaceId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lod2MultiSurfaceId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lod2MultiSurfaceId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lod2MultiSurfaceId.getAnnotation(FieldAnnotation.class).innerType());

        Field lod3MultiSurfaceId = thematicSurface.getClass().getDeclaredField("lod3MultiSurfaceId");
        assertEquals(SchemaManagerAdapter.ONTO_LOD3_MULTI_SURFACE_ID, lod3MultiSurfaceId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lod3MultiSurfaceId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lod3MultiSurfaceId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lod3MultiSurfaceId.getAnnotation(FieldAnnotation.class).innerType());

        Field lod4MultiSurfaceId = thematicSurface.getClass().getDeclaredField("lod4MultiSurfaceId");
        assertEquals(SchemaManagerAdapter.ONTO_LOD4_MULTI_SURFACE_ID, lod4MultiSurfaceId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lod4MultiSurfaceId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lod4MultiSurfaceId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lod4MultiSurfaceId.getAnnotation(FieldAnnotation.class).innerType());

        Field objectClassId = thematicSurface.getClass().getDeclaredField("objectClassId");
        assertEquals(SchemaManagerAdapter.ONTO_OBJECT_CLASS_ID, objectClassId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(objectClassId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", objectClassId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, objectClassId.getAnnotation(FieldAnnotation.class).innerType());

        Field roomId = thematicSurface.getClass().getDeclaredField("roomId");
        assertEquals(SchemaManagerAdapter.ONTO_ROOM_ID, roomId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(roomId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", roomId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, roomId.getAnnotation(FieldAnnotation.class).innerType());

        Field openingId = thematicSurface.getClass().getDeclaredField("openingId");
        assertEquals(SchemaManagerAdapter.ONTO_OPENING_ID, openingId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(openingId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals(SchemaManagerAdapter.OPENING_TO_THEM_SURFACE_GRAPH + "/", openingId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Opening.class, openingId.getAnnotation(FieldAnnotation.class).innerType());

    }

    @Test
    public void testNewThematicSurfaceMethods() throws NoSuchMethodException {

        ThematicSurface thematicSurface = new ThematicSurface();

        assertNotNull(thematicSurface.getClass().getDeclaredMethod("getBuildingId"));
        assertNotNull(thematicSurface.getClass().getDeclaredMethod("setBuildingId", Building.class));

        assertNotNull(thematicSurface.getClass().getDeclaredMethod("getBuildingInstallationId"));
        assertNotNull(thematicSurface.getClass().getDeclaredMethod("setBuildingInstallationId", URI.class));

        assertNotNull(thematicSurface.getClass().getDeclaredMethod("getLod2MultiSurfaceId"));
        assertNotNull(thematicSurface.getClass().getDeclaredMethod("setLod2MultiSurfaceId", SurfaceGeometry.class));

        assertNotNull(thematicSurface.getClass().getDeclaredMethod("getLod3MultiSurfaceId"));
        assertNotNull(thematicSurface.getClass().getDeclaredMethod("setLod3MultiSurfaceId", SurfaceGeometry.class));

        assertNotNull(thematicSurface.getClass().getDeclaredMethod("getLod4MultiSurfaceId"));
        assertNotNull(thematicSurface.getClass().getDeclaredMethod("setLod4MultiSurfaceId", SurfaceGeometry.class));

        assertNotNull(thematicSurface.getClass().getDeclaredMethod("getObjectClassId"));
        assertNotNull(thematicSurface.getClass().getDeclaredMethod("setObjectClassId", BigInteger.class));

        assertNotNull(thematicSurface.getClass().getDeclaredMethod("getRoomId"));
        assertNotNull(thematicSurface.getClass().getDeclaredMethod("setRoomId", Room.class));

        assertNotNull(thematicSurface.getClass().getDeclaredMethod("getOpeningId"));
        assertNotNull(thematicSurface.getClass().getDeclaredMethod("setOpeningId", ArrayList.class));

    }

    @Test
    public void testNewThematicSurfaceFieldValues() throws NoSuchFieldException, IllegalAccessException {

        ThematicSurface thematicSurface = new ThematicSurface();

        Field CEILING_SURFACE = thematicSurface.getClass().getDeclaredField("CEILING_SURFACE");
        assertEquals(BigInteger.valueOf(30), CEILING_SURFACE.get(thematicSurface));

        Field INTERIOR_WALL_SURFACE = thematicSurface.getClass().getDeclaredField("INTERIOR_WALL_SURFACE");
        assertEquals(BigInteger.valueOf(31), INTERIOR_WALL_SURFACE.get(thematicSurface));

        Field FLOOR_SURFACE = thematicSurface.getClass().getDeclaredField("FLOOR_SURFACE");
        assertEquals(BigInteger.valueOf(32), FLOOR_SURFACE.get(thematicSurface));

        Field ROOF_SURFACE = thematicSurface.getClass().getDeclaredField("ROOF_SURFACE");
        assertEquals(BigInteger.valueOf(33), ROOF_SURFACE.get(thematicSurface));

        Field WALL_SURFACE = thematicSurface.getClass().getDeclaredField("WALL_SURFACE");
        assertEquals(BigInteger.valueOf(34), WALL_SURFACE.get(thematicSurface));

        Field GROUND_SURFACE = thematicSurface.getClass().getDeclaredField("GROUND_SURFACE");
        assertEquals(BigInteger.valueOf(35), GROUND_SURFACE.get(thematicSurface));

        Field CLOSURE_SURFACE = thematicSurface.getClass().getDeclaredField("CLOSURE_SURFACE");
        assertEquals(BigInteger.valueOf(36), CLOSURE_SURFACE.get(thematicSurface));

        Field OUTER_CEILING_SURFACE = thematicSurface.getClass().getDeclaredField("OUTER_CEILING_SURFACE");
        assertEquals(BigInteger.valueOf(60), OUTER_CEILING_SURFACE.get(thematicSurface));

        Field OUTER_FLOOR_SURFACE = thematicSurface.getClass().getDeclaredField("OUTER_FLOOR_SURFACE");
        assertEquals(BigInteger.valueOf(61), OUTER_FLOOR_SURFACE.get(thematicSurface));

    }
}
