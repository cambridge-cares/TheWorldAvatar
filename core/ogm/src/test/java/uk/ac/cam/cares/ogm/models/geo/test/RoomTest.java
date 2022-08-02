package uk.ac.cam.cares.ogm.models.geo.test;

import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;
import uk.ac.cam.cares.ogm.models.ModelAnnotation;
import uk.ac.cam.cares.ogm.models.geo.Building;
import uk.ac.cam.cares.ogm.models.geo.Room;
import uk.ac.cam.cares.ogm.models.geo.SchemaManagerAdapter;
import uk.ac.cam.cares.ogm.models.geo.SurfaceGeometry;

import java.lang.reflect.Field;
import java.math.BigInteger;

import static org.junit.jupiter.api.Assertions.*;

public class RoomTest {

    @Test
    public void testNewRoomAnnotations() throws NoSuchFieldException, IllegalAccessException {

        Room room = new Room();

        assertEquals(SchemaManagerAdapter.ROOM_GRAPH + "/", room.getClass().getAnnotation(ModelAnnotation.class).defaultGraphName());

        Field objectClassId = room.getClass().getDeclaredField("objectClassId");
        objectClassId.setAccessible(true);
        assertEquals(objectClassId.get(room), room.OBJECT_CLASS_ID);
        assertEquals(SchemaManagerAdapter.ONTO_OBJECT_CLASS_ID, objectClassId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(objectClassId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", objectClassId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, objectClassId.getAnnotation(FieldAnnotation.class).innerType());

        Field usage = room.getClass().getDeclaredField("usage");
        assertEquals(SchemaManagerAdapter.ONTO_USAGE, usage.getAnnotation(FieldAnnotation.class).value());
        assertFalse(usage.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", usage.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, usage.getAnnotation(FieldAnnotation.class).innerType());

        Field usageCodespace = room.getClass().getDeclaredField("usageCodespace");
        assertEquals(SchemaManagerAdapter.ONTO_USAGE_CODESPACE, usageCodespace.getAnnotation(FieldAnnotation.class).value());
        assertFalse(usageCodespace.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", usageCodespace.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, usageCodespace.getAnnotation(FieldAnnotation.class).innerType());

        Field function = room.getClass().getDeclaredField("function");
        assertEquals(SchemaManagerAdapter.ONTO_FUNCTION, function.getAnnotation(FieldAnnotation.class).value());
        assertFalse(function.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", function.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, function.getAnnotation(FieldAnnotation.class).innerType());

        Field functionCodespace = room.getClass().getDeclaredField("functionCodespace");
        assertEquals(SchemaManagerAdapter.ONTO_FUNCTION_CODESPACE, functionCodespace.getAnnotation(FieldAnnotation.class).value());
        assertFalse(functionCodespace.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", functionCodespace.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, functionCodespace.getAnnotation(FieldAnnotation.class).innerType());

        Field classID = room.getClass().getDeclaredField("classID");
        assertEquals(SchemaManagerAdapter.ONTO_CLASS, classID.getAnnotation(FieldAnnotation.class).value());
        assertFalse(classID.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", classID.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, classID.getAnnotation(FieldAnnotation.class).innerType());

        Field classCodespace = room.getClass().getDeclaredField("classCodespace");
        assertEquals(SchemaManagerAdapter.ONTO_CLASS_CODESPACE, classCodespace.getAnnotation(FieldAnnotation.class).value());
        assertFalse(classCodespace.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", classCodespace.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, classCodespace.getAnnotation(FieldAnnotation.class).innerType());

        Field buildingId = room.getClass().getDeclaredField("buildingId");
        assertEquals(SchemaManagerAdapter.ONTO_BUILDING_ID, buildingId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(buildingId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", buildingId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, buildingId.getAnnotation(FieldAnnotation.class).innerType());

        Field lod4MultiSurfaceId = room.getClass().getDeclaredField("lod4MultiSurfaceId");
        assertEquals(SchemaManagerAdapter.ONTO_LOD4_MULTI_SURFACE_ID, lod4MultiSurfaceId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lod4MultiSurfaceId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lod4MultiSurfaceId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lod4MultiSurfaceId.getAnnotation(FieldAnnotation.class).innerType());

        Field lod4SolidId = room.getClass().getDeclaredField("lod4SolidId");
        assertEquals(SchemaManagerAdapter.ONTO_LOD4_SOLID_ID, lod4SolidId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lod4SolidId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lod4SolidId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lod4SolidId.getAnnotation(FieldAnnotation.class).innerType());

        Field OBJECT_CLASS_ID = room.getClass().getDeclaredField("OBJECT_CLASS_ID");
        assertEquals(BigInteger.valueOf(41), OBJECT_CLASS_ID.get(room));
    }

    @Test
    public void testNewRoomMethods() throws NoSuchMethodException {

        Room room = new Room();

        assertNotNull(room.getClass().getDeclaredMethod("getObjectClassId"));
        assertNotNull(room.getClass().getDeclaredMethod("setObjectClassId", BigInteger.class));

        assertNotNull(room.getClass().getDeclaredMethod("getUsage"));
        assertNotNull(room.getClass().getDeclaredMethod("setUsage", String.class));

        assertNotNull(room.getClass().getDeclaredMethod("getUsageCodespace"));
        assertNotNull(room.getClass().getDeclaredMethod("setUsageCodespace", String.class));

        assertNotNull(room.getClass().getDeclaredMethod("getFunction"));
        assertNotNull(room.getClass().getDeclaredMethod("setFunction", String.class));

        assertNotNull(room.getClass().getDeclaredMethod("getFunctionCodespace"));
        assertNotNull(room.getClass().getDeclaredMethod("setFunctionCodespace", String.class));

        assertNotNull(room.getClass().getDeclaredMethod("getClassID"));
        assertNotNull(room.getClass().getDeclaredMethod("setClassID", String.class));

        assertNotNull(room.getClass().getDeclaredMethod("getClassCodespace"));
        assertNotNull(room.getClass().getDeclaredMethod("setClassCodespace", String.class));

        assertNotNull(room.getClass().getDeclaredMethod("getBuildingId"));
        assertNotNull(room.getClass().getDeclaredMethod("setBuildingId", Building.class));

        assertNotNull(room.getClass().getDeclaredMethod("getLod4MultiSurfaceId"));
        assertNotNull(room.getClass().getDeclaredMethod("setLod4MultiSurfaceId", SurfaceGeometry.class));

        assertNotNull(room.getClass().getDeclaredMethod("getLod4SolidId"));
        assertNotNull(room.getClass().getDeclaredMethod("setLod4SolidId", SurfaceGeometry.class));
    }
}
