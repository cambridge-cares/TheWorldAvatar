package uk.ac.cam.cares.ogm.models.geo.test;

import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;
import uk.ac.cam.cares.ogm.models.ModelAnnotation;
import uk.ac.cam.cares.ogm.models.ModelContext;
import uk.ac.cam.cares.ogm.models.geo.Opening;
import uk.ac.cam.cares.ogm.models.geo.SchemaManagerAdapter;
import uk.ac.cam.cares.ogm.models.geo.SurfaceGeometry;

import java.lang.reflect.Field;
import java.math.BigInteger;
import java.net.URI;

import static org.junit.jupiter.api.Assertions.*;

public class OpeningTest {

    @Test
    public void testNewOpeningAnnotations() throws NoSuchFieldException, IllegalAccessException {

        Opening opening = new Opening();

        assertEquals(SchemaManagerAdapter.OPENING_GRAPH + "/", opening.getClass().getAnnotation(ModelAnnotation.class).defaultGraphName());

        //Test field Annotations
        Field objectClassId = opening.getClass().getDeclaredField("objectClassId");
        assertEquals(SchemaManagerAdapter.ONTO_OBJECT_CLASS_ID, objectClassId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(objectClassId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", objectClassId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, objectClassId.getAnnotation(FieldAnnotation.class).innerType());

        Field addressId = opening.getClass().getDeclaredField("addressId");
        assertEquals(SchemaManagerAdapter.ONTO_ADDRESS_ID, addressId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(addressId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", addressId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, addressId.getAnnotation(FieldAnnotation.class).innerType());

        Field lod3ImplicitRefPoint = opening.getClass().getDeclaredField("lod3ImplicitRefPoint");
        assertEquals(SchemaManagerAdapter.ONTO_LOD3_IMPLICIT_REF_POINT, lod3ImplicitRefPoint.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lod3ImplicitRefPoint.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lod3ImplicitRefPoint.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lod3ImplicitRefPoint.getAnnotation(FieldAnnotation.class).innerType());

        Field lod3ImplicitRepId = opening.getClass().getDeclaredField("lod3ImplicitRepId");
        assertEquals(SchemaManagerAdapter.ONTO_LOD3_IMPLICIT_REP_ID, lod3ImplicitRepId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lod3ImplicitRepId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lod3ImplicitRepId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lod3ImplicitRepId.getAnnotation(FieldAnnotation.class).innerType());

        Field lod3ImplicitTransformation = opening.getClass().getDeclaredField("lod3ImplicitTransformation");
        assertEquals(SchemaManagerAdapter.ONTO_LOD3_IMPLICIT_TRANSFORMATION, lod3ImplicitTransformation.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lod3ImplicitTransformation.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lod3ImplicitTransformation.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lod3ImplicitTransformation.getAnnotation(FieldAnnotation.class).innerType());

        Field lod3MultiSurfaceId = opening.getClass().getDeclaredField("lod3MultiSurfaceId");
        assertEquals(SchemaManagerAdapter.ONTO_LOD3_MULTI_SURFACE_ID, lod3MultiSurfaceId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lod3MultiSurfaceId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lod3MultiSurfaceId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lod3MultiSurfaceId.getAnnotation(FieldAnnotation.class).innerType());

        Field lod4ImplicitRefPoint = opening.getClass().getDeclaredField("lod4ImplicitRefPoint");
        assertEquals(SchemaManagerAdapter.ONTO_LOD4_IMPLICIT_REF_POINT, lod4ImplicitRefPoint.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lod4ImplicitRefPoint.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lod4ImplicitRefPoint.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lod4ImplicitRefPoint.getAnnotation(FieldAnnotation.class).innerType());

        Field lod4ImplicitRepId = opening.getClass().getDeclaredField("lod4ImplicitRepId");
        assertEquals(SchemaManagerAdapter.ONTO_LOD4_IMPLICIT_REP_ID, lod4ImplicitRepId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lod4ImplicitRepId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lod4ImplicitRepId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lod4ImplicitRepId.getAnnotation(FieldAnnotation.class).innerType());

        Field lod4ImplicitTransformation = opening.getClass().getDeclaredField("lod4ImplicitTransformation");
        assertEquals(SchemaManagerAdapter.ONTO_LOD4_IMPLICIT_TRANSFORMATION, lod4ImplicitTransformation.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lod4ImplicitTransformation.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lod4ImplicitTransformation.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lod4ImplicitTransformation.getAnnotation(FieldAnnotation.class).innerType());

        Field lod4MultiSurfaceId = opening.getClass().getDeclaredField("lod4MultiSurfaceId");
        assertEquals(SchemaManagerAdapter.ONTO_LOD4_MULTI_SURFACE_ID, lod4MultiSurfaceId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lod4MultiSurfaceId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lod4MultiSurfaceId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lod4MultiSurfaceId.getAnnotation(FieldAnnotation.class).innerType());

        Field themSurfaceId = opening.getClass().getDeclaredField("themSurfaceId");
        assertEquals(SchemaManagerAdapter.ONTO_THEMSURFACE_ID, themSurfaceId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(themSurfaceId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals(SchemaManagerAdapter.OPENING_TO_THEM_SURFACE_GRAPH + "/", themSurfaceId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, themSurfaceId.getAnnotation(FieldAnnotation.class).innerType());

        Field WINDOW_CLASS_ID = opening.getClass().getDeclaredField("WINDOW_CLASS_ID");
        assertEquals(BigInteger.valueOf(38), WINDOW_CLASS_ID.get(opening));

        Field DOOR_CLASS_ID = opening.getClass().getDeclaredField("DOOR_CLASS_ID");
        assertEquals(BigInteger.valueOf(39), DOOR_CLASS_ID.get(opening));
    }

    @Test
    public void testNewOpeningMethods() throws NoSuchMethodException {

        Opening opening = new Opening();

        assertNotNull(opening.getClass().getDeclaredMethod("getObjectClassId"));
        assertNotNull(opening.getClass().getDeclaredMethod("setObjectClassId", BigInteger.class));

        assertNotNull(opening.getClass().getDeclaredMethod("getAddressId"));
        assertNotNull(opening.getClass().getDeclaredMethod("setAddressId", URI.class));

        assertNotNull(opening.getClass().getDeclaredMethod("getLod3ImplicitRefPoint"));
        assertNotNull(opening.getClass().getDeclaredMethod("setLod3ImplicitRefPoint", URI.class));

        assertNotNull(opening.getClass().getDeclaredMethod("getLod3ImplicitRepId"));
        assertNotNull(opening.getClass().getDeclaredMethod("setLod3ImplicitRepId", String.class));

        assertNotNull(opening.getClass().getDeclaredMethod("getLod3ImplicitTransformation"));
        assertNotNull(opening.getClass().getDeclaredMethod("setLod3ImplicitTransformation", String.class));

        assertNotNull(opening.getClass().getDeclaredMethod("getLod3MultiSurfaceId"));
        assertNotNull(opening.getClass().getDeclaredMethod("setLod3MultiSurfaceId", SurfaceGeometry.class));

        assertNotNull(opening.getClass().getDeclaredMethod("getLod4ImplicitRefPoint"));
        assertNotNull(opening.getClass().getDeclaredMethod("setLod4ImplicitRefPoint", URI.class));

        assertNotNull(opening.getClass().getDeclaredMethod("getLod4ImplicitRepId"));
        assertNotNull(opening.getClass().getDeclaredMethod("setLod4ImplicitRepId", String.class));

        assertNotNull(opening.getClass().getDeclaredMethod("getLod4ImplicitTransformation"));
        assertNotNull(opening.getClass().getDeclaredMethod("setLod4ImplicitTransformation", String.class));

        assertNotNull(opening.getClass().getDeclaredMethod("getLod4MultiSurfaceId"));
        assertNotNull(opening.getClass().getDeclaredMethod("setLod4MultiSurfaceId", SurfaceGeometry.class));

        assertNotNull(opening.getClass().getDeclaredMethod("getThemSurfaceId"));
        assertNotNull(opening.getClass().getDeclaredMethod("setThemSurfaceId", URI.class));
    }

    @Test
    public void testnewWindow(){

        ModelContext context = new ModelContext("", "");
        String iri = "http://localhost:9999/blazegraph/namespace/test/surfacegeometry/temp_uuid";
        Opening window = Opening.newWindow(context, iri);
        assertEquals(Opening.WINDOW_CLASS_ID, window.getObjectClassId());
    }

    @Test
    public void testnewDoor(){

        ModelContext context = new ModelContext("", "");
        String iri = "http://localhost:9999/blazegraph/namespace/test/surfacegeometry/temp_uuid";
        Opening door = Opening.newDoor(context, iri);
        assertEquals(Opening.DOOR_CLASS_ID, door.getObjectClassId());
    }
}
