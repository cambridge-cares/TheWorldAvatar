package uk.ac.cam.cares.ogm.models.geo.test;

import org.citydb.database.adapter.blazegraph.SchemaManagerAdapter;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;
import uk.ac.cam.cares.ogm.models.ModelAnnotation;
import uk.ac.cam.cares.ogm.models.geo.ExternalReference;

import java.lang.reflect.Field;
import java.net.URI;

import static org.junit.jupiter.api.Assertions.*;

public class ExternalReferenceTest {

    @Test
    public void testNewExternalReferenceAnnotations() throws NoSuchFieldException {

        ExternalReference externalReference = new ExternalReference();

        assertEquals(SchemaManagerAdapter.EXTERNAL_REFERENCES_GRAPH + "/", externalReference.getClass().getAnnotation(ModelAnnotation.class).defaultGraphName());

        //Test field Annotations
        Field cityObjectId = externalReference.getClass().getDeclaredField("cityObjectId");
        assertEquals(SchemaManagerAdapter.ONTO_CITY_OBJECT_ID, cityObjectId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(cityObjectId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", cityObjectId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, cityObjectId.getAnnotation(FieldAnnotation.class).innerType());

        Field infoSys = externalReference.getClass().getDeclaredField("infoSys");
        assertEquals(SchemaManagerAdapter.ONTO_INFO_SYS, infoSys.getAnnotation(FieldAnnotation.class).value());
        assertFalse(infoSys.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", infoSys.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, infoSys.getAnnotation(FieldAnnotation.class).innerType());

        Field name = externalReference.getClass().getDeclaredField("name");
        assertEquals(SchemaManagerAdapter.ONTO_NAME, name.getAnnotation(FieldAnnotation.class).value());
        assertFalse(name.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", name.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, name.getAnnotation(FieldAnnotation.class).innerType());

        Field uri = externalReference.getClass().getDeclaredField("uri");
        assertEquals(SchemaManagerAdapter.ONTO_URI, uri.getAnnotation(FieldAnnotation.class).value());
        assertFalse(uri.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", uri.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, uri.getAnnotation(FieldAnnotation.class).innerType());
    }

    @Test
    public void testNewExternalReferenceMethods() throws NoSuchMethodException {

        ExternalReference externalReference = new ExternalReference();

        assertNotNull(externalReference.getClass().getDeclaredMethod("getCityObjectId"));
        assertNotNull(externalReference.getClass().getDeclaredMethod("setCityObjectId", URI.class));

        assertNotNull(externalReference.getClass().getDeclaredMethod("getInfoSys"));
        assertNotNull(externalReference.getClass().getDeclaredMethod("setInfoSys", String.class));

        assertNotNull(externalReference.getClass().getDeclaredMethod("getName"));
        assertNotNull(externalReference.getClass().getDeclaredMethod("setName", String.class));

        assertNotNull(externalReference.getClass().getDeclaredMethod("getUri"));
        assertNotNull(externalReference.getClass().getDeclaredMethod("setUri", URI.class));
    }
}
