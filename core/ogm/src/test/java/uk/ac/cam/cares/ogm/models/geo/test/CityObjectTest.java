package uk.ac.cam.cares.ogm.models.geo.test;

import org.citydb.database.adapter.blazegraph.SchemaManagerAdapter;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;
import uk.ac.cam.cares.ogm.models.ModelAnnotation;
import uk.ac.cam.cares.ogm.models.geo.CityObject;
import uk.ac.cam.cares.ogm.models.geo.EnvelopeType;
import uk.ac.cam.cares.ogm.models.geo.ExternalReference;
import uk.ac.cam.cares.ogm.models.geo.GenericAttribute;

import java.lang.reflect.Field;
import java.math.BigInteger;
import java.util.ArrayList;

import static org.junit.jupiter.api.Assertions.*;


public class CityObjectTest {

    @Test
    public void testNewCityObjectAnnotations() throws NoSuchFieldException {

        CityObject cityobject = new CityObject();

        assertEquals(SchemaManagerAdapter.CITY_OBJECT_GRAPH + "/", cityobject.getClass().getAnnotation(ModelAnnotation.class).defaultGraphName());

        //Test field Annotations
        Field creationDate = cityobject.getClass().getDeclaredField("creationDate");
        assertEquals(SchemaManagerAdapter.ONTO_CREATION_DATE, creationDate.getAnnotation(FieldAnnotation.class).value());
        assertFalse(creationDate.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", creationDate.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, creationDate.getAnnotation(FieldAnnotation.class).innerType());

        Field description = cityobject.getClass().getDeclaredField("description");
        assertEquals(SchemaManagerAdapter.ONTO_DESCRIPTION, description.getAnnotation(FieldAnnotation.class).value());
        assertFalse(description.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", description.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, description.getAnnotation(FieldAnnotation.class).innerType());

        Field envelopeType = cityobject.getClass().getDeclaredField("envelopeType");
        assertEquals(SchemaManagerAdapter.ONTO_ENVELOPE_TYPE, envelopeType.getAnnotation(FieldAnnotation.class).value());
        assertFalse(envelopeType.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", envelopeType.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, envelopeType.getAnnotation(FieldAnnotation.class).innerType());

        Field gmlId = cityobject.getClass().getDeclaredField("gmlId");
        assertEquals(SchemaManagerAdapter.ONTO_GML_ID, gmlId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(gmlId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", gmlId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, gmlId.getAnnotation(FieldAnnotation.class).innerType());

        Field lastModificationDate = cityobject.getClass().getDeclaredField("lastModificationDate");
        assertEquals(SchemaManagerAdapter.ONTO_LAST_MODIFICATION_DATE, lastModificationDate.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lastModificationDate.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lastModificationDate.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lastModificationDate.getAnnotation(FieldAnnotation.class).innerType());

        Field lineage = cityobject.getClass().getDeclaredField("lineage");
        assertEquals(SchemaManagerAdapter.ONTO_LINEAGE, lineage.getAnnotation(FieldAnnotation.class).value());
        assertFalse(lineage.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", lineage.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, lineage.getAnnotation(FieldAnnotation.class).innerType());

        Field name = cityobject.getClass().getDeclaredField("name");
        assertEquals(SchemaManagerAdapter.ONTO_NAME, name.getAnnotation(FieldAnnotation.class).value());
        assertFalse(name.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", name.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, name.getAnnotation(FieldAnnotation.class).innerType());

        Field nameCodespace = cityobject.getClass().getDeclaredField("nameCodespace");
        assertEquals(SchemaManagerAdapter.ONTO_NAME_CODESPACE, nameCodespace.getAnnotation(FieldAnnotation.class).value());
        assertFalse(nameCodespace.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", nameCodespace.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, nameCodespace.getAnnotation(FieldAnnotation.class).innerType());

        Field objectClassId = cityobject.getClass().getDeclaredField("objectClassId");
        assertEquals(SchemaManagerAdapter.ONTO_OBJECT_CLASS_ID, objectClassId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(objectClassId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", objectClassId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, objectClassId.getAnnotation(FieldAnnotation.class).innerType());

        Field reasonForUpdate = cityobject.getClass().getDeclaredField("reasonForUpdate");
        assertEquals(SchemaManagerAdapter.ONTO_REASON_FOR_UPDATE, reasonForUpdate.getAnnotation(FieldAnnotation.class).value());
        assertFalse(reasonForUpdate.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", reasonForUpdate.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, reasonForUpdate.getAnnotation(FieldAnnotation.class).innerType());

        Field relativeToTerrain = cityobject.getClass().getDeclaredField("relativeToTerrain");
        assertEquals(SchemaManagerAdapter.ONTO_RELATIVE_TO_TERRAIN, relativeToTerrain.getAnnotation(FieldAnnotation.class).value());
        assertFalse(relativeToTerrain.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", relativeToTerrain.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, relativeToTerrain.getAnnotation(FieldAnnotation.class).innerType());

        Field relativeToWater = cityobject.getClass().getDeclaredField("relativeToWater");
        assertEquals(SchemaManagerAdapter.ONTO_RELATIVE_TO_WATER, relativeToWater.getAnnotation(FieldAnnotation.class).value());
        assertFalse(relativeToWater.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", relativeToWater.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, relativeToWater.getAnnotation(FieldAnnotation.class).innerType());

        Field terminationDate = cityobject.getClass().getDeclaredField("terminationDate");
        assertEquals(SchemaManagerAdapter.ONTO_TERMINATION_DATE, terminationDate.getAnnotation(FieldAnnotation.class).value());
        assertFalse(terminationDate.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", terminationDate.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, terminationDate.getAnnotation(FieldAnnotation.class).innerType());

        Field updatingPerson = cityobject.getClass().getDeclaredField("updatingPerson");
        assertEquals(SchemaManagerAdapter.ONTO_UPDATING_PERSON, updatingPerson.getAnnotation(FieldAnnotation.class).value());
        assertFalse(updatingPerson.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", updatingPerson.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, updatingPerson.getAnnotation(FieldAnnotation.class).innerType());

        Field genericAttributes = cityobject.getClass().getDeclaredField("genericAttributes");
        assertEquals(SchemaManagerAdapter.ONTO_CITY_OBJECT_ID, genericAttributes.getAnnotation(FieldAnnotation.class).value());
        assertTrue(genericAttributes.getAnnotation(FieldAnnotation.class).backward());
        assertEquals(SchemaManagerAdapter.GENERIC_ATTRIB_GARPH + "/", genericAttributes.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(GenericAttribute.class, genericAttributes.getAnnotation(FieldAnnotation.class).innerType());

        Field externalReferences = cityobject.getClass().getDeclaredField("externalReferences");
        assertEquals(SchemaManagerAdapter.ONTO_CITY_OBJECT_ID, externalReferences.getAnnotation(FieldAnnotation.class).value());
        assertTrue(externalReferences.getAnnotation(FieldAnnotation.class).backward());
        assertEquals(SchemaManagerAdapter.EXTERNAL_REFERENCES_GRAPH + "/", externalReferences.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(ExternalReference.class, externalReferences.getAnnotation(FieldAnnotation.class).innerType());

    }

    @Test
    public void testNewCityObjectMethods() throws NoSuchMethodException {

        CityObject cityobject = new CityObject();

        //Test for Getter and Setter methods
        assertNotNull(cityobject.getClass().getDeclaredMethod("getCreationDate"));
        assertNotNull(cityobject.getClass().getDeclaredMethod("setCreationDate", String.class));
        assertNotNull(cityobject.getClass().getDeclaredMethod("getDescription"));
        assertNotNull(cityobject.getClass().getDeclaredMethod("setDescription", String.class));
        assertNotNull(cityobject.getClass().getDeclaredMethod("getEnvelopeType"));
        assertNotNull(cityobject.getClass().getDeclaredMethod("setEnvelopeType", EnvelopeType.class));
        assertNotNull(cityobject.getClass().getDeclaredMethod("getGmlId"));
        assertNotNull(cityobject.getClass().getDeclaredMethod("setGmlId", String.class));
        assertNotNull(cityobject.getClass().getDeclaredMethod("getLastModificationDate"));
        assertNotNull(cityobject.getClass().getDeclaredMethod("setLastModificationDate", String.class));
        assertNotNull(cityobject.getClass().getDeclaredMethod("getLineage"));
        assertNotNull(cityobject.getClass().getDeclaredMethod("setLineage", String.class));
        assertNotNull(cityobject.getClass().getDeclaredMethod("getName"));
        assertNotNull(cityobject.getClass().getDeclaredMethod("setName", String.class));
        assertNotNull(cityobject.getClass().getDeclaredMethod("getNameCodespace"));
        assertNotNull(cityobject.getClass().getDeclaredMethod("setNameCodespace", String.class));
        assertNotNull(cityobject.getClass().getDeclaredMethod("getObjectClassId"));
        assertNotNull(cityobject.getClass().getDeclaredMethod("setObjectClassId", BigInteger.class));
        assertNotNull(cityobject.getClass().getDeclaredMethod("getReasonForUpdate"));
        assertNotNull(cityobject.getClass().getDeclaredMethod("setReasonForUpdate", String.class));
        assertNotNull(cityobject.getClass().getDeclaredMethod("getRelativeToTerrain"));
        assertNotNull(cityobject.getClass().getDeclaredMethod("setRelativeToTerrain", String.class));
        assertNotNull(cityobject.getClass().getDeclaredMethod("getRelativeToWater"));
        assertNotNull(cityobject.getClass().getDeclaredMethod("setRelativeToWater", String.class));
        assertNotNull(cityobject.getClass().getDeclaredMethod("getTerminationDate"));
        assertNotNull(cityobject.getClass().getDeclaredMethod("setTerminationDate", String.class));
        assertNotNull(cityobject.getClass().getDeclaredMethod("getUpdatingPerson"));
        assertNotNull(cityobject.getClass().getDeclaredMethod("setUpdatingPerson", String.class));
        assertNotNull(cityobject.getClass().getDeclaredMethod("getGenericAttributes"));
        assertNotNull(cityobject.getClass().getDeclaredMethod("setGenericAttributes", ArrayList.class));
        assertNotNull(cityobject.getClass().getDeclaredMethod("getExternalReferences"));
        assertNotNull(cityobject.getClass().getDeclaredMethod("setExternalReferences", ArrayList.class));
    }
}
