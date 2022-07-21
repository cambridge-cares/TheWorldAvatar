package uk.ac.cam.cares.ogm.models.geo.test;

import org.citydb.database.adapter.blazegraph.SchemaManagerAdapter;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;
import uk.ac.cam.cares.ogm.models.ModelAnnotation;
import uk.ac.cam.cares.ogm.models.geo.GenericAttribute;

import java.lang.reflect.Field;
import java.math.BigInteger;
import java.net.URI;

import static org.junit.jupiter.api.Assertions.*;

public class GenericAttributeTest {

    @Test
    public void testNewGenericAttributeAnnotations() throws NoSuchFieldException {

        GenericAttribute genericAttribute = new GenericAttribute();

        assertEquals(SchemaManagerAdapter.GENERIC_ATTRIB_GARPH + "/", genericAttribute.getClass().getAnnotation(ModelAnnotation.class).defaultGraphName());

        Field attrName = genericAttribute.getClass().getDeclaredField("attrName");
        assertEquals(SchemaManagerAdapter.ONTO_ATTR_NAME, attrName.getAnnotation(FieldAnnotation.class).value());
        assertFalse(attrName.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", attrName.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, attrName.getAnnotation(FieldAnnotation.class).innerType());

        Field uriVal = genericAttribute.getClass().getDeclaredField("uriVal");
        assertEquals(SchemaManagerAdapter.ONTO_URI_VAL, uriVal.getAnnotation(FieldAnnotation.class).value());
        assertFalse(uriVal.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", uriVal.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, uriVal.getAnnotation(FieldAnnotation.class).innerType());

        Field strVal = genericAttribute.getClass().getDeclaredField("strVal");
        assertEquals(SchemaManagerAdapter.ONTO_STR_VAL, strVal.getAnnotation(FieldAnnotation.class).value());
        assertFalse(strVal.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", strVal.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, strVal.getAnnotation(FieldAnnotation.class).innerType());

        Field unit = genericAttribute.getClass().getDeclaredField("unit");
        assertEquals(SchemaManagerAdapter.ONTO_UNIT, unit.getAnnotation(FieldAnnotation.class).value());
        assertFalse(unit.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", unit.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, unit.getAnnotation(FieldAnnotation.class).innerType());

        Field rootGenattribId = genericAttribute.getClass().getDeclaredField("rootGenattribId");
        assertEquals(SchemaManagerAdapter.ONTO_ROOT_GENATTRIB_ID, rootGenattribId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(rootGenattribId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", rootGenattribId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, rootGenattribId.getAnnotation(FieldAnnotation.class).innerType());

        Field realVal = genericAttribute.getClass().getDeclaredField("realVal");
        assertEquals(SchemaManagerAdapter.ONTO_REAL_VAL, realVal.getAnnotation(FieldAnnotation.class).value());
        assertFalse(unit.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", realVal.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, realVal.getAnnotation(FieldAnnotation.class).innerType());

        Field parentGenattribId = genericAttribute.getClass().getDeclaredField("parentGenattribId");
        assertEquals(SchemaManagerAdapter.ONTO_PARRENT_GENATTRIB_ID, parentGenattribId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(parentGenattribId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", parentGenattribId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, parentGenattribId.getAnnotation(FieldAnnotation.class).innerType());

        Field intVal = genericAttribute.getClass().getDeclaredField("intVal");
        assertEquals(SchemaManagerAdapter.ONTO_INT_VAL, intVal.getAnnotation(FieldAnnotation.class).value());
        assertFalse(intVal.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", intVal.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, intVal.getAnnotation(FieldAnnotation.class).innerType());

        Field dateVal = genericAttribute.getClass().getDeclaredField("dateVal");
        assertEquals(SchemaManagerAdapter.ONTO_DATE_VAL, dateVal.getAnnotation(FieldAnnotation.class).value());
        assertFalse(dateVal.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", dateVal.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, dateVal.getAnnotation(FieldAnnotation.class).innerType());

        Field dataType = genericAttribute.getClass().getDeclaredField("dataType");
        assertEquals(SchemaManagerAdapter.ONTO_DATA_TYPE, dataType.getAnnotation(FieldAnnotation.class).value());
        assertFalse(dataType.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", dataType.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, dataType.getAnnotation(FieldAnnotation.class).innerType());

        Field cityObjectId = genericAttribute.getClass().getDeclaredField("cityObjectId");
        assertEquals(SchemaManagerAdapter.ONTO_CITY_OBJECT_ID, cityObjectId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(cityObjectId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", cityObjectId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, cityObjectId.getAnnotation(FieldAnnotation.class).innerType());
    }

    @Test
    public void testNewGenericAtrributeMethods() throws NoSuchMethodException {

        GenericAttribute genericAttribute = new GenericAttribute();

        assertNotNull(genericAttribute.getClass().getDeclaredMethod("getAttrName"));
        assertNotNull(genericAttribute.getClass().getDeclaredMethod("setAttrName", String.class));

        assertNotNull(genericAttribute.getClass().getDeclaredMethod("getUriVal"));
        assertNotNull(genericAttribute.getClass().getDeclaredMethod("setUriVal", String.class));

        assertNotNull(genericAttribute.getClass().getDeclaredMethod("getStrVal"));
        assertNotNull(genericAttribute.getClass().getDeclaredMethod("setStrVal", String.class));

        assertNotNull(genericAttribute.getClass().getDeclaredMethod("getUnit"));
        assertNotNull(genericAttribute.getClass().getDeclaredMethod("setUnit", String.class));

        assertNotNull(genericAttribute.getClass().getDeclaredMethod("getRootGenattribId"));
        assertNotNull(genericAttribute.getClass().getDeclaredMethod("setRootGenattribId", String.class));

        assertNotNull(genericAttribute.getClass().getDeclaredMethod("getRealVal"));
        assertNotNull(genericAttribute.getClass().getDeclaredMethod("setRealVal", String.class));

        assertNotNull(genericAttribute.getClass().getDeclaredMethod("getParentGenattribId"));
        assertNotNull(genericAttribute.getClass().getDeclaredMethod("setParentGenattribId", String.class));

        assertNotNull(genericAttribute.getClass().getDeclaredMethod("getIntVal"));
        assertNotNull(genericAttribute.getClass().getDeclaredMethod("setIntVal", String.class));

        assertNotNull(genericAttribute.getClass().getDeclaredMethod("getDateVal"));
        assertNotNull(genericAttribute.getClass().getDeclaredMethod("setDateVal", String.class));

        assertNotNull(genericAttribute.getClass().getDeclaredMethod("getDataType"));
        assertNotNull(genericAttribute.getClass().getDeclaredMethod("setDataType", BigInteger.class));

        assertNotNull(genericAttribute.getClass().getDeclaredMethod("getCityObjectId"));
        assertNotNull(genericAttribute.getClass().getDeclaredMethod("setCityObjectId", URI.class));
    }
}
