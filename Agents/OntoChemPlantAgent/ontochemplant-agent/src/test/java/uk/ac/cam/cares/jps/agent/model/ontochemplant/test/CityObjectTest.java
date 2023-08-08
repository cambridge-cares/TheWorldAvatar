package uk.ac.cam.cares.jps.agent.model.ontochemplant.test;

import uk.ac.cam.cares.jps.agent.model.ontochemplant.CityObject;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import java.lang.reflect.Field;

import org.junit.Test;

public class CityObjectTest {
	
	@Test
	public void testCityObject() throws NoSuchFieldException, IllegalAccessException {
	CityObject testcityobj = new CityObject();
	
	//Test field Annotations
    Field objclassid = testcityobj.getClass().getDeclaredField("objectClassId");
    assertEquals("http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#objectClassId", objclassid.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Model.class, objclassid.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(objclassid.getAnnotation(FieldAnnotation.class).backward());
    	
	}

}
