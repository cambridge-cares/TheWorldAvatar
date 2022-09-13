package uk.ac.cam.cares.jps.agent.model.ontochemplant.test;

import uk.ac.cam.cares.jps.agent.model.ontochemplant.DesignCapacity;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import java.lang.reflect.Field;

import org.junit.Test;

public class DesignCapacityTest {
	
	@Test
	public void testDesignCapacity() throws NoSuchFieldException, IllegalAccessException {
	DesignCapacity testdesigncap = new DesignCapacity();
	
	//Test field Annotations
    Field designcap = testdesigncap.getClass().getDeclaredField("DesignCapacity_TonsPerYear");
    assertEquals("http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue", designcap.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Model.class, designcap.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(designcap.getAnnotation(FieldAnnotation.class).backward());
    	
	}

}
