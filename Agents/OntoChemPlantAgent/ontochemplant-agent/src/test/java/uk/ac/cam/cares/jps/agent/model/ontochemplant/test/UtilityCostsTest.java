package uk.ac.cam.cares.jps.agent.model.ontochemplant.test;

import uk.ac.cam.cares.jps.agent.model.ontochemplant.UtilityCosts;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import java.lang.reflect.Field;

import org.junit.Test;

public class UtilityCostsTest {
	
	@Test
	public void testUtilityCosts() throws NoSuchFieldException, IllegalAccessException {
	UtilityCosts testutilitycosts = new UtilityCosts();
	
	//Test field Annotations
    Field costs = testutilitycosts.getClass().getDeclaredField("UtilityCosts_USD");
    assertEquals("http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue", costs.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Model.class, costs.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(costs.getAnnotation(FieldAnnotation.class).backward());
        
	}

}
