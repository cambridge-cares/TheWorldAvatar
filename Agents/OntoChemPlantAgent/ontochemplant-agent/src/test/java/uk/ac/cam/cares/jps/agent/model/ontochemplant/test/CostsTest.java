package uk.ac.cam.cares.jps.agent.model.ontochemplant.test;

import uk.ac.cam.cares.jps.agent.model.ontochemplant.Costs;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import java.lang.reflect.Field;

import org.junit.Test;

public class CostsTest {
	
	@Test
	public void testCosts() throws NoSuchFieldException, IllegalAccessException {
	Costs testcosts = new Costs();
	
	//Test field Annotations
    Field cost = testcosts.getClass().getDeclaredField("Costs_USD");
    assertEquals("http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue", cost.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Model.class, cost.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(cost.getAnnotation(FieldAnnotation.class).backward());
    	
	}

}
