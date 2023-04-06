package uk.ac.cam.cares.jps.agent.model.ontochemplant.test;

import uk.ac.cam.cares.jps.agent.model.ontochemplant.Revenue;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import java.lang.reflect.Field;

import org.junit.Test;

public class RevenueTest {
	
	@Test
	public void testRevenue() throws NoSuchFieldException, IllegalAccessException {
	Revenue testrevenue = new Revenue();
	
	//Test field Annotations
    Field revenue = testrevenue.getClass().getDeclaredField("Revenue_USD");
    assertEquals("http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue", revenue.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Model.class, revenue.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(revenue.getAnnotation(FieldAnnotation.class).backward());
        
	}

}
