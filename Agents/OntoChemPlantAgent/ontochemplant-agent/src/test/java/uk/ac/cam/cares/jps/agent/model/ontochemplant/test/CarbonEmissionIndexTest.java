package uk.ac.cam.cares.jps.agent.model.ontochemplant.test;

import uk.ac.cam.cares.jps.agent.model.ontochemplant.CarbonEmissionIndex;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import java.lang.reflect.Field;

import org.junit.Test;

public class CarbonEmissionIndexTest {
	
	@Test
	public void testCarbonEmissionIndex() throws NoSuchFieldException, IllegalAccessException {
		
		CarbonEmissionIndex testCEI = new CarbonEmissionIndex();
		
		//Test field Annotations
	    Field cei = testCEI.getClass().getDeclaredField("CarbonEmissionIndex");
	    assertEquals("http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue", cei.getAnnotation(FieldAnnotation.class).value());
	    assertEquals(Model.class, cei.getAnnotation(FieldAnnotation.class).innerType());   
	    assertFalse(cei.getAnnotation(FieldAnnotation.class).backward());

	}

}
