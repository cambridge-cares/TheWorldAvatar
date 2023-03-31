package uk.ac.cam.cares.jps.agent.model.ontochemplant.test;

import uk.ac.cam.cares.jps.agent.model.ontochemplant.IndividualCO2Emission;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import java.lang.reflect.Field;

import org.junit.Test;

public class IndividualCO2EmissionTest {
	
	@Test
	public void testCO2Emission() throws NoSuchFieldException, IllegalAccessException {
	IndividualCO2Emission testCO2 = new IndividualCO2Emission();
	
	//Test field Annotations
    Field CO2 = testCO2.getClass().getDeclaredField("CO2Emission_TonsPerYear");
    assertEquals("http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue", CO2.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Model.class, CO2.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(CO2.getAnnotation(FieldAnnotation.class).backward());
    	
	}

}
