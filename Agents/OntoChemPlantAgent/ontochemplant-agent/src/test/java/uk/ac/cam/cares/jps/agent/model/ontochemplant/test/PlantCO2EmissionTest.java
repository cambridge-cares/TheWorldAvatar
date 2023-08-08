package uk.ac.cam.cares.jps.agent.model.ontochemplant.test;

import uk.ac.cam.cares.jps.agent.model.ontochemplant.PlantCO2Emission;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import java.lang.reflect.Field;

import org.junit.Test;

public class PlantCO2EmissionTest {
	
	@Test
	public void testPlantCO2Emission() throws NoSuchFieldException, IllegalAccessException {
	PlantCO2Emission testPlantCO2 = new PlantCO2Emission();
	
	//Test field Annotations
    Field CO2 = testPlantCO2.getClass().getDeclaredField("PlantCO2Emission_TonsPerYear");
    assertEquals("http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue", CO2.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Model.class, CO2.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(CO2.getAnnotation(FieldAnnotation.class).backward());
  	
	}

}
