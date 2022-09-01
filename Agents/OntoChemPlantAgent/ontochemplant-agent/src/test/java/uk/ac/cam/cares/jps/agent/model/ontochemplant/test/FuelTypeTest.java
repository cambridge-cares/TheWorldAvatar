package uk.ac.cam.cares.jps.agent.model.ontochemplant.test;

import uk.ac.cam.cares.jps.agent.model.ontochemplant.CarbonEmissionIndex;
import uk.ac.cam.cares.jps.agent.model.ontochemplant.FuelType;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import java.lang.reflect.Field;

import org.junit.Test;

public class FuelTypeTest {
	
	@Test
	public void testFuelType() throws NoSuchFieldException, IllegalAccessException {
	FuelType testfueltype = new FuelType();
	
	//Test field Annotations
    Field fuel = testfueltype.getClass().getDeclaredField("hasCarbonEmissionIndex");
    assertEquals("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasCarbonEmissionIndex", fuel.getAnnotation(FieldAnnotation.class).value());
    assertEquals(CarbonEmissionIndex.class, fuel.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(fuel.getAnnotation(FieldAnnotation.class).backward());
    	
	}

}
