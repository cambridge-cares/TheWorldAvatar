package uk.ac.cam.cares.jps.agent.model.ontochemplant.test;

import uk.ac.cam.cares.jps.agent.model.ontochemplant.PowerConsumption;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import java.lang.reflect.Field;

import org.junit.Test;

public class PowerConsumptionTest {
	
	@Test
	public void testPowerConsumption() throws NoSuchFieldException, IllegalAccessException {
	PowerConsumption testpowercon = new PowerConsumption();
	
	//Test field Annotations
    Field power = testpowercon.getClass().getDeclaredField("PowerConsumption_GigaJoulePerYear");
    assertEquals("http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue", power.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Model.class, power.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(power.getAnnotation(FieldAnnotation.class).backward());
        
	}

}
