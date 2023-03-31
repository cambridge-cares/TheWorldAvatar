package uk.ac.cam.cares.jps.agent.model.ontochemplant.test;

import uk.ac.cam.cares.jps.agent.model.ontochemplant.GeneratedHeat;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import java.lang.reflect.Field;

import org.junit.Test;

public class GeneratedHeatTest {
	
	@Test
	public void testGeneratedHeat() throws NoSuchFieldException, IllegalAccessException {
	GeneratedHeat testheat = new GeneratedHeat();
	
	//Test field Annotations
    Field heat = testheat.getClass().getDeclaredField("GeneratedHeat_MegaWatt");
    assertEquals("http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue", heat.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Model.class, heat.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(heat.getAnnotation(FieldAnnotation.class).backward());
    	
	}

}
