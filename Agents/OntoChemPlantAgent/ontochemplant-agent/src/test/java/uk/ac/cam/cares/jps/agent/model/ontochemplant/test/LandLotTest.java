package uk.ac.cam.cares.jps.agent.model.ontochemplant.test;

import uk.ac.cam.cares.jps.agent.model.ontochemplant.LandLot;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import java.lang.reflect.Field;

import org.junit.Test;

public class LandLotTest {
	
	@Test
	public void testLandLot() throws NoSuchFieldException, IllegalAccessException {
	LandLot testlandlot = new LandLot();
	
	//Test field Annotations
    Field landlot = testlandlot.getClass().getDeclaredField("LandLotNumber");
    assertEquals("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasLandlotNumber", landlot.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Model.class, landlot.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(landlot.getAnnotation(FieldAnnotation.class).backward());
    
    Field lotarea = testlandlot.getClass().getDeclaredField("LotArea");
    assertEquals("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasLotArea", lotarea.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Model.class, lotarea.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(lotarea.getAnnotation(FieldAnnotation.class).backward());
    	
	}

}
