package uk.ac.cam.cares.jps.agent.model.ontochemplant.test;

import uk.ac.cam.cares.jps.agent.model.ontochemplant.Address;
import uk.ac.cam.cares.jps.agent.model.ontochemplant.LandLot;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.lang.reflect.Field;

import org.junit.Test;

public class AddressTest {
	
	@Test
	public void testAddress() throws NoSuchFieldException, IllegalAccessException {
	Address testaddress = new Address();
	
	//Test field Annotations
    Field latitude = testaddress.getClass().getDeclaredField("LatitudeEPSG24500");
    assertEquals("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasLatitudeEPSG24500", latitude.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Model.class, latitude.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(latitude.getAnnotation(FieldAnnotation.class).backward());
    
    Field longitude = testaddress.getClass().getDeclaredField("LongitudeEPSG24500");
    assertEquals("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasLongitudeEPSG24500", longitude.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Model.class, longitude.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(longitude.getAnnotation(FieldAnnotation.class).backward());
    
    Field landlot = testaddress.getClass().getDeclaredField("LandLotDetails");
    assertEquals("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasLandLotDetails", landlot.getAnnotation(FieldAnnotation.class).value());
    assertEquals(LandLot.class, landlot.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(landlot.getAnnotation(FieldAnnotation.class).backward());

	}

}
