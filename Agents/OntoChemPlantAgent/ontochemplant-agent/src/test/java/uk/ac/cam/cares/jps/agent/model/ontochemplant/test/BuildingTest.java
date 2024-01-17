package uk.ac.cam.cares.jps.agent.model.ontochemplant.test;

import uk.ac.cam.cares.jps.agent.model.ontochemplant.Address;
import uk.ac.cam.cares.jps.agent.model.ontochemplant.Building;
import uk.ac.cam.cares.jps.agent.model.ontochemplant.StorageCapacity;
import uk.ac.cam.cares.jps.agent.model.ontochemplant.UtilityCosts;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Field;

import org.junit.Test;

public class BuildingTest {
	
	@Test
	public void testBuilding() throws NoSuchFieldException, IllegalAccessException {
	Building testbuilding = new Building();
	
	//Test field Annotations
    Field address = testbuilding.getClass().getDeclaredField("address");
    assertEquals("http://ontology.eil.utoronto.ca/icontact.owl#hasAddress", address.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Address.class, address.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(address.getAnnotation(FieldAnnotation.class).backward());
    
    Field utilitycosts = testbuilding.getClass().getDeclaredField("utilitycosts");
    assertEquals("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasUtilityCost", utilitycosts.getAnnotation(FieldAnnotation.class).value());
    assertEquals(UtilityCosts.class, utilitycosts.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(utilitycosts.getAnnotation(FieldAnnotation.class).backward());
    
    Field buildingIRI = testbuilding.getClass().getDeclaredField("BuildingIRI");
    assertEquals("https://www.theworldavatar.com/kg/ontobuiltenv/hasOntoCityGMLRepresentation", buildingIRI.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Model.class, buildingIRI.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(buildingIRI.getAnnotation(FieldAnnotation.class).backward());
    
    Field ownedBy = testbuilding.getClass().getDeclaredField("OwnedBy");
    assertEquals("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isOwnerOf", ownedBy.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Model.class, ownedBy.getAnnotation(FieldAnnotation.class).innerType());   
    assertTrue(ownedBy.getAnnotation(FieldAnnotation.class).backward());
	
	}

}
