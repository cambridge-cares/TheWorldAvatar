package uk.ac.cam.cares.jps.agent.model.ontochemplant.test;

import org.junit.Test;
import uk.ac.cam.cares.jps.agent.model.ontochemplant.StorageCapacity;
import uk.ac.cam.cares.jps.agent.model.ontochemplant.StorageTank;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;

import java.lang.reflect.Field;

import static org.junit.Assert.*;

public class StorageTankTest {
	
	@Test
	public void testStorageTank() throws NoSuchFieldException, IllegalAccessException {
	StorageTank teststorage = new StorageTank();
	
	//Test field Annotations
    Field capacity = teststorage.getClass().getDeclaredField("StorageCapacity");
    assertEquals("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasStorageCapacity", capacity.getAnnotation(FieldAnnotation.class).value());
    assertEquals(StorageCapacity.class, capacity.getAnnotation(FieldAnnotation.class).innerType());
    assertFalse(capacity.getAnnotation(FieldAnnotation.class).backward());

    Field stores = teststorage.getClass().getDeclaredField("Contents");
    assertEquals("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#stores", stores.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Model.class, stores.getAnnotation(FieldAnnotation.class).innerType());
    assertFalse(stores.getAnnotation(FieldAnnotation.class).backward());

    Field owner = teststorage.getClass().getDeclaredField("ownedBy");
    assertEquals("http://www.opengis.net/ont/geosparql#ehContains", owner.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Model.class, owner.getAnnotation(FieldAnnotation.class).innerType());
    assertTrue(owner.getAnnotation(FieldAnnotation.class).backward());
	}

}
