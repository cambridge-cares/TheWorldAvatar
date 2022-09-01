package uk.ac.cam.cares.jps.agent.model.ontochemplant.test;

import uk.ac.cam.cares.jps.agent.model.ontochemplant.StorageCapacity;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import java.lang.reflect.Field;

import org.junit.Test;

public class StorageCapacityTest {
	
	@Test
	public void testStorageCapacity() throws NoSuchFieldException, IllegalAccessException {
	StorageCapacity teststorage = new StorageCapacity();
	
	//Test field Annotations
    Field storage = teststorage.getClass().getDeclaredField("StorageCapacity_MetricTon");
    assertEquals("http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue", storage.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Model.class, storage.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(storage.getAnnotation(FieldAnnotation.class).backward());
        
	}

}
