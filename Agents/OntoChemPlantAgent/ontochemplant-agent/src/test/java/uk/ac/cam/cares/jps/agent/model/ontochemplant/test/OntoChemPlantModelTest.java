package uk.ac.cam.cares.jps.agent.model.ontochemplant.test;

import uk.ac.cam.cares.jps.agent.model.ontochemplant.OntoChemPlantModel;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Field;

import org.junit.Test;

public class OntoChemPlantModelTest {
	
	@Test
	public void testOntoChemPlantModel() throws NoSuchFieldException, IllegalAccessException {
	OntoChemPlantModel testOCPmodel = new OntoChemPlantModel();
	
	//Test field Annotations
    Field model = testOCPmodel.getClass().getDeclaredField("OntoCityGMLRepresentationOf");
    assertEquals("https://www.theworldavatar.com/kg/ontobuiltenv/hasOntoCityGMLRepresentation", model.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Model.class, model.getAnnotation(FieldAnnotation.class).innerType());   
    assertTrue(model.getAnnotation(FieldAnnotation.class).backward());

    	
	}

}
