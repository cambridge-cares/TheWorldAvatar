package uk.ac.cam.cares.jps.agent.model.ontochemplant.test;

import uk.ac.cam.cares.jps.agent.model.ontochemplant.Address;
import uk.ac.cam.cares.jps.agent.model.ontochemplant.OntoChemPlantModel;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;

import java.lang.reflect.Field;
import java.net.URI;

import org.junit.Test;

import static org.junit.Assert.*;

public class OntoChemPlantModelTest {
	
	@Test
	public void testOntoChemPlantModel() throws NoSuchFieldException, IllegalAccessException {
	OntoChemPlantModel testOCPmodel = new OntoChemPlantModel();
	
	//Test field Annotations
    Field model = testOCPmodel.getClass().getDeclaredField("OntoCityGMLRepresentationOf");
    assertEquals("https://www.theworldavatar.com/kg/ontobuiltenv/hasOntoCityGMLRepresentation", model.getAnnotation(FieldAnnotation.class).value());
    assertEquals(URI.class, model.getAnnotation(FieldAnnotation.class).innerType());
    assertTrue(model.getAnnotation(FieldAnnotation.class).backward());
	}

}
