package uk.ac.cam.cares.jps.agent.model.ontochemplant.test;

import uk.ac.cam.cares.jps.agent.model.ontochemplant.Costs;
import uk.ac.cam.cares.jps.agent.model.ontochemplant.GeneratedHeat;
import uk.ac.cam.cares.jps.agent.model.ontochemplant.IndividualCO2Emission;
import uk.ac.cam.cares.jps.agent.model.ontochemplant.PlantItem;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Field;

import org.junit.Test;

public class PlantItemTest {
	
	@Test
	public void testPlantItem() throws NoSuchFieldException, IllegalAccessException {
	PlantItem testPlantitem = new PlantItem();
	
	//Test field Annotations
    Field CO2 = testPlantitem.getClass().getDeclaredField("IndividualCO2Emission");
    assertEquals("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasIndividualCO2Emission", CO2.getAnnotation(FieldAnnotation.class).value());
    assertEquals(IndividualCO2Emission.class, CO2.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(CO2.getAnnotation(FieldAnnotation.class).backward());
    
    Field heat = testPlantitem.getClass().getDeclaredField("GeneratedHeat");
    assertEquals("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasGeneratedHeat", heat.getAnnotation(FieldAnnotation.class).value());
    assertEquals(GeneratedHeat.class, heat.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(heat.getAnnotation(FieldAnnotation.class).backward());
    
    Field IRI = testPlantitem.getClass().getDeclaredField("CityFurnitureIRI");
    assertEquals("https://www.theworldavatar.com/kg/ontobuiltenv/hasOntoCityGMLRepresentation", IRI.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Model.class, IRI.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(IRI.getAnnotation(FieldAnnotation.class).backward());
  	
    Field ownedby = testPlantitem.getClass().getDeclaredField("OwnedBy");
    assertEquals("http://www.opengis.net/ont/geosparql#ehContains", ownedby.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Model.class, ownedby.getAnnotation(FieldAnnotation.class).innerType());   
    assertTrue(ownedby.getAnnotation(FieldAnnotation.class).backward());
    
    Field costs = testPlantitem.getClass().getDeclaredField("Costs");
    assertEquals("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_performance/economic_performance.owl#hasCost", costs.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Costs.class, costs.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(costs.getAnnotation(FieldAnnotation.class).backward());
    
	}

}
