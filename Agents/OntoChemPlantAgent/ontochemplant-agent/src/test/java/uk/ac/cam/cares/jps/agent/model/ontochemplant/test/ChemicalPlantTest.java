package uk.ac.cam.cares.jps.agent.model.ontochemplant.test;

import uk.ac.cam.cares.jps.agent.model.ontochemplant.ChemicalPlant;
import uk.ac.cam.cares.jps.agent.model.ontochemplant.Company;
import uk.ac.cam.cares.jps.agent.model.ontochemplant.DesignCapacity;
import uk.ac.cam.cares.jps.agent.model.ontochemplant.FuelType;
import uk.ac.cam.cares.jps.agent.model.ontochemplant.PlantCO2Emission;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Field;

import org.junit.Test;

public class ChemicalPlantTest {
	
	@Test
	public void testChemicalPlant() throws NoSuchFieldException, IllegalAccessException {
	ChemicalPlant testchemplant = new ChemicalPlant();
	
	//Test field Annotations
    Field thermalEffi = testchemplant.getClass().getDeclaredField("hasThermalEfficiency");
    assertEquals("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasThermalEfficiency", thermalEffi.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Model.class, thermalEffi.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(thermalEffi.getAnnotation(FieldAnnotation.class).backward());
    
    Field fueltype = testchemplant.getClass().getDeclaredField("hasFuelType");
    assertEquals("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasFuelType", fueltype.getAnnotation(FieldAnnotation.class).value());
    assertEquals(FuelType.class, fueltype.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(fueltype.getAnnotation(FieldAnnotation.class).backward());
    
    Field CO2 = testchemplant.getClass().getDeclaredField("hasPlantCO2Emission");
    assertEquals("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasPlantCO2Emission", CO2.getAnnotation(FieldAnnotation.class).value());
    assertEquals(PlantCO2Emission.class, CO2.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(CO2.getAnnotation(FieldAnnotation.class).backward());
    
    Field designcapacity = testchemplant.getClass().getDeclaredField("hasDesignCapacity");
    assertEquals("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasDesignCapacity", designcapacity.getAnnotation(FieldAnnotation.class).value());
    assertEquals(DesignCapacity.class, designcapacity.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(designcapacity.getAnnotation(FieldAnnotation.class).backward());
    
    Field owner = testchemplant.getClass().getDeclaredField("Owner");
    assertEquals("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isOwnerOf", owner.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Company.class, owner.getAnnotation(FieldAnnotation.class).innerType());   
    assertTrue(owner.getAnnotation(FieldAnnotation.class).backward());
    	
	}

}
