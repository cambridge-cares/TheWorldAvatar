package uk.ac.cam.cares.jps.agent.model.ontochemplant.test;

import uk.ac.cam.cares.jps.agent.model.ontochemplant.Company;
import uk.ac.cam.cares.jps.agent.model.ontochemplant.PowerConsumption;
import uk.ac.cam.cares.jps.agent.model.ontochemplant.Revenue;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import java.lang.reflect.Field;

import org.junit.Test;

public class CompanyTest {
	
	@Test
	public void testCompany() throws NoSuchFieldException, IllegalAccessException {
	Company testcompany = new Company();
	
	//Test field Annotations
    Field year = testcompany.getClass().getDeclaredField("hasYearOfEstablishment");
    assertEquals("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasYearOfEstablishment", year.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Model.class, year.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(year.getAnnotation(FieldAnnotation.class).backward());
    
    Field sectoral = testcompany.getClass().getDeclaredField("hasSectoralDescription");
    assertEquals("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasSectoralDescription", sectoral.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Model.class, sectoral.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(sectoral.getAnnotation(FieldAnnotation.class).backward());
    
    Field ssic = testcompany.getClass().getDeclaredField("hasSSICCode");
    assertEquals("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasSSICCode", ssic.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Model.class, ssic.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(ssic.getAnnotation(FieldAnnotation.class).backward());
    
    Field employees = testcompany.getClass().getDeclaredField("hasNumberofEmployees");
    assertEquals("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasNumberofEmployees", employees.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Model.class, employees.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(employees.getAnnotation(FieldAnnotation.class).backward());
    
    Field businessact = testcompany.getClass().getDeclaredField("hasBusinessActivity");
    assertEquals("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasBusinessActivity", businessact.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Model.class, businessact.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(businessact.getAnnotation(FieldAnnotation.class).backward());
    
    Field prodtech = testcompany.getClass().getDeclaredField("hasProductionTechnology");
    assertEquals("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasProductionTechnology", prodtech.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Model.class, prodtech.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(prodtech.getAnnotation(FieldAnnotation.class).backward());
    
    Field revenue = testcompany.getClass().getDeclaredField("hasRevenue");
    assertEquals("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_performance/economic_performance.owl#hasRevenue", revenue.getAnnotation(FieldAnnotation.class).value());
    assertEquals(Revenue.class, revenue.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(revenue.getAnnotation(FieldAnnotation.class).backward());
    
    Field powerconsump = testcompany.getClass().getDeclaredField("hasPowerConsumption");
    assertEquals("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasPowerConsumption", powerconsump.getAnnotation(FieldAnnotation.class).value());
    assertEquals(PowerConsumption.class, powerconsump.getAnnotation(FieldAnnotation.class).innerType());   
    assertFalse(powerconsump.getAnnotation(FieldAnnotation.class).backward());
    	
	}

}
