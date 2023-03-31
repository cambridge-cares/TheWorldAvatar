package uk.ac.cam.cares.jps.agent.ontochemplant.test;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Test;
import org.mockito.*;

import uk.ac.cam.cares.jps.agent.model.ontochemplant.Building;
import uk.ac.cam.cares.jps.agent.model.ontochemplant.CityObject;
import uk.ac.cam.cares.jps.agent.model.ontochemplant.OntoChemPlantModel;
import uk.ac.cam.cares.jps.agent.model.ontochemplant.PlantItem;
import uk.ac.cam.cares.jps.agent.ontochemplant.OntoChemPlantAgent;
import uk.ac.cam.cares.jps.agent.ontochemplant.OntoChemPlantAgentLauncher;
import uk.ac.cam.cares.ogm.models.ModelContext;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.*;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigInteger;

public class OntoChemPlantAgentTest {
	
	// Test createOntoChemPlantModel(JSONObject input)
	@Test
	public void createOntoChemPlantModelTest() throws NoSuchMethodException, Exception {
		
		OntoChemPlantAgent testAgent = new OntoChemPlantAgent();
		Method createOntoChemPlantModel = testAgent.getClass().getDeclaredMethod("createOntoChemPlantModel", JSONObject.class);
		JSONArray iri = new JSONArray();
		JSONObject input = new JSONObject();
		
		// Mock city object
        CityObject mockcity = mock(CityObject.class);
        ModelContext modelContext = mock(ModelContext.class);
        when(modelContext.createHollowModel(eq(CityObject.class), anyString())).thenReturn(new CityObject());
        when(modelContext.createHollowModel(eq(OntoChemPlantModel.class), anyString())).thenReturn(new OntoChemPlantModel());
		
		// Test plant item case
        try {
			  iri.put("http://www.theworldavatar.com:83/citieskg/namespace/jriEPSG24500/sparql/cityobject/UUID_bd07e1dd-7ffe-4776-8cf0-5409c007e437/");
			  input.put(OntoChemPlantAgentLauncher.IRI, iri);
	      	      
		      // Plant Item
		      when(mockcity.getObjectClassId()).thenReturn(BigInteger.valueOf(21));
			  when(modelContext.createHollowModel(eq(PlantItem.class), anyString())).thenReturn(new PlantItem());
			  
			  PlantItem mockplant = mock(PlantItem.class);
			  Mockito.doNothing().when(modelContext).pullPartial(eq(mockcity), anyString());
			  
			  OntoChemPlantModel mockOCP = mock(OntoChemPlantModel.class);
			  Mockito.doNothing().when(modelContext).pullAll(mockOCP);
			  Mockito.doNothing().when(modelContext).recursivePullAll(eq(mockplant), anyInt());


        } catch (Exception e) {
              fail();
        } finally {
        	  input.remove(OntoChemPlantAgentLauncher.IRI);
        }
        
        // Test Building case
        try {
			  iri.put("http://www.theworldavatar.com:83/citieskg/namespace/jriEPSG24500/sparql/cityobject/UUID_04186941-9931-4eae-8c29-12191d11d477/");
			  input.put(OntoChemPlantAgentLauncher.IRI, iri);

		      when(mockcity.getObjectClassId()).thenReturn(BigInteger.valueOf(26));
			  when(modelContext.createHollowModel(eq(Building.class), anyString())).thenReturn(new Building());
			  
			  Building mockbuilding = mock(Building.class);
			  Mockito.doNothing().when(modelContext).pullPartial(eq(mockcity), anyString());
			  
			  OntoChemPlantModel mockOCP = mock(OntoChemPlantModel.class);
			  Mockito.doNothing().when(modelContext).pullAll(mockOCP);
			  Mockito.doNothing().when(modelContext).recursivePullAll(eq(mockbuilding), anyInt());


      } catch (Exception e) {
            fail();
      } finally {
      	  input.remove(OntoChemPlantAgentLauncher.IRI);
      }

	}

}