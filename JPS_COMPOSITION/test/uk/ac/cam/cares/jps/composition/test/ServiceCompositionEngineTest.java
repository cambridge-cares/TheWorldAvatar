package uk.ac.cam.cares.jps.composition.test;

import static org.junit.Assert.*;

import java.io.IOException;
import java.net.URISyntaxException;

import org.json.JSONException;
import org.junit.Test;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;

import uk.ac.cam.cares.jps.composition.CompositionEngine.ServiceCompositionEngine;
import uk.ac.cam.cares.jps.composition.EngineModel.Layer;
import uk.ac.cam.cares.jps.composition.ServiceModel.Service;
import uk.ac.cam.cares.jps.composition.util.*;

public class ServiceCompositionEngineTest {

	@Test
	public void test() throws JsonParseException, JsonMappingException, URISyntaxException, IOException, JSONException {
		String agentInString = "{\"uri\":\"http://www.theworldavatar.com/Composite_Service_oa0i4Ej\",\"operations\":[{\"uri\":\"http://www.theworldavatar.com/Operation_pu7p56P\",\"inputs\":[{\"uri\":\"http://www.theworldavatar.com/MessageContent_Input_5mEIdC4\",\"mandatoryParts\":[{\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_Mk0A9hC\",\"value\":\"\",\"datatypeValue\":\"xsd:anyURI\",\"modelReference\":\"http://www.theworldavatar.com/OntoCAPE/OntoCAPE/chemical_process_system/CPS_realization/plant.owl#Plant\"},{\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_sS5WPD7\",\"value\":\"\",\"datatypeValue\":\"xsd:anyURI\",\"modelReference\":\"http://www.theworldavatar.com/OntoEIP/OntoCAPE/OntoCAPE/upper_level/coordinate_system.owl#Coordinate\"}],\"optionalParts\":[]}],\"outputs\":[{\"uri\":\"http://www.theworldavatar.com/MessageContent_Output_mgxnLhA\",\"mandatoryParts\":[{\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_FEfLW62\",\"value\":\"\",\"datatypeValue\":\"xsd:anyURI\",\"modelReference\":\"http://www.theworldavatar.com/VisualizationOfConcentrationWithBuildings\"}],\"optionalParts\":[]}],\"httpUrl\":\"http://www.theworldavatar.com/\"}]}";
		Service agent = FormatTranslator.convertJSONTOJavaClass(agentInString);
		ServiceCompositionEngine engine = new ServiceCompositionEngine(agent);
		boolean met = false;
		int index = 0;
		while(!met) {
			index++;
			met = engine.appendLayerToGraph(index);
		}
		for (Layer layer : engine.newGraph.layers) {
			System.out.println(layer.getServices());
		}
		engine.eliminateRedundantAgent();
		for (Layer layer : engine.newGraph.layers) {
			System.out.println(layer.getServices());
		}
		
		// now start to visualize the graph ... 
		// convert it to a 
	}

}
