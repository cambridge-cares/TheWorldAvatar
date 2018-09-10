package uk.ac.cam.cares.jps.composition.test;

import org.json.JSONObject;
import org.junit.Test;

import uk.ac.cam.cares.jps.composition.compositionengine.ServiceCompositionEngine;
import uk.ac.cam.cares.jps.composition.enginemodel.Layer;
import uk.ac.cam.cares.jps.composition.servicemodel.Service;
import uk.ac.cam.cares.jps.composition.util.ConnectionBuilder;
import uk.ac.cam.cares.jps.composition.util.FormatTranslator;

public class ServiceCompositionEngineTest {

	@Test
	public void test() throws Exception {
//		String agentInString = "{\"uri\":\"http://www.theworldavatar.com/Composite_Service_oa0i4Ej\",\"operations\":[{\"uri\":\"http://www.theworldavatar.com/Operation_pu7p56P\",\"inputs\":[{\"uri\":\"http://www.theworldavatar.com/MessageContent_Input_5mEIdC4\",\"mandatoryParts\":[{\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_Mk0A9hC\",\"value\":\"\",\"datatypeValue\":\"xsd:anyURI\",\"modelReference\":\"http://www.theworldavatar.com/OntoCAPE/OntoCAPE/chemical_process_system/CPS_realization/plant.owl#Plant\"},{\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_sS5WPD7\",\"value\":\"\",\"datatypeValue\":\"xsd:anyURI\",\"modelReference\":\"http://www.theworldavatar.com/OntoEIP/OntoCAPE/OntoCAPE/upper_level/coordinate_system.owl#Coordinate\"}],\"optionalParts\":[]}],\"outputs\":[{\"uri\":\"http://www.theworldavatar.com/MessageContent_Output_mgxnLhA\",\"mandatoryParts\":[{\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_FEfLW62\",\"value\":\"\",\"datatypeValue\":\"xsd:anyURI\",\"modelReference\":\"http://www.theworldavatar.com/VisualizationOfConcentrationWithBuildings\"}],\"optionalParts\":[]}],\"httpUrl\":\"http://www.theworldavatar.com/\"}]}";
	
		String agentInString = "{\"uri\":\"http://www.theworldavatar.com/Composite_Service_eR1fQwC\",\"operations\":[{\"uri\":\"http://www.theworldavatar.com/Operation_pexDwAC\",\"inputs\":[{\"uri\":\"http://www.theworldavatar.com/MessageContent_Input_xzbAvBW\",\"mandatoryParts\":[{\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_CghedAK\",\"value\":\"\",\"datatypeValue\":\"\",\"modelReference\":\"http://test.com/ontology/Region\"}],\"optionalParts\":[]}],\"outputs\":[{\"uri\":\"http://www.theworldavatar.com/MessageContent_Output_18YRk5SC\",\"mandatoryParts\":[{\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_15wGxcwo\",\"value\":\"\",\"datatypeValue\":\"\",\"modelReference\":\"http://test.com/ontology/ADMSSimulation\"}],\"optionalParts\":[]},{\"uri\":\"http://www.theworldavatar.com/MessageContent_Output_2tQIEfW\",\"mandatoryParts\":[{\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_h4hbVfA\",\"value\":\"\",\"datatypeValue\":\"\",\"modelReference\":\"http://test.com/ontology/BuildingList\"}],\"optionalParts\":[]}],\"httpUrl\":\"http://www.theworldavatar.com/JPS_COMPOSITION/CoordinateToWeather\"}]}";
		Service agent = FormatTranslator.convertJSONTOJavaClass(agentInString);
		ServiceCompositionEngine engine = new ServiceCompositionEngine(agent, "http://localhost:8080");
		
		
		System.out.println(agent.getAllOutputs());
		
		
		boolean met = false;
		int index = 0;
		while (!met) {
			index++;
			met = engine.appendLayerToGraph(index);
			System.out.println("You made this far 003");

		}
		System.out.println("You made this far 004");

		int size = 1;
		while (size != 0) {
			size = engine.eliminateRedundantAgent();
		}

		ConnectionBuilder connectionBuilder = new ConnectionBuilder();
		connectionBuilder.buildEdge(engine.getGraph()); // build the connection between services
		connectionBuilder.connectEdges(engine.getGraph());
		connectionBuilder.rearrangeEdges(engine.getGraph());

		JSONObject graphInJSON = FormatTranslator.convertGraphJavaClassTOJSON(engine.getGraph());

		
		System.out.println("success");
		System.out.println(graphInJSON.toString());
		System.out.println("success!");
		
	}

}
