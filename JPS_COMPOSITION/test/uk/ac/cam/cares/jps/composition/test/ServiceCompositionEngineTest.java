package uk.ac.cam.cares.jps.composition.test;

import org.junit.Test;

import uk.ac.cam.cares.jps.composition.compositionengine.ServiceCompositionEngine;
import uk.ac.cam.cares.jps.composition.enginemodel.Layer;
import uk.ac.cam.cares.jps.composition.servicemodel.Service;
import uk.ac.cam.cares.jps.composition.util.FormatTranslator;

public class ServiceCompositionEngineTest {

	@Test
	public void test() throws Exception {
		String agentInString = "{\"uri\":\"http://www.theworldavatar.com/Composite_Service_oa0i4Ej\",\"operations\":[{\"uri\":\"http://www.theworldavatar.com/Operation_pu7p56P\",\"inputs\":[{\"uri\":\"http://www.theworldavatar.com/MessageContent_Input_5mEIdC4\",\"mandatoryParts\":[{\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_Mk0A9hC\",\"value\":\"\",\"datatypeValue\":\"xsd:anyURI\",\"modelReference\":\"http://www.theworldavatar.com/OntoCAPE/OntoCAPE/chemical_process_system/CPS_realization/plant.owl#Plant\"},{\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_sS5WPD7\",\"value\":\"\",\"datatypeValue\":\"xsd:anyURI\",\"modelReference\":\"http://www.theworldavatar.com/OntoEIP/OntoCAPE/OntoCAPE/upper_level/coordinate_system.owl#Coordinate\"}],\"optionalParts\":[]}],\"outputs\":[{\"uri\":\"http://www.theworldavatar.com/MessageContent_Output_mgxnLhA\",\"mandatoryParts\":[{\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_FEfLW62\",\"value\":\"\",\"datatypeValue\":\"xsd:anyURI\",\"modelReference\":\"http://www.theworldavatar.com/VisualizationOfConcentrationWithBuildings\"}],\"optionalParts\":[]}],\"httpUrl\":\"http://www.theworldavatar.com/\"}]}";
		Service agent = FormatTranslator.convertJSONTOJavaClass(agentInString);
		ServiceCompositionEngine engine = new ServiceCompositionEngine(agent, "http://localhost:8080");
		boolean met = false;
		int index = 0;
		while (!met) {
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

	}

}
