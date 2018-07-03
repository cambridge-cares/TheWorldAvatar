package uk.ac.cam.cares.jps.composition.test;

import static org.junit.Assert.*;

import java.io.IOException;
import java.net.URISyntaxException;

import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Test;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;

import uk.ac.cam.cares.jps.composition.Ontology.ServiceWriter;
import uk.ac.cam.cares.jps.composition.ServiceModel.Service;
import uk.ac.cam.cares.jps.composition.util.FormatTranslator;

public class ServiceWriterTest {

	@Test
	public void test() throws JsonParseException, JsonMappingException, URISyntaxException, IOException, JSONException {

		String agentInString = "{\"operations\":[{\"outputs\":[{\"optionalParts\":[],\"mandatoryParts\":[{\"modelReference\":\"http://www.theworldavatar.com/VisualizationOfConcentrationWithBuildings\",\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_FEfLW62\",\"value\":\"\",\"datatypeValue\":\"xsd:anyURI\"}],\"uri\":\"http://www.theworldavatar.com/MessageContent_Output_mgxnLhA\"}],\"inputs\":[{\"optionalParts\":[],\"mandatoryParts\":[{\"modelReference\":\"http://www.theworldavatar.com/OntoCAPE/OntoCAPE/chemical_process_system/CPS_realization/plant.owl#Plant\",\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_Mk0A9hC\",\"value\":\"http://www.theworldavatar.com/Plant-001.owl\",\"datatypeValue\":\"xsd:anyURI\"},{\"modelReference\":\"http://www.theworldavatar.com/OntoEIP/OntoCAPE/OntoCAPE/upper_level/coordinate_system.owl#Coordinate\",\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_sS5WPD7\",\"value\":\"\",\"datatypeValue\":\"xsd:anyURI\"}],\"uri\":\"http://www.theworldavatar.com/MessageContent_Input_5mEIdC4\"}],\"httpUrl\":\"http://www.theworldavatar.com/\",\"uri\":\"http://www.theworldavatar.com/Operation_pu7p56P\"}],\"uri\":\"http://www.theworldavatar.com/Composite_Service_oa0i4Ej\"}\n";
		Service agent = FormatTranslator.convertJSONTOJavaClass(agentInString);
		ServiceWriter writer = new ServiceWriter();
		writer.generateModel(agent);
	}

}
