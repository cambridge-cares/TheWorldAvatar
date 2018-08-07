package uk.ac.cam.cares.jps.composition.test;

import java.io.IOException;
import java.net.URISyntaxException;

import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Test;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;

import uk.ac.cam.cares.jps.composition.servicemodel.Service;
import uk.ac.cam.cares.jps.composition.util.FormatTranslator;

public class UtilTest {

	@Test
	public void test() throws JSONException, JsonParseException, JsonMappingException, URISyntaxException, IOException {
		String agentInString = "{\"uri\":\"http://www.theworldavatar.com/Composite_Service_ebtckPt\",\n"
				+ "\"operations\":[{\"uri\":\"http://www.theworldavatar.com/Operation_5x2dBp8\",\"inputs\":[{\"uri\":\"http://www.theworldavatar.com/MessageContent_Input_elRIItx\",\"mandatoryParts\":[{\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_EUvsewL\",\"modelReference\":\"http://www.theworldavatar.com/OntoEIP/supporting_concepts/space_and_time_v1.owl#City\",\"value\":\"http://dbpedia.org/resource/Singapore\",\"datatypeValue\":\"xsd:anyURI\"}]}],\"httpUrl\":\"http://www.theworldavatar.com/\"}]}\n"
				+ "\n";
		new JSONObject(agentInString);
		// System.out.println(agentInString);
		Service agent = FormatTranslator.convertJSONTOJavaClass(agentInString);
		System.out.println(agent.getAllOutputs());
	}

}
