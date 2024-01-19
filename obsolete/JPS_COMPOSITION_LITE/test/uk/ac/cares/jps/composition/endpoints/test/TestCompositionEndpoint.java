package uk.ac.cares.jps.composition.endpoints.test;

import org.apache.http.client.utils.URIBuilder;
import org.junit.After;
import org.junit.Test;

import uk.ac.cares.jps.composition.utils.Request;

public class TestCompositionEndpoint {

	@After
	public void tearDown() throws Exception {

	}

	@Test
	public void test() {

		String agentInString = "{\r\n" + "  \"operations\": [\r\n"
				+ "    {\r\n" + "      \"outputs\": [\r\n"
				+ "        {\r\n" + "          \"optionalParts\": [],\r\n"
				+ "          \"mandatoryParts\": [\r\n"
				+ "            {\r\n"
				+ "              \"type\": \"https://www.w3.org/ns/csvw#Table\",\r\n"
				+ "              \"uri\": \"http://www.theworldavatar.com/Mandatory_MessagePart_15wGxcwo\",\r\n"
				+ "              \"value\": \"\",\r\n"
				+ "              \"datatypeValue\": \"\"\r\n"
				+ "            }\r\n" + "          ],\r\n"
				+ "          \"uri\": \"http://www.theworldavatar.com/MessageContent_Output_18YRk5SC\"\r\n"
				+ "        },\r\n" + "        {\r\n"
				+ "          \"optionalParts\": [],\r\n"
				+ "          \"mandatoryParts\": [\r\n"
				+ "            {\r\n"
				+ "              \"type\": \"http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#BuildingType\",\r\n"
				+ "              \"uri\": \"http://www.theworldavatar.com/Mandatory_MessagePart_guVoBmm\",\r\n"
				+ "              \"value\": \"\",\r\n"
				+ "              \"datatypeValue\": \"\"\r\n"
				+ "            }\r\n" + "          ],\r\n"
				+ "          \"uri\": \"http://www.theworldavatar.com/MessageContent_Output_1gpms0DA\"\r\n"
				+ "        }\r\n" + "      ],\r\n"
				+ "      \"inputs\": [\r\n" + "        {\r\n"
				+ "          \"optionalParts\": [],\r\n"
				+ "          \"mandatoryParts\": [\r\n"
				+ "            {\r\n"
				+ "              \"type\": \"http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType\",\r\n"
				+ "              \"uri\": \"http://www.theworldavatar.com/Mandatory_MessagePart_CghedAK\",\r\n"
				+ "              \"value\": \"\",\r\n"
				+ "              \"datatypeValue\": \"\"\r\n"
				+ "            }\r\n" + "          ],\r\n"
				+ "          \"uri\": \"http://www.theworldavatar.com/MessageContent_Input_xzbAvBW\"\r\n"
				+ "        }\r\n" + "      ],\r\n"
				+ "      \"httpUrl\": \"http://www.theworldavatar.com/JPS_COMPOSITION/CoordinateToWeather\",\r\n"
				+ "      \"uri\": \"http://www.theworldavatar.com/Operation_pexDwAC\"\r\n"
				+ "    }\r\n" + "  ],\r\n"
				+ "  \"uri\": \"http://www.theworldavatar.com/Composite_Service_8PPv74p\"\r\n"
				+ "}";

		String myHost = "localhost";
		int myPort = 8080;
		URIBuilder builder = new URIBuilder().setScheme("http")
				.setHost(myHost).setPort(myPort)
				.setPath("/JPS_COMPOSITION_LITE/CompositionEndpoint")
				.setParameter("query", agentInString);
		System.out.println(builder);
		String result = Request.executeGet(builder);

		System.out.println(result);

	}

}
