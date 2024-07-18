package uk.ac.cam.cares.jps.base.config;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class JPSConstantsTest {

	@Test
	public void testJPSConstants() {
		
		assertEquals(JPSConstants.KNOWLEDGE_BASE_JPS, "jps");
		assertEquals(JPSConstants.KNOWLEDGE_BASE_PATH_JPS_DATASET, "/jps/dataset");
		assertEquals(JPSConstants.ACCESS_AGENT_PATH, "/access-agent/access");
		assertEquals(JPSConstants.RDB_ACCESS_AGENT_PATH, "/access-agent/rdbaccess");
		assertEquals(JPSConstants.SCENARIO_NAME_BASE, "base");
		assertEquals(JPSConstants.SCENARIO_SUBDIR_KB, "kb");
		assertEquals(JPSConstants.SCENARIO_SUBDIR_DATA, "data");
		assertEquals(JPSConstants.SCENARIO_NAME_TEST, "testPOWSYSNuclearStartSimulationAndProcessResultAgentCallForTestScenario");
		assertEquals(JPSConstants.SCENARIO_JPS_CONTEXT, "jpscontext");
		assertEquals(JPSConstants.SCENARIO_URL, "scenariourl");
		assertEquals(JPSConstants.SCENARIO_USE_CASE_URL, "usecaseurl");
		assertEquals(JPSConstants.SCENARIO_RESOURCE, "scenarioresource");
		assertEquals(JPSConstants.SCENARIO_DATASET, "dataset");
		assertEquals(JPSConstants.SCENARIO_AGENT, "scenarioagent");
		assertEquals(JPSConstants.SCENARIO_AGENT_OPERATION, "scenarioagentoperation");
		assertEquals(JPSConstants.SCENARIO_SIMULATION_TIME, "simulationtime");
		assertEquals(JPSConstants.QUERY_SPARQL_QUERY, "sparqlquery");
		assertEquals(JPSConstants.QUERY_SPARQL_UPDATE, "sparqlupdate");
		assertEquals(JPSConstants.QUERY_FILE, "file");
		assertEquals(JPSConstants.RUN_SIMULATION, "runsimulation");
		assertEquals(JPSConstants.CONTENT, "body");
		assertEquals(JPSConstants.REQUESTURL, "requestUrl");
		assertEquals(JPSConstants.PATH, "path");
		assertEquals(JPSConstants.CONTENTTYPE, "contentType");
		assertEquals(JPSConstants.METHOD, "method");
		assertEquals(JPSConstants.HEADERS, "acceptHeaders");
		assertEquals(JPSConstants.TARGETIRI, "targetresourceiri");
		assertEquals(JPSConstants.TARGETGRAPH, "targetgraph");
		assertEquals(JPSConstants.KNOWLEDGE_BASE_URL, "jps/kb");
		assertTrue(JPSConstants.COPY_ON_READ);
		assertEquals(JPSConstants.SCENARIO_OPTION_COPY_ON_READ, "copyonread");
		assertEquals(JPSConstants.QUERY_ENDPOINT, "queryendpoint");
		assertEquals(JPSConstants.UPDATE_ENDPOINT, "updateendpoint");
		assertEquals(JPSConstants.RESULT_KEY, "result");
		assertEquals(JPSConstants.ASK_RESULT_KEY, "ASK");
	}

}
