package uk.ac.cam.cares.jps.base.router.test;

import static org.junit.jupiter.api.Assertions.*;

import org.json.JSONObject;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import com.github.tomakehurst.wiremock.client.WireMock;
import com.github.tomakehurst.wiremock.junit5.WireMockRuntimeInfo;
import com.github.tomakehurst.wiremock.junit5.WireMockTest;

import uk.ac.cam.cares.jps.base.agent.AgentCaller;
import uk.ac.cam.cares.jps.base.discovery.MediaType;

/**
 * This class is intended to test the integration of the 
 * {@link uk.ac.cam.cares.jps.base.agent.AgentCaller AgentCaller} with
 * the {@link uk.ac.cam.cares.jps.base.router.AgentRouter AgentRouter}.
 * @author csl37
 *
 */
@WireMockTest()
class AgentCallerTest {
	
	private static int port;
	private static String agentURL;
	private static String AGENT_NAME = "agent1";
	private static String PATH = "/agent1/location";
			
	@BeforeAll
	static void setup(WireMockRuntimeInfo wmRuntimeInfo) {
		port = wmRuntimeInfo.getHttpPort();
		agentURL = "http://localhost:"+String.valueOf(port)+PATH;
	}
	
	@Test
	void testGetWithJson(WireMockRuntimeInfo wmRuntimeInfo) {
		
		String json = "{ \"parameter\": \"value\" }";
		
		AgentCaller agentCaller = new AgentCaller();
		AgentCaller spyAgentCaller = Mockito.spy(agentCaller);
		Mockito.doReturn(agentURL).when(spyAgentCaller).getAgentUrl(AGENT_NAME);
						
		WireMock wireMock = wmRuntimeInfo.getWireMock();
	    wireMock.register(WireMock.get(WireMock.urlPathMatching(PATH)).willReturn(WireMock.okJson(json)));
		
	    String result = spyAgentCaller.getWithJson(AGENT_NAME, new JSONObject(json));
	    assertEquals(json, result);
	}

	@Test
	void testPostWithJson(WireMockRuntimeInfo wmRuntimeInfo) {
		
		String json = "{ \"parameter\": \"value\" }";
		
		AgentCaller agentCaller = new AgentCaller();
		AgentCaller spyAgentCaller = Mockito.spy(agentCaller);
		Mockito.doReturn(agentURL).when(spyAgentCaller).getAgentUrl(AGENT_NAME);
						
		WireMock wireMock = wmRuntimeInfo.getWireMock();
	    wireMock.register(WireMock.post(WireMock.urlPathMatching(PATH)).willReturn(WireMock.okJson(json)));
		
	    String result = spyAgentCaller.postWithJson(AGENT_NAME, new JSONObject(json));
	    assertEquals(json, result);
	}

	@Test
	void testPost(WireMockRuntimeInfo wmRuntimeInfo) {
		
		String body = "<http://www.example.com/test/s> <http://www.example.com/test/p> <http://www.example.com/test/o>.";
		String json = "{ \"parameter\": \"value\" }";
				
		AgentCaller agentCaller = new AgentCaller();
		AgentCaller spyAgentCaller = Mockito.spy(agentCaller);
		Mockito.doReturn(agentURL).when(spyAgentCaller).getAgentUrl(AGENT_NAME);
						
		WireMock wireMock = wmRuntimeInfo.getWireMock();
	    wireMock.register(WireMock.post(WireMock.urlPathMatching(PATH)).willReturn(WireMock.okJson(json)));
		
	    String result = spyAgentCaller.post(AGENT_NAME, body, MediaType.APPLICATION_N_TRIPLES.type, MediaType.APPLICATION_JSON.type);
	    assertEquals(json, result);
	}
}
