package uk.ac.cam.cares.jps.base.router;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.apache.http.HttpHeaders;
import org.json.JSONObject;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import com.github.tomakehurst.wiremock.client.WireMock;
import com.github.tomakehurst.wiremock.junit5.WireMockRuntimeInfo;
import com.github.tomakehurst.wiremock.junit5.WireMockTest;

import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.http.Http;

@WireMockTest()
class AgentCallerTest {
	
	private static int port;
	private static String agentURL;
	private static String AGENT_NAME = "agent1";
	private static String PATH = "/agent1/location";
	
	private static final String PARAMETER = "parameter";
	private static final String VALUE = "value";
	private static final String JSON ="{ \""+PARAMETER+"\": \""+VALUE+"\" }";
	
	
	@BeforeAll
	static void setup(WireMockRuntimeInfo wmRuntimeInfo) {
		port = wmRuntimeInfo.getHttpPort();
		agentURL = "http://localhost:"+String.valueOf(port)+PATH;
	}
	
	@Test
	void testGetWithJson(WireMockRuntimeInfo wmRuntimeInfo) {
				
		AgentCaller agentCaller = new AgentCaller();
		AgentCaller spyAgentCaller = Mockito.spy(agentCaller);
		Mockito.doReturn(agentURL).when(spyAgentCaller).getAgentUrl(AGENT_NAME);
						
		WireMock wireMock = wmRuntimeInfo.getWireMock();
	    wireMock.register(WireMock.get(WireMock.urlPathMatching(PATH))
	    						.withHeader(HttpHeaders.ACCEPT, WireMock.matching(MediaType.APPLICATION_JSON.type))
	    						.withQueryParam(Http.JSON_PARAMETER_KEY, WireMock.containing(PARAMETER))
	    						.withQueryParam(Http.JSON_PARAMETER_KEY, WireMock.containing(VALUE))
	    						.willReturn(WireMock.okJson(JSON)));
		
	    String result = spyAgentCaller.getWithJson(AGENT_NAME, new JSONObject(JSON));
	    assertEquals(JSON, result);
	}

	@Test
	void testPostWithJson(WireMockRuntimeInfo wmRuntimeInfo) {
				
		AgentCaller agentCaller = new AgentCaller();
		AgentCaller spyAgentCaller = Mockito.spy(agentCaller);
		Mockito.doReturn(agentURL).when(spyAgentCaller).getAgentUrl(AGENT_NAME);
						
		WireMock wireMock = wmRuntimeInfo.getWireMock();
	    wireMock.register(WireMock.post(WireMock.urlPathMatching(PATH))
					    		.withHeader(HttpHeaders.ACCEPT, WireMock.matching(MediaType.APPLICATION_JSON.type))
					    		.withHeader(HttpHeaders.CONTENT_TYPE, WireMock.matching(MediaType.APPLICATION_JSON.type))
								.withRequestBody( WireMock.containing(PARAMETER))
								.withRequestBody( WireMock.containing(VALUE))
	    						.willReturn(WireMock.okJson(JSON)));
		
	    String result = spyAgentCaller.postWithJson(AGENT_NAME, new JSONObject(JSON));
	    assertEquals(JSON, result);
	}

	@Test
	void testPost(WireMockRuntimeInfo wmRuntimeInfo) {
		
		String body = "<http://www.example.com/test/s> <http://www.example.com/test/p> <http://www.example.com/test/o>.";
		String acceptType = MediaType.APPLICATION_JSON.type;	
		String contentType = MediaType.APPLICATION_N_TRIPLES.type;
		
		AgentCaller agentCaller = new AgentCaller();
		AgentCaller spyAgentCaller = Mockito.spy(agentCaller);
		Mockito.doReturn(agentURL).when(spyAgentCaller).getAgentUrl(AGENT_NAME);
						
		WireMock wireMock = wmRuntimeInfo.getWireMock();
	    wireMock.register(WireMock.post(WireMock.urlPathMatching(PATH))
	    						.withHeader(HttpHeaders.ACCEPT, WireMock.matching(acceptType))
	    						.withHeader(HttpHeaders.CONTENT_TYPE, WireMock.matching(contentType))
	    						.withRequestBody(WireMock.matching(body))
	    						.willReturn(WireMock.okJson(JSON)));
		
	    String result = spyAgentCaller.post(AGENT_NAME, body, contentType, acceptType);
	    assertEquals(JSON, result);
	}
}
