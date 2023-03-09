package uk.ac.cam.cares.jps.base.discovery;

import javax.servlet.http.HttpServletRequest;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.Gson;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class AgentCallAdditionalMethods {

	private static Logger logger = LoggerFactory.getLogger(AgentCallAdditionalMethods.class);
	
	@Deprecated
	public static AgentResponse callAgent(String contextPath, AgentRequest agentRequest)  {
		
		Gson gson = new Gson();
		
		logger.info("callAgent start ");
		
		String serializedAgentRequest = gson.toJson(agentRequest);
		
		logger.info("SerAgRequ " + serializedAgentRequest);
		
		try {
			String serializedAgentResponse = AgentCaller.executeGet(contextPath, "agentrequest", serializedAgentRequest);
			
			logger.info("SerAgResp " + serializedAgentResponse);
						
			return gson.fromJson(serializedAgentResponse, AgentResponse.class);
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}
	
	@Deprecated
	public static AgentRequest getAgentRequest(HttpServletRequest req) {
		String serializedAgentRequest = req.getParameter("agentrequest");
		return new Gson().fromJson(serializedAgentRequest, AgentRequest.class);
	}
	
}
