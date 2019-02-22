package uk.ac.cam.cares.jps.discovery.test;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AbstractAgentServiceDescription;
import uk.ac.cam.cares.jps.base.discovery.Agent;
import uk.ac.cam.cares.jps.base.discovery.AgentCallAdditionalMethods;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.discovery.AgentRequest;
import uk.ac.cam.cares.jps.base.discovery.AgentResponse;
import uk.ac.cam.cares.jps.base.discovery.Parameter;

@WebServlet(urlPatterns = {"/DiscoveryTest/AgentTwo"})
public class WeatherAgentTwo extends HttpServlet {
	
	private static final long serialVersionUID = -4199209974912271432L;
	
	Logger logger = LoggerFactory.getLogger(WeatherAgentTwo.class);
	
	@Override
	public void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException, ServletException {
		
		logger.info("WeatherAgentTwo start");
		
		AgentRequest agentRequest = AgentCallAdditionalMethods.getAgentRequest(req);
		AgentResponse agentResponse = new AgentResponse();
		AbstractAgentServiceDescription.copyParameters(agentRequest, agentResponse);
		
		Parameter param = agentResponse.getOutputParameters().get(0);
		param.setValue("30.3");
		
		AgentCaller.printToResponse(agentResponse, resp);
		
		logger.info("WeatherAgentTwo exit");
	}
	
	public Agent getAgent() {
		String general = "domain,weather";
		String input = "city,null";
		String output = "IRItemperature,null";
		
		return DescriptionFactory.createAgent("http://localhost:8080/JPS_DISCOVERY/DiscoveryTest/AgentTwo", general, input, output);
	}
}