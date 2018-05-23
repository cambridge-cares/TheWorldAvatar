package uk.ac.cam.cares.jps.discovery.test;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AbstractAgentServiceDescription;
import uk.ac.cam.cares.jps.base.discovery.Agent;
import uk.ac.cam.cares.jps.base.discovery.AgentRequest;
import uk.ac.cam.cares.jps.base.discovery.AgentResponse;
import uk.ac.cam.cares.jps.base.discovery.Parameter;
import uk.ac.cam.cares.jps.discovery.factory.DiscoveryFactory;
import uk.ac.cam.cares.jps.discovery.util.ISerializer;
import uk.ac.cam.cares.jps.discovery.util.JPSBaseServlet;

@WebServlet(urlPatterns = {"/DiscoveryTest/AgentTwo"})
public class WeatherAgentTwo extends JPSBaseServlet {
	
	private static final long serialVersionUID = -4199209974912271432L;
	
	Logger logger = LoggerFactory.getLogger(WeatherAgentTwo.class);
	private ISerializer serializer = DiscoveryFactory.getSerializer();
	
	@Override
	public void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException, ServletException {
		
		logger.info("WeatherAgentTwo start");

		String serializedAgentRequest = req.getParameter("agentrequest");
		AgentRequest agentRequest = serializer.<AgentRequest>convertFrom(serializedAgentRequest, AgentRequest.class).get();
		AgentResponse agentResponse = new AgentResponse();
		AbstractAgentServiceDescription.copyParameters(agentRequest, agentResponse);
		
		Parameter param = agentResponse.getOutputParameters().get(0);
		param.setValue("30.3");
		String serializedAgentResponse = serializer.convertToString(agentResponse);
		
		print(resp, serializedAgentResponse);
		
		serializer.<AgentResponse>convertFrom(serializedAgentResponse, AgentResponse.class).get();
		
		logger.info("WeatherAgentTwo exit");
	}
	
	public Agent getAgent() {
		String general = "domain,weather";
		String input = "city,null";
		String output = "IRItemperature,null";
		
		return DescriptionFactory.createAgent("http://localhost:8080/JPS_DISCOVERY/DiscoveryTest/AgentTwo", general, input, output);
	}
}