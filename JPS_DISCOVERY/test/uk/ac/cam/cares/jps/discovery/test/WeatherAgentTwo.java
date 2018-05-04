package uk.ac.cam.cares.jps.discovery.test;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.discovery.api.AbstractAgentDescription;
import uk.ac.cam.cares.jps.discovery.api.AgentDescription;
import uk.ac.cam.cares.jps.discovery.api.AgentRequest;
import uk.ac.cam.cares.jps.discovery.api.AgentResponse;
import uk.ac.cam.cares.jps.discovery.api.Parameter;
import uk.ac.cam.cares.jps.discovery.api.TypeString;
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
		AgentRequest agentRequest = serializer.<AgentRequest>convertFrom(serializedAgentRequest).get();
		AgentResponse agentResponse = new AgentResponse();
		AbstractAgentDescription.copy(agentRequest, agentResponse);
		
		Parameter param = agentResponse.getOutputParameters().get(0);
		param.setValue(new TypeString("30.3"));
		String serializedAgentResponse = serializer.convertToString(agentResponse);
		
		print(resp, serializedAgentResponse);
		
		serializer.<AgentResponse>convertFrom(serializedAgentResponse).get();
		
		logger.info("WeatherAgentTwo exit");
	}
	
	public AgentDescription getAgentDescription() {
		String general = "domain,weather,address,http://localhost:8080/JPS_DISCOVERY/DiscoveryTest/AgentTwo";
		String input = "city,null";
		String output = "IRItemperature,null";
		
		return DescriptionFactory.createAgentDescription(general, input, output);
	}
}