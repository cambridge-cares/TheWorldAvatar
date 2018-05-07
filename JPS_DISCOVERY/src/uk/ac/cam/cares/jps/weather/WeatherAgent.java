package uk.ac.cam.cares.jps.weather;

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
import uk.ac.cam.cares.jps.discovery.test.DescriptionFactory;
import uk.ac.cam.cares.jps.discovery.test.WeatherAgentTwo;
import uk.ac.cam.cares.jps.discovery.util.ISerializer;
import uk.ac.cam.cares.jps.discovery.util.JPSBaseServlet;

@WebServlet(urlPatterns = {"/DiscoveryTest/WeatherAgent"})
public class WeatherAgent extends JPSBaseServlet {
	
	private static final long serialVersionUID = -4199209974912271432L;
	
	Logger logger = LoggerFactory.getLogger(WeatherAgentTwo.class);
	private ISerializer serializer = DiscoveryFactory.getSerializer();
	
	//private OWLSerializer serializer2= new OWLSerializer();
	
	@Override
	public void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException, ServletException {
		
		logger.info("WeatherAgent start");

		String serializedAgentRequest = req.getParameter("agentrequest");
		
		//convert from the string to serialized object for the response
		AgentRequest agentRequest = serializer.<AgentRequest>convertFrom(serializedAgentRequest).get();
		
		AgentResponse agentResponse = new AgentResponse();
		//copy from the request stream of input and output parameter into the response stream
		AbstractAgentDescription.copy(agentRequest, agentResponse);
		
		//later put the value to be taken from the website of weather
		Parameter param = agentResponse.getOutputParameters().get(0);
		param.setValue(new TypeString("28"));
		Parameter param2 = agentResponse.getOutputParameters().get(1);
		param2.setValue(new TypeString("5"));
		Parameter param3 = agentResponse.getOutputParameters().get(2);
		param3.setValue(new TypeString("14"));
		Parameter param4 = agentResponse.getOutputParameters().get(3);
		param4.setValue(new TypeString("90"));
		
		//convert from the serialized object to string for the response
		String serializedAgentResponse = serializer.convertToString(agentResponse);
		
	// serializer2.convertToString(agentResponse);
		
		print(resp, serializedAgentResponse);
		
		//convert from the string to serialized object for the response
		serializer.<AgentResponse>convertFrom(serializedAgentResponse).get();
		
		//serializer2.<AgentResponse>convertFrom(serializedAgentResponsetoOWL).get();
		
	//serializer2.createAgentWithAgentDescription();
		
		logger.info("WeatherAgent exit");
	}
	
	public AgentDescription getAgentDescription() {
		String general = "domain,weather,address,http://localhost:8080/JPS_DISCOVERY/DiscoveryTest/WeatherAgent";
		String input = "city,null";
		String output = "temperature,null,cloudcover,null,windspeed,null,winddirection,null";
		
		return DescriptionFactory.createAgentDescription(general, input, output);
	}
}