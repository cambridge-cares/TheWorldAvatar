package uk.ac.cam.cares.jps.weather;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.discovery.api.AbstractAgentServiceDescription;
import uk.ac.cam.cares.jps.discovery.api.Agent;
import uk.ac.cam.cares.jps.discovery.api.AgentRequest;
import uk.ac.cam.cares.jps.discovery.api.AgentResponse;
import uk.ac.cam.cares.jps.discovery.api.Parameter;
import uk.ac.cam.cares.jps.discovery.api.TypeString;
import uk.ac.cam.cares.jps.discovery.factory.DiscoveryFactory;
import uk.ac.cam.cares.jps.discovery.test.DescriptionFactory;
import uk.ac.cam.cares.jps.discovery.util.ISerializer;
import uk.ac.cam.cares.jps.discovery.util.JPSBaseServlet;
import uk.ac.cam.cares.jps.util.PythonHelper;

@WebServlet(urlPatterns = {"/DiscoveryTest/WeatherAgent"})
public class WeatherAgent extends JPSBaseServlet {
	
	private static final long serialVersionUID = -4199209974912271432L;
	
	Logger logger = LoggerFactory.getLogger(WeatherAgent.class);
	private ISerializer serializer = DiscoveryFactory.getSerializer();
	

	
	@Override
	public void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException, ServletException {
		
		logger.info("WeatherAgent start");

		String serializedAgentRequest = req.getParameter("agentrequest");
		
		//convert from the string to serialized object for the response
		AgentRequest agentRequest = serializer.<AgentRequest>convertFrom(serializedAgentRequest).get();
		
		AgentResponse agentResponse = new AgentResponse();
		
		//if need to take the value using the python code
		//PythonHelper.callPython("JPS/python/caresjpsadmsinputs/cobbling.py", "main");
		
		String key1="nl"; //country
		String key2="the-hague"; //specific-area
		String key3="251687"; //unknown real=251687
		URL url = new URL("https://www.accuweather.com/en/"+key1+"/"+key2+"/"+key3+"/current-weather/"+key3);
	
        // Get the input stream through URL Connection
        URLConnection con = url.openConnection();
        InputStream is =con.getInputStream();
        BufferedReader br = new BufferedReader(new InputStreamReader(is));

        String s=null;
        String line = null;
       
        while ((line = br.readLine()) != null) {
        	    	     	 s+=line;
           
        }
        
        String selected=s.split("<span class=\"cond\">")[5];
        String selected2=s.split("<span class=\"cond\">")[6];
       
        String tempvalue= selected.split("<span class=\"large-temp\">")[1].split(";</span>")[0].split("&")[0];
        String cloudcover=selected2.split("</strong></li>")[4].split("<li>")[1].split("<strong>")[1].split("%")[0];
        String cloudcovercalculate = String.valueOf(Math.round(Float.valueOf(cloudcover)/100*8));
        String windvalue= selected2.split("<li class=\"wind\"><strong>")[1].split("</strong></li>")[0].split(" ")[0];
        String direction=selected2.split("<li class=\\\"wind\">")[1].split("</li>")[0].split(" ")[3];
    
        direction = convertWindDirectiontoAngle(direction);
        
        logger.info("temperature value= "+tempvalue);
        logger.info("wind value= "+windvalue);
        logger.info("wind direction from= "+direction);
        logger.info("cloud cover= "+cloudcovercalculate);
       
		
		//copy from the request stream of input and output parameter into the response stream
		AbstractAgentServiceDescription.copyParameters(agentRequest, agentResponse);
		
		//later put the value to be taken from the website of weather and put to the knowledge base of the weather
		Parameter param = agentResponse.getOutputParameters().get(0);
		param.setValue(new TypeString("28"));
		Parameter param2 = agentResponse.getOutputParameters().get(1);
		param2.setValue(new TypeString("5"));
		Parameter param3 = agentResponse.getOutputParameters().get(2);
		param3.setValue(new TypeString("15"));
		Parameter param4 = agentResponse.getOutputParameters().get(3);
		param4.setValue(new TypeString("50"));
		
		//convert from the serialized object to string for the response
		String serializedAgentResponse = serializer.convertToString(agentResponse);
		
		print(resp, serializedAgentResponse);
		
		//convert from the string to serialized object for the response
		serializer.<AgentResponse>convertFrom(serializedAgentResponse).get();
		

		
		logger.info("WeatherAgent exit");
	}

	private String convertWindDirectiontoAngle(String direction) {
		String windirkey[]= {"N","NNE","NE","ENE","E","ESE", "SE", "SSE","S","SSW","SW","WSW","W","WNW","NW","NNW"};
		for (int x = 0; x < windirkey.length; x++) {
			if (direction.equals(windirkey[x])) {
				Double angle = x * 22.5;
				direction = String.valueOf(angle);
			}
		}
		return direction;
	}
	
	
	public Agent getAgent() {
		String general = "domain,weather";
		String input = "city,null";
		String output = "temperature,null,cloudcover,null,windspeed,null,winddirection,null";
		
		return DescriptionFactory.createAgent("http://localhost:8080/JPS_DISCOVERY/DiscoveryTest/WeatherAgent", general, input, output);
	}
}