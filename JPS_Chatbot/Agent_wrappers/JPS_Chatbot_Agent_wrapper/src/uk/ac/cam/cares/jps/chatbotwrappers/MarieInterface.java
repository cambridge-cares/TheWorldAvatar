package uk.ac.cam.cares.jps.chatbotwrappers;

import javax.servlet.annotation.WebServlet;

import org.apache.http.client.methods.HttpGet;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.chatbot.config.ChatbotConstants;
 


@WebServlet("/MarieInterface")
public class MarieInterface extends JPSHttpServlet {

	


	@Override	
	protected JSONObject processRequestParameters(JSONObject requestParams) {
		// expected inputs 
		
	    String host = AgentLocator.getProperty("host");
	    System.out.println("host is: " + host);
	    String port = ChatbotConstants.ChatbotPortNumber;
	    System.out.println("port is:" + port);
	    String subdirectory = ChatbotConstants.SubDirectory;
	    System.out.println("subd is:" + subdirectory);
		
		
	    String question = requestParams.getString("question");
	    // host to be 
	    
	    String path = "http://" + host + ":" + port + "/" + subdirectory;
	    System.out.println("path is:" + path); 
	   
	    
	    String result_string = AgentCaller.executeGet(path + "/query_wolfram", "question", question);
	    JSONObject result = new JSONObject();
	    result.put("result", result_string);
		return result;
	}
	
	
	// make request to the flask thing ... 
	
	
}
