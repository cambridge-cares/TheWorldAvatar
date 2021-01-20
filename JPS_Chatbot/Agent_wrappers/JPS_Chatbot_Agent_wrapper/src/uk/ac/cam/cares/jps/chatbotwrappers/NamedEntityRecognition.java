package uk.ac.cam.cares.jps.chatbotwrappers;

import java.io.IOException;
import java.util.ArrayList;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.chatbot.config.ChatbotConstants;

/**
 * Servlet implementation class NamedEntityRecognition
 */
@WebServlet("/NamedEntityRecognition")
public class NamedEntityRecognition extends JPSHttpServlet {
	private static final long serialVersionUID = 1L;
	private static final String route = "/query_wolfram";
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public NamedEntityRecognition() {
        super();
        // TODO Auto-generated constructor stub
    }

	@Override	
	protected JSONObject processRequestParameters(JSONObject requestParams) {
		// expected inputs 
		
	    String host = AgentLocator.getProperty("host");
	    String port = ChatbotConstants.ChatbotPortNumber;
	    String subdirectory = ChatbotConstants.SubDirectory;
	    String question = requestParams.getString("question");	    
	    // construct the complete http request 
	    
	    // To request the interface provided by flask (The Python scripts already provide HTTP access, the Java wrapper is
	    // an extra layer to provide a standard agent interface.) 
	    String path = "http://" + host + ":" + port + "/" + subdirectory + route;
	    String result_string = AgentCaller.executeGet(path, "question", question);
	    JSONObject result = new JSONObject();
	    result.put("result", result_string);
		return result;
	}
    
 

}
