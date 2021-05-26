package uk.ac.cam.cares.jps.chatbotwrappers;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.http.HttpResponse.BodyHandler;
import java.net.http.HttpResponse.BodyHandlers;
import java.util.ArrayList;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.chatbot.config.ChatbotConstants;

/**
 * Servlet implementation class QuestionClassification
 */
@WebServlet(description = "This service provides HTTP interface for Marie question classification agent", urlPatterns = { "/QuestionClassification" })
public class QuestionClassification extends HttpServlet {
	private static final long serialVersionUID = 1L;
	private static final String route = "/QuestionClassification"; // set the internal route for this agent 

    /**
     * @see HttpServlet#HttpServlet()
     */
    public QuestionClassification() {
        super();
        // TODO Auto-generated constructor stub
    }

    
	protected JSONObject processRequestParameters(JSONObject requestParams) {
		// expected input is "term"
		
	    String host = AgentLocator.getProperty("host");
	    String port = ChatbotConstants.ChatbotPortNumber; 
	    String subdirectory = ChatbotConstants.SubDirectory;
	    String question = requestParams.getString("question");	    // get the input 
	    // construct the complete http request 
	    
	    
	    String path = "http://" + host + ":" + port + "/" + subdirectory + route;
	    // To request the interface provided by flask (The Python scripts already provide HTTP access, the Java wrapper is
	    // an extra layer to provide a standard agent interface.) 
	    
 
	    String result_string = AgentCaller.executeGet(path, "question", question);
	    JSONObject result = new JSONObject();
	    result.put("result", result_string);
		return result;
	}
	
	
 

}
