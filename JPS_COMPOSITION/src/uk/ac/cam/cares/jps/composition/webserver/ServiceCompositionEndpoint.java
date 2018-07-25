package uk.ac.cam.cares.jps.composition.WebServer;

import java.io.IOException;
import java.net.URISyntaxException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.HTTP;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.composition.CompositionEngine.ServiceCompositionEngine;
import uk.ac.cam.cares.jps.composition.ServiceModel.Service;
import uk.ac.cam.cares.jps.composition.util.ConnectionBuilder;
import uk.ac.cam.cares.jps.composition.util.FormatTranslator;

/**
 * Servlet implementation class ServiceCompositionEndpoint
 */
@WebServlet("/ServiceCompositionEndpoint")
public class ServiceCompositionEndpoint extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public ServiceCompositionEndpoint() {
        super();
        // TODO Auto-generated constructor stub
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
	  
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
		  try {
	            StringBuilder sb = new StringBuilder();
	            String s;
	            while ((s = request.getReader().readLine()) != null) {sb.append(s);}
	            JSONObject jsonObject =  HTTP.toJSONObject(sb.toString());
	            String AgentInString = jsonObject.getString("Method").toString();
	            Service agent = FormatTranslator.convertJSONTOJavaClass(AgentInString);
	            ServiceCompositionEngine myCompositionEngine = new ServiceCompositionEngine(agent);
	            
	            boolean met = false;
	    		int index = 0;
	    		while(!met) {
	    			index++;
	    			met = myCompositionEngine.appendLayerToGraph(index);
	    		}
	    		myCompositionEngine.eliminateRedundantAgent();
	    		ConnectionBuilder connectionBuilder = new ConnectionBuilder();
	    		connectionBuilder.buildEdge(myCompositionEngine.getGraph()); // build the connection between services 
	    		JSONObject graphInJSON = FormatTranslator.convertGraphJavaClassTOJSON(myCompositionEngine.getGraph());
	    		response.getWriter().write(graphInJSON.toString());
	    		
	          }
		  catch (Exception ex) {
			  
		  }
	}
}
