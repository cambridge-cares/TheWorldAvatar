package uk.ac.cam.cares.jps.composition.WebServer;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.HTTP;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.composition.CompositionEngine.ServiceCompositionEngine;
import uk.ac.cam.cares.jps.composition.EngineModel.Graph;
import uk.ac.cam.cares.jps.composition.EngineModel.Layer;
import uk.ac.cam.cares.jps.composition.ServiceModel.MessagePart;
import uk.ac.cam.cares.jps.composition.ServiceModel.Service;
import uk.ac.cam.cares.jps.composition.util.FormatTranslator;

/**
 * Servlet implementation class ServiceComposition
 */
@WebServlet("/ServiceComposition")
public class ServiceComposition extends HttpServlet {
	private static final long serialVersionUID = 1L;            

    /**
     * @throws URISyntaxException 
     * @throws IOException 
     * @throws JSONException 
     * @see HttpServlet#HttpServlet()
     */
    public ServiceComposition() throws JSONException, IOException, URISyntaxException {
        super();
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
			// First take the inputs of the request JSON object
		  try {
	            StringBuilder sb = new StringBuilder();
	            String s;
	            while ((s = request.getReader().readLine()) != null) {sb.append(s);}
	            JSONObject jsonObject =  HTTP.toJSONObject(sb.toString());
	            String AgentInString = jsonObject.getString("Method").toString();
	            Service agent = FormatTranslator.convertJSONTOJavaClass(AgentInString);
	            ServiceCompositionEngine compositionEngine = new ServiceCompositionEngine(agent);
	            boolean met = false;
	            int i = 0;
	            response.getWriter().write("GOOD");
	            while(!met)
	            {
		            met = compositionEngine.appendLayerToGraph(i++);
	            }
		  }
		  catch (Exception ex) {
			  
		  } 
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doGet(request, response);
	}

}
