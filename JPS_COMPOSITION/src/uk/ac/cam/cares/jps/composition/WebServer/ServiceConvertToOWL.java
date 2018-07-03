package uk.ac.cam.cares.jps.composition.WebServer;

import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.HTTP;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.composition.CompositionEngine.ServiceCompositionEngine;
import uk.ac.cam.cares.jps.composition.ServiceModel.Service;
import uk.ac.cam.cares.jps.composition.util.FormatTranslator;
import uk.ac.cam.cares.jps.composition.Ontology.*;

/**
 * Servlet implementation class ServiceConvertToOWL
 */
@WebServlet("/ServiceConvertToOWL")
public class ServiceConvertToOWL extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public ServiceConvertToOWL() {
        super();
        // TODO Auto-generated constructor stub
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
		  try {
	            StringBuilder sb = new StringBuilder();
	            String s;
	            while ((s = request.getReader().readLine()) != null) {sb.append(s);}
	            JSONObject jsonObject =  HTTP.toJSONObject(sb.toString());
	            String AgentInString = jsonObject.getString("Method").toString();
	            Service agent = FormatTranslator.convertJSONTOJavaClass(AgentInString);
	            ServiceWriter writer = new ServiceWriter();
	            response.getWriter().write(writer.generateModel(agent));
		  }
		  catch (Exception ex) {
			  
		  } 
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
		doGet(request, response);
	}

}
