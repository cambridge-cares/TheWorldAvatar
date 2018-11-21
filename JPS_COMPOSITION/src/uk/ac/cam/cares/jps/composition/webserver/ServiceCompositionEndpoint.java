package uk.ac.cam.cares.jps.composition.webserver;

import java.io.IOException;
import java.util.Map;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.HTTP;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.agents.discovery.ServiceDiscovery;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.KeyValueServer;
import uk.ac.cam.cares.jps.composition.compositionengine.ServiceCompositionEngine;
import uk.ac.cam.cares.jps.composition.enginemodel.Graph;
import uk.ac.cam.cares.jps.composition.performance.SmartContractDataConnector;
import uk.ac.cam.cares.jps.composition.servicemodel.Service;
import uk.ac.cam.cares.jps.composition.util.FormatTranslator;


@WebServlet("/ServiceCompositionEndpoint")
public class ServiceCompositionEndpoint extends HttpServlet {
	private static final long serialVersionUID = 1L;

	public static Map<String,Long[]> scoreMap;
	
	public void init(ServletConfig config) throws ServletException {
		try {
			scoreMap = SmartContractDataConnector.findScoresForAgents();
		} catch (JSONException e) {
			e.printStackTrace();
		}
	}
	
	
	public ServiceCompositionEndpoint() {
		super();
	}

	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

	}

	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		try {
			StringBuilder sb = new StringBuilder();
			String s;
			while ((s = request.getReader().readLine()) != null) {
				sb.append(s);
			}
			JSONObject jsonObject = HTTP.toJSONObject(sb.toString());
			String AgentInString = jsonObject.getString("Method").toString();
			
			String compositionDir = AgentLocator.getCurrentJpsAppDirectory(this);
 			KeyValueServer.set(ServiceDiscovery.KEY_DIR_KB_AGENTS, compositionDir + "/testres/admsservices");
 
			Service agent = FormatTranslator.convertJSONTOJavaClass(AgentInString);
 
			ServiceCompositionEngine engine = new ServiceCompositionEngine(agent, "http://localhost:8080");
			if(engine.start()) {
				Graph graph = engine.getGraph();
				graph.scoreMap = scoreMap;
				System.out.println("============= scoreMap ============");
				System.out.println(scoreMap);
				System.out.println("===================================");
				JSONObject graphInJSON = FormatTranslator.convertGraphJavaClassTOJSON(graph);
				response.getWriter().write(graphInJSON.toString());
			}
			else {
				response.getWriter().write("Unsolvable");
			}


		} catch (Exception ex) {

		}
	}

}
