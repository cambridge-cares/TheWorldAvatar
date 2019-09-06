package uk.ac.cam.cares.jps.scenario;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.ScenarioHelper;

@WebServlet(urlPatterns = {"/kb/*", "/data/*"})
public class KnowledgeBaseManager extends HttpServlet {

	private static final long serialVersionUID = -4195274773048314961L;
	
	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
			
		String path = request.getServletPath() + request.getPathInfo();
		String cutPath = ScenarioHelper.cutHash(path);;
		String bucket = ScenarioHelper.getScenarioBucket(JPSConstants.SCENARIO_NAME_BASE);
		String hostport = KeyValueManager.get(IKeys.HOST) + "_" + KeyValueManager.get(IKeys.PORT);
		String localFile = bucket + "/" + hostport + cutPath;		
		String result =  new QueryBroker().readFile(localFile); 
		AgentCaller.printToResponse(result, response);
	}
}
