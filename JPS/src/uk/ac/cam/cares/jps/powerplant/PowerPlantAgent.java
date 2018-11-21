package uk.ac.cam.cares.jps.powerplant;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
/*Author ZHOUXIAOCHI*/

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

/**
 * Servlet implementation class PowerPlantWrapperAgent
 */
@WebServlet("/powerplant/calculateemission")
public class PowerPlantAgent extends HttpServlet {

	private static final long serialVersionUID = 2796334308068192311L;
	private Logger logger = LoggerFactory.getLogger(PowerPlantAgent.class);

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
 
		try {
			JSONObject jo = new JSONObject().put("waste", "todo - add here the IRI of the waste emission stream of the given plant");
			AgentCaller.printToResponse(jo.toString(), response);
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
		}
	}
}
