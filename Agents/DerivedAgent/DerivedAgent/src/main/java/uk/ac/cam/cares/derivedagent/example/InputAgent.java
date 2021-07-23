package uk.ac.cam.cares.derivedagent.example;

import javax.servlet.annotation.WebServlet;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;

/**
 * An agent to modify the input
 * @author Kok Foong Lee
 *
 */
@WebServlet(urlPatterns = {"/InputAgent"}) 
public class InputAgent extends JPSAgent {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		return requestParams;
	}
}
