package uk.ac.cam.cares.derivation.asynexample;

import javax.servlet.annotation.WebServlet;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

/**
 * This InitialiseAgents agent initialises all the asynchronous agents in this example.
 * @author Jiaru Bai (jb2197@cam.ac.uk)
 * 
 */
@WebServlet(urlPatterns = {InitialiseAgents.API_PATTERN})
public class InitialiseAgents extends JPSAgent {

    private static final long serialVersionUID = 1L;

    private static final Logger LOGGER = LogManager.getLogger(InitialiseAgents.class);

    static final String API_PATTERN = "/InitialiseAgents";

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        Config.initProperties();

		// invoke all asynchronous agents so that they can be initialised
        AgentCaller.executeGet(Config.agentHttpUrlRNG);
		AgentCaller.executeGet(Config.agentHttpUrlMaxValue);
		AgentCaller.executeGet(Config.agentHttpUrlMinValue);
		AgentCaller.executeGet(Config.agentHttpUrlDifference);
        
        JSONObject response = new JSONObject();
        // if there's no error executing above codes, then all four agents are initialised successfully
        response.put("status", "Agents initialised successfully.");

        return response;
    }
}
