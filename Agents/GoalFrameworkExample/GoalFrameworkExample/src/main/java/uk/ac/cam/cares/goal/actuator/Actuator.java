package uk.ac.cam.cares.goal.actuator;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import uk.ac.cam.cares.goal.example.Config;
import uk.ac.cam.cares.goal.example.InitialiseInstances;
import uk.ac.cam.cares.goal.example.UpdateDerivations;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import javax.servlet.annotation.WebServlet;

@WebServlet(urlPatterns =Actuator.URL_ACTUATOR)
public class Actuator extends JPSAgent {

    public static final String URL_ACTUATOR = "/Actuator";


    private static final long serialVersionUID = 1L;
    private static final Logger LOGGER = LogManager.getLogger(Actuator.class);

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {

        System.out.println("Actuator did something");


        String res_msg = "Actuator did something";
        LOGGER.info(res_msg);

        JSONObject response = new JSONObject();
        response.put("status", res_msg);
        return response;
    }






}
