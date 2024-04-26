package uk.ac.cam.cares.goal.actuator;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;

import javax.servlet.annotation.WebServlet;

@WebServlet(urlPatterns =Actuator.URL_ACTUATOR)
public class Actuator extends JPSAgent {

    public static final String URL_ACTUATOR = "/Actuator";


    private static final long serialVersionUID = 1L;
    private static final Logger LOGGER = LogManager.getLogger(Actuator.class);

    private final String FUNCTION_KEY = "function";
    private static String function = null;

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {

        this.function = requestParams.getString(FUNCTION_KEY);
        if (function.equals("FillTruck")){
            System.out.println("Truck is being filled.");
            System.out.println("Truck is full");

            //POST request to input truck weight

            //Update Derivations


        }else if (function.equals("EmptyTruck")){
            System.out.println("Truck is moving to MRF.");
        }

        LOGGER.info(res_msg);
        JSONObject response = new JSONObject();
        response.put("status", res_msg);
        return response;
    }






}
