package uk.ac.cam.cares.jps.agent.androidstatus;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.BadRequestException;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.regex.Pattern;

@WebServlet(urlPatterns = {"/set", "/get"})
public class AndroidStatusAgent extends JPSAgent{
    private static final Logger LOGGER = LogManager.getLogger(AndroidStatusAgent.class);
    private String equipmentIRI = "";
    private final String EQUIPMENT_IRI_KEY = "equipmentIRI";

    /**
     * Handle the POST and GET requests and route to specific handling logic based on the path.
     * @param requestParams Parameters sent in the request. Only /set expects a parameter equipmentIRI
     * @param request HttpServletRequest instance
     * @return result in JSONObject format.
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSSS");
        String datetime = dateFormat.format(new Date());
        LOGGER.info("Request received at: {}", datetime);

        String url = request.getRequestURI();
        if (url.contains("?")) url = url.split(Pattern.quote("?"))[0];

        if (url.contains("set")) {
            if (!validateInput(requestParams)) {
                throw new JPSRuntimeException("Unable to validate request sent to the agent.");
            }

            this.equipmentIRI = requestParams.getString(EQUIPMENT_IRI_KEY);
            LOGGER.info("Successfully set equipment iri to " + equipmentIRI);

            JSONObject response = new JSONObject();
            response.put("message", "Successfully set equipment iri to " + equipmentIRI);
            return response;
        } else if (url.contains("get")) {
            JSONObject result = new JSONObject();
            result.put("equipmentIRI", equipmentIRI);
            LOGGER.info("Return: " + result);
            return result;
        }

        throw new JPSRuntimeException("Unknown Path");
    }

    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (!requestParams.has(EQUIPMENT_IRI_KEY)) {
            LOGGER.error("equipmentIRI is missing");
            return false;
        }

        return true;
    }
}
