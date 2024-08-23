package uk.ac.cam.cares.jps.agent.androidstatus;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.regex.Pattern;

@WebServlet(urlPatterns = {"/set", "/get"})
public class AndroidStatusAgent extends JPSAgent{
    private static final Logger LOGGER = LogManager.getLogger(AndroidStatusAgent.class);
    private String iri = "";
    private final String IRI_KEY = "iri";

    /**
     * Handle the POST and GET requests and route to specific handling logic based on the path.
     * @param requestParams Parameters sent in the request. Only /set expects a parameter iri
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

            this.iri = requestParams.getString(IRI_KEY);
            LOGGER.info("Successfully set iri to " + iri);

            JSONObject response = new JSONObject();
            response.put("message", "Successfully set iri to " + iri);
            return response;
        } else if (url.contains("get")) {
            JSONObject result = new JSONObject();
            result.put("iri", iri);
            LOGGER.info("Return: " + result);
            return result;
        }

        throw new JPSRuntimeException("Unknown Path");
    }

    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (!requestParams.has(IRI_KEY)) {
            LOGGER.error("iri is missing");
            return false;
        }

        return true;
    }
}
