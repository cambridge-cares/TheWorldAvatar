package uk.ac.cam.cares.jps.agent.dashboard.utils;

import com.google.gson.Gson;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.dashboard.DashboardAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.net.http.HttpResponse;
import java.util.HashMap;
import java.util.Map;

/**
 * A class that provides common methods to handle responses.
 *
 * @author qhouyee
 */
public class ResponseHelper {
    private static final Logger LOGGER = LogManager.getLogger(DashboardAgent.class);

    /**
     * Verifies the success of the request, which should return status code of 200.
     * If unsuccessful, agent will stop processing the request.
     *
     * @param response The response returned from the API.
     * @param errorMessage Error message to display if unsuccessful.
     */
    public static void verifySuccessfulRequest(HttpResponse response, String errorMessage) {
        if (response.statusCode() != 200) {
            LOGGER.fatal(errorMessage);
            throw new JPSRuntimeException(errorMessage);
        }
    }

    /**
     * Retrieves the body content of the response (usually in JSON), and process it into a Java Map.
     *
     * @param response The response returned from the API.
     * @return The response body as a map for easy access.
     */
    public static Map<String, Object> retrieveResponseBodyAsMap(HttpResponse response) {
        Gson gson = new Gson();
        // Although the method returns the result as a HashMap, it can be abstracted to its parent Map object
        return gson.fromJson(response.body().toString(), HashMap.class);
    }
}
