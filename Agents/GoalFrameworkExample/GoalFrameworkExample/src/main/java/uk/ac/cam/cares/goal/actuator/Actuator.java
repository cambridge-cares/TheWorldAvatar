package uk.ac.cam.cares.goal.actuator;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import uk.ac.cam.cares.goal.example.InitialiseInstances;
import uk.ac.cam.cares.goal.example.InputAgent;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import javax.servlet.annotation.WebServlet;

@WebServlet(urlPatterns =Actuator.URL_ACTUATOR)
public class Actuator extends JPSAgent {

    public static final String URL_ACTUATOR = "/Actuator";
    private static final long serialVersionUID = 1L;
    private static final Logger LOGGER = LogManager.getLogger(Actuator.class);

    public static final String FUNCTION_KEY = "function";
    public static final String FUNCTION_FILLTRUCK_VALUE = "FillTruck";
    public static final String FUNCTION_EMPTYTRUCK_VALUE = "EmptyTruck";
    private static String function = null;

    @Override
    public JSONObject processRequestParameters(JSONObject incomingRequestParams) {


        this.function = incomingRequestParams.getString(FUNCTION_KEY);
        if (function.equals(FUNCTION_FILLTRUCK_VALUE)){
            LOGGER.info("<ACTUATOR> Truck is being filled.");

            JSONObject requestParams = new JSONObject();

            try (CloseableHttpClient httpClient = HttpClients.createDefault()) {
                //POST request to input truck weight
                requestParams.put(InputAgent.INPUT_KEY, InputAgent.TRUCK_INSTANCE_KEY);
                requestParams.put(InputAgent.INPUT_NUMBER_KEY,"1000");
                HttpPost post = new HttpPost(InitialiseInstances.baseURL + InputAgent.URL_INPUTAGENT);
                StringEntity stringEntity = new StringEntity(requestParams.toString(), ContentType.APPLICATION_JSON);
                post.setEntity(stringEntity);

                try (CloseableHttpResponse httpResponse = httpClient.execute(post)) {
                    if (httpResponse.getStatusLine().getStatusCode() != 200) {
                        String msg = "Failed to update truck input weight with original request: "
                                + requestParams;
                        String body = EntityUtils.toString(httpResponse.getEntity());
                        LOGGER.error(msg);
                        throw new JPSRuntimeException(msg + " Error body: " + body);
                    }
                    String response = EntityUtils.toString(httpResponse.getEntity());
                    LOGGER.debug("Obtained http response from agent: " + response);
                }
            } catch (Exception e) {
                LOGGER.error("Failed to update input weight with original request: " + requestParams, e);
                throw new JPSRuntimeException("Failed to update input weight with original request: "
                        + requestParams, e);
            }
            LOGGER.info("<ACTUATOR> Truck is full.");
        }
        else if (function.equals(FUNCTION_EMPTYTRUCK_VALUE)){

            LOGGER.info("<ACTUATOR> Truck moving to MRF, emptying both bins and trucks");


            JSONObject requestParams = new JSONObject();

            try (CloseableHttpClient httpClient = HttpClients.createDefault()) {
                //POST request to input truck weight
                requestParams.put(InputAgent.INPUT_KEY, InputAgent.TRUCK_INSTANCE_KEY);
                requestParams.put(InputAgent.INPUT_NUMBER_KEY,"0");
                HttpPost post = new HttpPost(InitialiseInstances.baseURL + InputAgent.URL_INPUTAGENT);
                StringEntity stringEntity = new StringEntity(requestParams.toString(), ContentType.APPLICATION_JSON);
                post.setEntity(stringEntity);

                try (CloseableHttpResponse httpResponse = httpClient.execute(post)) {
                    if (httpResponse.getStatusLine().getStatusCode() != 200) {
                        String msg = "Failed to update truck input weight with original request: "
                                + requestParams;
                        String body = EntityUtils.toString(httpResponse.getEntity());
                        LOGGER.error(msg);
                        throw new JPSRuntimeException(msg + " Error body: " + body);
                    }
                    String response = EntityUtils.toString(httpResponse.getEntity());
                    LOGGER.debug("Obtained http response from agent: " + response);
                }
            } catch (Exception e) {
                LOGGER.error("Failed to update truck input with original request: " + requestParams, e);
                throw new JPSRuntimeException("Failed to update truck input weight with original request: "
                        + requestParams, e);
            }

            LOGGER.info("<ACTUATOR> Truck emptied");

            JSONObject binRequestParams = new JSONObject();

            try (CloseableHttpClient httpClient = HttpClients.createDefault()) {
                //POST request to input truck weight
                binRequestParams.put(InputAgent.INPUT_KEY, InputAgent.BIN_INSTANCE_KEY);
                binRequestParams.put(InputAgent.INPUT_NUMBER_KEY,"0");
                HttpPost post = new HttpPost(InitialiseInstances.baseURL + InputAgent.URL_INPUTAGENT);
                StringEntity stringEntity = new StringEntity(binRequestParams.toString(), ContentType.APPLICATION_JSON);
                post.setEntity(stringEntity);

                try (CloseableHttpResponse httpResponse = httpClient.execute(post)) {
                    if (httpResponse.getStatusLine().getStatusCode() != 200) {
                        String msg = "Failed to update bin input weight with original request: "
                                + binRequestParams;
                        String body = EntityUtils.toString(httpResponse.getEntity());
                        LOGGER.error(msg);
                        throw new JPSRuntimeException(msg + " Error body: " + body);
                    }
                    String response = EntityUtils.toString(httpResponse.getEntity());
                    LOGGER.debug("Obtained http response from agent: " + response);
                }
            } catch (Exception e) {
                LOGGER.error("Failed to update bin input weight with original request: " + binRequestParams, e);
                throw new JPSRuntimeException("Failed to update bin input weight with original request: "
                        + binRequestParams, e);
            }
            LOGGER.info("<ACTUATOR> Bin emptied");

        }
        String res_msg = "Actuator activated";
        LOGGER.info(res_msg);
        JSONObject response = new JSONObject();
        response.put("status", res_msg);
        return response;
    }
}

