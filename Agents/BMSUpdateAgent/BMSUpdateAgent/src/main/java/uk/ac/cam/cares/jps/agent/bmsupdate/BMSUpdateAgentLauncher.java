package uk.ac.cam.cares.jps.agent.bmsupdate;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Properties;

/**
 * This class acts as the entry point of the agent that accepts parameter requests to specific routes and achieve its task.
 *
 * @author sandradeng20
 */
@WebServlet(urlPatterns = {"/set"})
public class BMSUpdateAgentLauncher extends JPSAgent {

    private static final Logger LOGGER = LogManager.getLogger(BMSUpdateAgentLauncher.class);

    private final String KEY_DATAIRI = "dataIRI";
    private final String KEY_TEMPERATURE = "temperature";

    // TODO: update the way to get other agents and the blazegraph once they run in stack
    private final String KEY_CLIENT_PROPERTIES = "clientProperties";

    private String esphomeAgentToggle;
    private String esphomeUpdateAgentRetrieve;
    private String sparqlQueryEndpoint;
    private String sparqlUpdateEndpoint;

    /**
     * Update the temperature of the given set point iri.
     *
     * @param requestParams request parameters
     * @return A JSONObject contains the fan status and the updated temperature.
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        return super.processRequestParameters(requestParams);
    }

    /**
     * Update the temperature of the given set point iri.
     *
     * @param requestParams request parameters
     * @return A JSONObject contains the fan status and the updated temperature.
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSSS");
        String datetime = dateFormat.format(new Date());
        LOGGER.info("Request received at: {}", datetime);

        if (!validateInput(requestParams)) {
            throw new JPSRuntimeException("Unable to validate request sent to the agent.");
        }

        JSONObject result = new JSONObject();

        String dataIRI = requestParams.getString(KEY_DATAIRI);
        double temperature = requestParams.getDouble(KEY_TEMPERATURE);
        String clientPropertiesFile = System.getenv(requestParams.getString(KEY_CLIENT_PROPERTIES));

        LOGGER.info("receiving parameters: dataIRI: " + dataIRI + "\ntemperature: " + temperature + "\nclientPropertiesFile" + clientPropertiesFile);

        try {
            initProperties(clientPropertiesFile);
        } catch (IOException e) {
            throw new JPSRuntimeException("Unable to read the client properties file.");
        }

        RemoteStoreClient rsClient = new RemoteStoreClient(sparqlQueryEndpoint, sparqlUpdateEndpoint);

        BMSUpdateAgent bmsUpdateAgent = new BMSUpdateAgent();
        double originalTemperature = Double.NEGATIVE_INFINITY;
        try {
            originalTemperature = bmsUpdateAgent.getTemperatureInKg(dataIRI, rsClient);
            bmsUpdateAgent.setTemperatureInKg(dataIRI, temperature, rsClient);

            String fanStatus = bmsUpdateAgent.toggleFan(esphomeAgentToggle);
            bmsUpdateAgent.updateStatusInDb(esphomeUpdateAgentRetrieve);

            result.put("message", "The temperature has been set to " + temperature);
            result.put("fanStatus", fanStatus);

            LOGGER.info("Query finished with result: " + result);
            return result;
        } catch (JPSRuntimeException e) {
            String errorMessage = "Error occurred";
            if (e.getMessage().equals(bmsUpdateAgent.FAIL_TO_TOGGLE)) {
                bmsUpdateAgent.setTemperatureInKg(dataIRI, originalTemperature, rsClient);
                errorMessage = errorMessage + "; " + "Set point has been reset to " + originalTemperature;
            } else if (e.getMessage().equals(bmsUpdateAgent.FAIL_TO_PULL_DATA)) {
                bmsUpdateAgent.setTemperatureInKg(dataIRI, originalTemperature, rsClient);
                String fanStatus = bmsUpdateAgent.toggleFan(esphomeAgentToggle);
                errorMessage = errorMessage + "; " + "Set point has been reset to " + originalTemperature + ", and the fan status is " + fanStatus;
            }

            LOGGER.error(errorMessage);
            result.put("message", errorMessage);
            return result;
        }
    }

    /**
     * Check whether the request contains all the required parameters.
     *
     * @param requestParams Request parameters parsed from the body of POST request
     * @return validity of the request parameters
     */
    @Override
    public boolean validateInput(JSONObject requestParams) {
        if (requestParams.isEmpty()) {
            LOGGER.error("The request param is empty");
            return false;
        }

        if (!requestParams.has(KEY_DATAIRI)) {
            LOGGER.error(KEY_DATAIRI + " is missing");
            return false;
        }

        if (!requestParams.has(KEY_TEMPERATURE)) {
            LOGGER.error(KEY_TEMPERATURE + " is missing");
            return false;
        }

        try {
            requestParams.getDouble(KEY_TEMPERATURE);
        } catch (JSONException e) {
            LOGGER.error("The input temperature is not a double.");
            return false;
        }

        if (!requestParams.has(KEY_CLIENT_PROPERTIES)) {
            LOGGER.error(KEY_CLIENT_PROPERTIES + " is missing");
            return false;
        }

        String clientProperties = requestParams.getString(KEY_CLIENT_PROPERTIES);
        if (clientProperties.equals("CLIENT_PROPERTIES")) {
            if (System.getenv(clientProperties) == null) {
                LOGGER.error("CLIENT_PROPERTIES not set in environment variable, expect a valid file path");
                return false;
            }
        }

        return true;
    }

    /**
     * Init variables with the client property file.
     *
     * @param filepath Path of the client property file.
     * @throws IOException Throw exception when fail to read the property file.
     */
    private void initProperties(String filepath) throws IOException {
        File file = new File(filepath);
        if (!file.exists()) {
            throw new JPSRuntimeException("No properties file found at specified filepath: " + filepath);
        }

        try (InputStream input = new FileInputStream(file)) {

            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);

            if (prop.containsKey("esphome.agent.toggle")) {
                esphomeAgentToggle = prop.getProperty("esphome.agent.toggle");
            } else {
                throw new JPSRuntimeException("Properties file is missing \"esphome.agent.toggle=<esphome_agent_toggle>\"");
            }
            if (prop.containsKey("esphome.update.agent.retrieve")) {
                esphomeUpdateAgentRetrieve = prop.getProperty("esphome.update.agent.retrieve");
            } else {
                throw new JPSRuntimeException("Properties file is missing \"esphome.update.agent.retrieve=<esphome_update_agent_retrieve>\"");
            }
            if (prop.containsKey("sparql.query.endpoint")) {
                sparqlQueryEndpoint = prop.getProperty("sparql.query.endpoint");
            } else {
                throw new JPSRuntimeException("Properties file is missing \"sparql.query.endpoint=<sparql_endpoint>\"");
            }
            if (prop.containsKey("sparql.update.endpoint")) {
               sparqlUpdateEndpoint = prop.getProperty("sparql.update.endpoint");
            } else {
                throw new JPSRuntimeException("Properties file is missing \"sparql.update.endpoint=<sparql_endpoint>\"");
            }

            LOGGER.debug("Properties config: ");
            LOGGER.debug("esphomeToggle: " + esphomeAgentToggle);
            LOGGER.debug("esphomeRetrieve: " + esphomeUpdateAgentRetrieve);
            LOGGER.debug("sparqlQuery: " + sparqlQueryEndpoint);
            LOGGER.debug("sparqlUpdate: " + sparqlUpdateEndpoint);
        }

    }


}
