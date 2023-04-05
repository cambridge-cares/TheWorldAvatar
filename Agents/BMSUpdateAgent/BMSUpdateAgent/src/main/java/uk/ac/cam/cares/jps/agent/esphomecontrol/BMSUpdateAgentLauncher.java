package uk.ac.cam.cares.jps.agent.esphomecontrol;

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

@WebServlet(urlPatterns = {"/set"})
public class BMSUpdateAgentLauncher extends JPSAgent {

    private static final Logger LOGGER = LogManager.getLogger(BMSUpdateAgentLauncher.class);

    private final String KEY_DATAIRI = "dataIRI";
    private final String KEY_TEMPERATURE = "temperature";

    // TODO: if this agent is running in stack, then the properties file set in env var doesn't make much sense..
    private final String KEY_CLIENT_PROPERTIES = "clientProperties";

    private String esphomeAgentToggle;
    private String esphomeUpdateAgentRetrieve;
    private String sparqlQueryEndpoint;
    private String sparqlUpdateEndpoint;

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        return super.processRequestParameters(requestParams);
    }

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

        RemoteStoreClient rsClient = setupKgConnection();

        BMSUpdateAgent bmsUpdateAgent = new BMSUpdateAgent(esphomeAgentToggle, esphomeUpdateAgentRetrieve);
        bmsUpdateAgent.setTemperatureInKg(dataIRI, temperature, rsClient);

        String fanStatus = bmsUpdateAgent.toggleFan();
        bmsUpdateAgent.updateStatusInDb();

        result.put("message", "The temperature has been set to " + temperature);
        result.put("fanStatus", fanStatus);

        return result;
    }

    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (requestParams.isEmpty()) {
            LOGGER.error("The request param is empty");
            return false;
        }

        if (requestParams.getString(KEY_DATAIRI).isEmpty()) {
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

        if (requestParams.getString(KEY_CLIENT_PROPERTIES).isEmpty()) {
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
                throw new JPSRuntimeException("Properties file is missing \"esphome.agent.toggle=<esphome_agent_toggle>\" ");
            }
            if (prop.containsKey("esphome.update.agent.retrieve")) {
                esphomeUpdateAgentRetrieve = prop.getProperty("esphome.update.agent.retrieve");
            } else {
                throw new JPSRuntimeException("Properties file is missing \"esphome.update.agent.retrieve=<esphome_update_agent_retrieve>\" ");
            }
            if (prop.containsKey("sparql.query.endpoint")) {
                sparqlQueryEndpoint = prop.getProperty("sparql.query.endpoint");
            } else {
                throw new JPSRuntimeException("Properties file is missing \"sparql.query.endpoint=<sparql_endpoint>\" ");
            }
            if (prop.containsKey("sparql.update.endpoint")) {
               sparqlUpdateEndpoint = prop.getProperty("sparql.update.endpoint");
            } else {
                throw new JPSRuntimeException("Properties file is missing \"sparql.update.endpoint=<sparql_endpoint>\" ");
            }

        }

    }

    private RemoteStoreClient setupKgConnection () {
        RemoteStoreClient rsClient = new RemoteStoreClient();

        rsClient.setQueryEndpoint(sparqlQueryEndpoint);
        rsClient.setUpdateEndpoint(sparqlUpdateEndpoint);

        return rsClient;
    }


}
