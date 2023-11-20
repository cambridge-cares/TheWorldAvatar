package uk.ac.cam.cares.jps.agent.bmsupdate;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Properties;
import java.util.regex.Pattern;

/**
 * This class acts as the entry point of the agent that accepts parameter requests to specific routes and achieve its task.
 *
 * @author sandradeng20
 */
@WebServlet(urlPatterns = {"/set", "/wacnet/write", "/updateTriples", "/updatePresentValue"})
public class BMSUpdateAgentLauncher extends JPSAgent {

    private static final Logger LOGGER = LogManager.getLogger(BMSUpdateAgentLauncher.class);

    private final String KEY_DATAIRI = "dataIRI";
    private final String KEY_TEMPERATURE = "temperature";
    private final String KEY_VALUE = "value";
    private final String KEY_BACNETOBJECTID = "bacnetObjectId";
    private final String KEY_BACNETDEVICEID = "bacnetDeviceId";
    private final String KEY_INSERT = "INSERT";
    private final String KEY_DELETE = "DELETE";
    private final String KEY_TRIGGER_VALUE = "triggerValue";

    private final String KEY_CLIENT_PROPERTIES = "clientProperties";

    private final String ENV_WACNET_API_PROPERTIES = "WACNET_API_PROPERTIES";

    RemoteRDBStoreClient RDBClient;
    RemoteStoreClient rsClient = new RemoteStoreClient();

    private String esphomeAgentToggle;
    private String esphomeUpdateAgentRetrieve;
    private String sparqlQueryEndpoint;
    private String sparqlUpdateEndpoint;
    private String sparqlUsername;
    private String sparqlPassword;
    private String dbUrl;
    private String dbUser;
    private String dbPassword;


    /**
     * Process request and execute route accordingly
     *
     * @param requestParams Request parameters parsed from the body of POST request
     * @return A JSONObject containing results of executed route
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSSS");
        String datetime = dateFormat.format(new Date());
        LOGGER.info("Request received at: {}", datetime);

        // Get the requested route
        String originalUrl = request.getRequestURI();
        JSONObject result = executeRoute(originalUrl, requestParams);
        return result;  
    }

    /**
     * execute route according to URL and request params
     * @param originalUrl the original url received
     * @param requestParams Request parameters parsed from the body of POST request
     * @return results of executed route
     */
    public JSONObject executeRoute(String originalUrl, JSONObject requestParams) {
        JSONObject result = new JSONObject();
        String url = originalUrl.substring(originalUrl.lastIndexOf("/"), originalUrl.length());
        LOGGER.info("The url is " + url);
        switch (url) {
            case "/set": {
                result = executeSet(requestParams);
                break;
            }
            case "/write": {
                //wacnet API route
                if (originalUrl.contains("/wacnet/write")) {
                    result = executeWriteWacnet(requestParams);
                }
                break;
            }
            case "/updateTriples": {
                //updateTriples route
                if (originalUrl.contains("/updateTriples")) {
                    result = executeUpdateTriples(requestParams);
                }
                break;
            }
            case "/updatePresentValue": {
                result = executeUpdatePresentValue(requestParams);
                break;
            }
        }
        return result;
    }

    /**
     * execute set route
     *
     * @param requestParams Request parameters parsed from the body of POST request
     * @return results of executeSet
     */
    public JSONObject executeSet(JSONObject requestParams) {
        JSONObject result = new JSONObject();
        String dataIRI = requestParams.getString(KEY_DATAIRI);
        double temperature = requestParams.getDouble(KEY_TEMPERATURE);
        String clientPropertiesFile = System.getenv(requestParams.getString(KEY_CLIENT_PROPERTIES));
        LOGGER.info("receiving parameters: dataIRI: " + dataIRI + "\ntemperature: " + temperature + "\nclientPropertiesFile" + clientPropertiesFile);
        try {
            initSetProperties(clientPropertiesFile);
        } catch (IOException e) {
            throw new JPSRuntimeException("Unable to read the client properties file.");
        }
        RemoteStoreClient rsClient = new RemoteStoreClient(sparqlQueryEndpoint, sparqlUpdateEndpoint);
        if (sparqlUsername != null && sparqlPassword != null) {
            rsClient.setUser(sparqlUsername);
            rsClient.setPassword(sparqlPassword);
        }
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
        return result;
    }

    /**
     * execute write wacnet route
     * @param requestParams Request parameters parsed from the body of POST request
     * @return results of executed route
     */
    public JSONObject executeWriteWacnet(JSONObject requestParams) {
        JSONObject result = new JSONObject();
        if (requestParams.has(KEY_DATAIRI) && requestParams.has(KEY_VALUE)) {
            String dataIRI = requestParams.getString(KEY_DATAIRI);
            Double value = requestParams.getDouble(KEY_VALUE);
            String clientPropertiesFile = System.getenv(requestParams.getString(KEY_CLIENT_PROPERTIES));
            BMSUpdateAgent bmsUpdateAgent = new BMSUpdateAgent();
            try {
                initSparqlProperties(clientPropertiesFile);
            } catch (IOException e) {
                throw new JPSRuntimeException("Unable to read the client properties file.", e);
            }
            rsClient = new RemoteStoreClient(sparqlQueryEndpoint, sparqlUpdateEndpoint);
            if (sparqlUsername != null && sparqlPassword != null) {
                rsClient.setUser(sparqlUsername);
                rsClient.setPassword(sparqlPassword);
            }
            //query for Bacnet Device ID linked to data IRI
            String deviceId = bmsUpdateAgent.getDeviceId(dataIRI, rsClient);
            //query for Bacnet Object ID linked to data IRI
            String objectId = bmsUpdateAgent.getObjectId(dataIRI, rsClient);
            //send request via API
            try {
                BMSWacnetAPIConnector wacnetAPIConnector = new BMSWacnetAPIConnector(System.getenv(ENV_WACNET_API_PROPERTIES));
                result = wacnetAPIConnector.writePresentValue(deviceId, objectId, value);
                return result;
            } catch (Exception e) {
                throw new JPSRuntimeException("Unable to write present value to Bacnet Object: " + objectId, e);
            }
        } else if (requestParams.has(KEY_BACNETDEVICEID) && requestParams.has(KEY_BACNETOBJECTID) && requestParams.has(KEY_VALUE)) {
            //retrieve Bacnet Device ID from request parameters
            String deviceId = requestParams.getString(KEY_BACNETDEVICEID);
            //retrieve Bacnet Object ID from request parameters
            String objectId = requestParams.getString(KEY_BACNETOBJECTID);
            Double value = requestParams.getDouble(KEY_VALUE);
            //send request via API
            try {
                BMSWacnetAPIConnector wacnetAPIConnector = new BMSWacnetAPIConnector(System.getenv(ENV_WACNET_API_PROPERTIES));
                result = wacnetAPIConnector.writePresentValue(deviceId, objectId, value);
                return result;
            } catch (IOException e) {
                throw new JPSRuntimeException("Unable to write present value to Bacnet Object: " + objectId, e);
            }
        } else {
            throw new JPSRuntimeException("missing keys and values in request!");
        }
    }

    /**
     * execute update triples route
     * @param requestParams Request parameters parsed from the body of POST request
     * @return results of executed route
     */
    public JSONObject executeUpdateTriples(JSONObject requestParams) {
        JSONObject result = new JSONObject();
        JSONArray checks = requestParams.getJSONArray("checks");
        for (int i = 0; i < checks.length(); i++){
            JSONObject checkAndUpdateParameters = checks.getJSONObject(i);
            String dataIRI = checkAndUpdateParameters.getString(KEY_DATAIRI);
            String triggerValue = checkAndUpdateParameters.getString(KEY_TRIGGER_VALUE);
            String insertString = null;
            String deleteString = null;
            String clientPropertiesFile = System.getenv(checkAndUpdateParameters.getString(KEY_CLIENT_PROPERTIES));
            BMSUpdateAgent bmsUpdateAgent = new BMSUpdateAgent();
            try {
                initUpdateTriplesProperties(clientPropertiesFile);
            } catch (IOException e) {
                throw new JPSRuntimeException("Unable to read the client properties file.", e);
            }
            rsClient = new RemoteStoreClient(sparqlQueryEndpoint, sparqlUpdateEndpoint);
            if (sparqlUsername != null && sparqlPassword != null) {
                rsClient.setUser(sparqlUsername);
                rsClient.setPassword(sparqlPassword);
            }
            RDBClient = new RemoteRDBStoreClient(dbUrl, dbUser, dbPassword);
            JSONObject subMessage = new JSONObject();
            if (checkAndUpdateParameters.has(KEY_INSERT) && checkAndUpdateParameters.has(KEY_DELETE)) {
                insertString = checkAndUpdateParameters.getString(KEY_INSERT);
                deleteString = checkAndUpdateParameters.getString(KEY_DELETE);
                // pass data IRI, remote store client, remote RDB client. triggerValue, insertString, deleteString to checkInsertAndDelete method
                subMessage = bmsUpdateAgent.checkInsertAndDelete(dataIRI, rsClient, RDBClient, triggerValue, insertString, deleteString);
                result.put("message" + i, subMessage);
            } else if (checkAndUpdateParameters.has(KEY_INSERT) && !checkAndUpdateParameters.has(KEY_DELETE)) {
                insertString = checkAndUpdateParameters.getString(KEY_INSERT);
                subMessage = bmsUpdateAgent.checkInsert(dataIRI, rsClient, RDBClient, triggerValue, insertString);
                result.put("message" + i, subMessage);
                // pass data IRI, remote store client, remote RDB client. triggerValue, insertString to checkInsert method
            } else if (checkAndUpdateParameters.has(KEY_DELETE) && !checkAndUpdateParameters.has(KEY_INSERT)) {
                deleteString = checkAndUpdateParameters.getString(KEY_DELETE);
                // pass data IRI, remote store client, remote RDB client. triggerValue, deleteString to checkDelete method
                subMessage = bmsUpdateAgent.checkDelete(dataIRI, rsClient, RDBClient, triggerValue, deleteString);
                result.put("message" + i, subMessage);
            }
        }
        return result;
    }

    /**
     * execute update present value route
     * @param requestParams Request parameters parsed from the body of POST request
     * @return results of executed route
     */
    public JSONObject executeUpdatePresentValue(JSONObject requestParams) {
        JSONObject result = new JSONObject();
        JSONObject subMessage = new JSONObject();
        JSONArray checks = requestParams.getJSONArray("checks");
        for (int i = 0; i < checks.length(); i++){
            JSONObject checkAndUpdateParameters = checks.getJSONObject(i);
            String dataIRI = checkAndUpdateParameters.getString(KEY_DATAIRI);
            String clientPropertiesFile = System.getenv(checkAndUpdateParameters.getString(KEY_CLIENT_PROPERTIES));
            BMSUpdateAgent bmsUpdateAgent = new BMSUpdateAgent();
            try {
                initSparqlProperties(clientPropertiesFile);
            } catch (IOException e) {
                throw new JPSRuntimeException("Unable to read the client properties file.", e);
            }
            rsClient = new RemoteStoreClient(sparqlQueryEndpoint, sparqlUpdateEndpoint);
            if (sparqlUsername != null && sparqlPassword != null) {
                rsClient.setUser(sparqlUsername);
                rsClient.setPassword(sparqlPassword);
            }
                //query for Bacnet Device ID linked to data IRI
                String deviceId = bmsUpdateAgent.getDeviceId(dataIRI, rsClient);
                //query for Bacnet Object ID linked to data IRI
                String objectId = bmsUpdateAgent.getObjectId(dataIRI, rsClient);
                //send request via API
                try {
                    BMSWacnetAPIConnector wacnetAPIConnector = new BMSWacnetAPIConnector(System.getenv(ENV_WACNET_API_PROPERTIES));
                    Double presentValue;
                    presentValue = wacnetAPIConnector.readPresentValue(deviceId, objectId);
                    subMessage = bmsUpdateAgent.setNumericalValue(dataIRI, presentValue, rsClient);
                    result.put("message" + i, subMessage);
                } catch (IOException e) {
                    throw new JPSRuntimeException("Unable to update the present value of " + dataIRI + " that has a Bacnet Object ID of " + objectId, e);
                }
            }
        return result;
        }

    /**
     * Init variables with the client property file.
     *
     * @param filepath Path of the client property file.
     * @throws IOException Throw exception when fail to read the property file.
     */
    private void initSetProperties(String filepath) throws IOException {
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
            if (prop.containsKey("sparql.username")) {
               sparqlUsername = prop.getProperty("sparql.username");
            }
            if (prop.containsKey("sparql.password")) {
               sparqlPassword = prop.getProperty("sparql.password");
            }
            LOGGER.debug("Properties config: ");
            LOGGER.debug("esphomeToggle: " + esphomeAgentToggle);
            LOGGER.debug("esphomeRetrieve: " + esphomeUpdateAgentRetrieve);
            LOGGER.debug("sparqlQuery: " + sparqlQueryEndpoint);
            LOGGER.debug("sparqlUpdate: " + sparqlUpdateEndpoint);
            LOGGER.debug("sparqlUsername: " + sparqlUsername);
            LOGGER.debug("sparqlPassword: " + sparqlPassword);
        }

    }

    /**
     * Init variables with the client property file.
     *
     * @param filepath Path of the client property file.
     * @throws IOException Throw exception when fail to read the property file.
     */
    private void initSparqlProperties(String filepath) throws IOException {
        File file = new File(filepath);
        if (!file.exists()) {
            throw new JPSRuntimeException("No properties file found at specified filepath: " + filepath);
        }

        try (InputStream input = new FileInputStream(file)) {

            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);

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
            if (prop.containsKey("sparql.username")) {
                sparqlUsername = prop.getProperty("sparql.username");
            }
            if (prop.containsKey("sparql.password")) {
               sparqlPassword = prop.getProperty("sparql.password");
            }

            LOGGER.debug("Properties config: ");
            LOGGER.debug("sparqlQuery: " + sparqlQueryEndpoint);
            LOGGER.debug("sparqlUpdate: " + sparqlUpdateEndpoint);
            LOGGER.debug("sparqlUsername: " + sparqlUsername);
            LOGGER.debug("sparqlPassword: " + sparqlPassword);
        }

    }

    /**
     * Init variables with the client property file.
     *
     * @param filepath Path of the client property file.
     * @throws IOException Throw exception when fail to read the property file.
     */
    private void initUpdateTriplesProperties(String filepath) throws IOException {
        File file = new File(filepath);
        if (!file.exists()) {
            throw new JPSRuntimeException("No properties file found at specified filepath: " + filepath);
        }

        try (InputStream input = new FileInputStream(file)) {

            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);

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
            if (prop.containsKey("sparql.username")) {
                sparqlUsername = prop.getProperty("sparql.username");
            }
            if (prop.containsKey("sparql.password")) {
               sparqlPassword = prop.getProperty("sparql.password");
            }
            if (prop.containsKey("db.url")) {
               dbUrl = prop.getProperty("db.url");
            } else {
                throw new JPSRuntimeException("Properties file is missing \"db.url=<db_url>\"");
            }
            if (prop.containsKey("db.user")) {
               dbUser = prop.getProperty("db.user");
            } else {
                throw new JPSRuntimeException("Properties file is missing \"db.user=<db_user>\"");
            }
            if (prop.containsKey("db.password")) {
               dbPassword = prop.getProperty("db.password");
            } else {
                throw new JPSRuntimeException("Properties file is missing \"db.password=<db_password>\"");
            }

            LOGGER.debug("Properties config: ");
            LOGGER.debug("sparqlQuery: " + sparqlQueryEndpoint);
            LOGGER.debug("sparqlUpdate: " + sparqlUpdateEndpoint);
            LOGGER.debug("sparqlUsername: " + sparqlUsername);
            LOGGER.debug("sparqlPassword: " + sparqlPassword);
            LOGGER.debug("dbUrl: " + dbUrl);
            LOGGER.debug("dbUser: " + dbUser);
            LOGGER.debug("dbPassword: " + dbPassword);
        }
    }
}
