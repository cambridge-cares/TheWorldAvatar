package uk.ac.cam.cares.jps.agent.sealevelimpactagent;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

@WebServlet(urlPatterns = "/slrimpact")

public class SeaLevelImpactAgent extends JPSAgent {
    
    private static String sspScenario = null;
    private static String confidence = null;
    private static Double quantile = null;

    private static final String PROPETIES_PATH = "/inputs/config.properties";
    private final String SSP_SCENARIO_KEY = "ssp";
    private final String CONFIDENCE_KEY = "confidence";
    private final String QUANTILE_KEY = "quantile";
    private static final Logger LOGGER = LogManager.getLogger(SeaLevelImpactAgent.class);
    private EndpointConfig endpointConfig = new EndpointConfig();
    private String dbName;
    private String kgEndpoint;
    private RemoteStoreClient storeClient;
    private RemoteRDBStoreClient remoteRDBStoreClient;

    /**
     * Initialise agent
     */
    public void init() {
        readConfig();

        if(!kgEndpoint.isEmpty() ){
            try {
                this.storeClient = new RemoteStoreClient(kgEndpoint, kgEndpoint);
            } catch (Exception e) {
             System.out.println(e + "Invalid blazegraph endpoint specified");
            }
        }else
        {   //Follow the running stack's blazegraph URL
            this.storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        }

        this.remoteRDBStoreClient = new RemoteRDBStoreClient(endpointConfig.getDbUrl(dbName), endpointConfig.getDbUser(), endpointConfig.getDbPassword());
    }

    /**
     * Read configuration settings from config.properties
     */
    public void readConfig() {
        try (InputStream input = FileReader.getStream(PROPETIES_PATH)) {
            Properties prop = new Properties();
            prop.load(input);
            this.dbName = prop.getProperty("db.name");
            this.kgEndpoint = prop.getProperty("kgEndpoint");

        } catch (FileNotFoundException e) {
            e.printStackTrace();
            throw new JPSRuntimeException("config.properties file not found");
        } catch (IOException e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }

    /**
     * Process request parameters and run functions within agent in the following flow
     * 1) Read files
     * 2) Retrieve POI locations from KG
     * 3) Find nearest_nodes of POIs
     * 4) Create TSP layer using geoserverclient - TSP Route, TSP sequence
     * @param requestParams
     * @return
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {

        if (!validateInput(requestParams)) {
            throw new JPSRuntimeException("Unable to validate request sent to the agent.");
        }

        this.sspScenario = requestParams.getString(SSP_SCENARIO_KEY);
        this.confidence = requestParams.getString(CONFIDENCE_KEY);
        this.quantile = requestParams.getDouble(QUANTILE_KEY);
        LOGGER.info("Successfully set SSP Scenario to " + sspScenario);
        LOGGER.info("Successfully set Confidence level to " + confidence);
        LOGGER.info("Successfully set Quantile to " + quantile);

        JSONObject response = new JSONObject();
        response.put("message", "Successfully set SSP Scenario to " + sspScenario);
        response.put("message", "Successfully set Confidence level to " + confidence);
        response.put("message", "Successfully set Quantile to " + quantile);

        try {
            init();

        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
        return response;
    }

    /**
     * Check if the JSONObject in the processRequestParameters inputs are correct or missing.
     * @param requestParams
     * @return
     * @throws BadRequestException
     */
    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (!requestParams.has(SSP_SCENARIO_KEY)) {
            LOGGER.error("Function is missing.");
            return false;
        }
        return true;
    }


}