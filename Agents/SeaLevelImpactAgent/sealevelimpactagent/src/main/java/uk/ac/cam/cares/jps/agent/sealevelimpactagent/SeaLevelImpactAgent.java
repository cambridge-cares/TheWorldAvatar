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
    private static Integer projectionyear = null;
    private static String confidence = null;
    private static Integer quantile = null;

    private static final String PROPETIES_PATH = "/inputs/config.properties";
    private final String SSP_SCENARIO_KEY = "ssp";
    private final String PROJECTIONYEAR_KEY = "projectionyear";
    private final String CONFIDENCE_KEY = "confidence";
    private final String QUANTILE_KEY = "quantile";
    private static final Logger LOGGER = LogManager.getLogger(SeaLevelImpactAgent.class);
    private EndpointConfig endpointConfig = new EndpointConfig();
    private String dbName;
    public static String citydbName =null;
    public static String heritagetreesTable = null;
    public static String historicsitesTable = null;
    public static String monumentsTable = null;
    public static String museumsTable = null;
    public static String touristattractionsTable = null;
    public static String landplotTable= null;
    public static String populationTable= null;
    public static String osm_streetTable= null;

    private String kgEndpoint;
    private RemoteStoreClient storeClient;
    private RemoteRDBStoreClient remoteRDBStoreClient;

    /**
     * Initialise agent
     */
    public void init() {
        readConfig();

        this.storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
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
            this.citydbName = prop.getProperty("citydb.name");
            this.heritagetreesTable= prop.getProperty("heritagetreesTable.name");
            this.historicsitesTable= prop.getProperty("historicsitesTable.name");
            this.monumentsTable= prop.getProperty("monumentsTable.name");
            this.museumsTable= prop.getProperty("museumsTable.name");
            this.touristattractionsTable= prop.getProperty("touristattractionsTable.name");
            this.landplotTable= prop.getProperty("landplotTable.name");
            this.populationTable= prop.getProperty("populationTable.name");
            this.osm_streetTable= prop.getProperty("osm_streetTable.name");
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
        this.projectionyear = requestParams.getInt(PROJECTIONYEAR_KEY);
        this.quantile = requestParams.getInt(QUANTILE_KEY);
        LOGGER.info("Successfully set SSP Scenario to " + sspScenario);
        LOGGER.info("Successfully set Projection year to "+projectionyear);
        LOGGER.info("Successfully set Confidence level to " + confidence);
        LOGGER.info("Successfully set Quantile to " + quantile);

        JSONObject response = new JSONObject();
        response.put("message", "Successfully set SSP Scenario to " + sspScenario);
        response.put("message", "Successfully set Projection Year to " + projectionyear);
        response.put("message", "Successfully set Confidence level to " + confidence);
        response.put("message", "Successfully set Quantile to " + quantile);

        try {
            init();
            ImpactAssessor impactAssessor = new ImpactAssessor();

            //Get sealevelchange UUID
            String seaLevelChangeUUID =impactAssessor.getSeaLevelChangeUUID(remoteRDBStoreClient, sspScenario, projectionyear, confidence, quantile);

            if (seaLevelChangeUUID.isEmpty()){response.put("message","No sealevelchange UUID");}
            LOGGER.info("Assessing sea-level rise impact for uuid "+ seaLevelChangeUUID);


            //Create SLR impact table
            impactAssessor.createTableIfNotExists(remoteRDBStoreClient);

            //Map population
            try {
                impactAssessor.mapPopulationAtRisk(remoteRDBStoreClient,seaLevelChangeUUID);
            }catch (Exception e) {
                LOGGER.info("Population failed to map: ", e);
            }

            try {
                impactAssessor.mapPopulationAtRisk(remoteRDBStoreClient,seaLevelChangeUUID);
            }catch (Exception e) {
                LOGGER.info("Population failed to map: ", e);
            }



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
            LOGGER.error("SSP is missing.");
            return false;
        }
        return true;
    }
}