package uk.ac.cam.cares.jps.agent.networkanalysisagent;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;

@WebServlet(urlPatterns = "/runtc")

public class NetworkAnalysisAgent extends JPSAgent {
    
    private static String tcFunction = null;

    private static final String PROPETIES_PATH = "/inputs/config.properties";
    private final String FUNCTION_KEY = "function";
    private static final Logger LOGGER = LogManager.getLogger(NetworkAnalysisAgent.class);

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

        this.remoteRDBStoreClient = new RemoteRDBStoreClient(endpointConfig.getDbUrl(dbName),
                endpointConfig.getDbUser(), endpointConfig.getDbPassword());
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
     * 3) Find nearest_node of POI
     * 4) Create trip centrality table
     * 5) Create geoserver layer
     * @param requestParams
     * @return
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {

        if (!validateInput(requestParams)) {
            throw new JPSRuntimeException("Unable to validate request sent to the agent.");
        }

        this.tcFunction = requestParams.getString(FUNCTION_KEY);

        LOGGER.info("Successfully set tcFunction to " + tcFunction);

        JSONObject response = new JSONObject();
        response.put("message", "Successfully set tcFunction to " + tcFunction);


        Path POI_PATH = Path.of("/inputs/"+ tcFunction +"/POIqueries");
        Path EDGESTABLESQL_PATH = Path.of("/inputs/"+ tcFunction +"/edgesSQLTable");


        try {
            init();
            // Read SPARQL and SQL files.
            Map<String, String> POImap = FileReader.readPOIsparql(POI_PATH);
            Map<String, String> EdgesTableSQLMap = FileReader.readEdgesTableSQL(EDGESTABLESQL_PATH);

            // Iterate through the SPARQL entries, execute the SPARQL queries and add POIs to the cumulative array
            JSONArray cumulativePOI = FileReader.getPOILocation(storeClient, POImap);

            TripCentralityCalculator tripCentralityCalculator = new TripCentralityCalculator();
            
            if (tcFunction.equals("UR")){
                List<String> tcTableNameList = new ArrayList<>();

                for (Map.Entry<String, String> entry : EdgesTableSQLMap.entrySet()) {
                    String tableName = entry.getKey().toLowerCase();
                    String sql = entry.getValue();
                    tcTableNameList.add(tableName);
                    System.out.println("Begin generating tables for "+tableName+" with the edgeTableSQL as " + sql);
                    tripCentralityCalculator.calculateTripCentrality(remoteRDBStoreClient, cumulativePOI, tableName, sql);
                }

                //Create geoserver layer
                GeoServerClient geoServerClient = GeoServerClient.getInstance();
                String workspaceName= "twa";
                String schema = "public";
                geoServerClient.createWorkspace(workspaceName);
                tripCentralityCalculator.generateTCLayer(geoServerClient, workspaceName, schema, dbName, "tripcentrality_"+tcTableNameList.get(1),tcTableNameList.get(0), tcTableNameList.get(1));
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
        if (!requestParams.has(FUNCTION_KEY)) {
            LOGGER.error("Function is missing.");
            return false;
        }
        return true;
    }


}