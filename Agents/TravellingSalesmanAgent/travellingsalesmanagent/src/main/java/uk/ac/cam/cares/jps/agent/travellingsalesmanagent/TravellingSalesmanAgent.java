package uk.ac.cam.cares.jps.agent.travellingsalesmanagent;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.util.Map;
import java.util.Properties;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.cmclinnovations.stack.clients.geoserver.UpdatedGSVirtualTableEncoder;

@WebServlet(urlPatterns = "/runtsp")

public class TravellingSalesmanAgent extends JPSAgent {
    
    private static String tspFunction = null; 

    private static final String PROPETIES_PATH = "/inputs/config.properties";
    private final String FUNCTION_KEY = "function";


    private static final Logger LOGGER = LogManager.getLogger(TravellingSalesmanAgent.class);

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

        this.tspFunction = requestParams.getString(FUNCTION_KEY);

        LOGGER.info("Successfully set tspFunction to " + tspFunction);

        JSONObject response = new JSONObject();
        response.put("message", "Successfully set tspFunction to " + tspFunction);


        Path POI_PATH = Path.of("/inputs/"+tspFunction+"/POIqueries");
        Path EDGESTABLESQL_PATH = Path.of("/inputs/"+tspFunction+"/edgesSQLTable");


        try {
            init();
            // Read SPARQL and SQL files.
            Map<String, String> POImap = FileReader.readPOIsparql(POI_PATH);
            Map<String, String> EdgesTableSQLMap = FileReader.readEdgesTableSQL(EDGESTABLESQL_PATH);

            // Iterate through the SPARQL entries, execute the SPARQL queries and add POIs to the cumulative array
            JSONArray cumulativePOI = FileReader.getPOILocation(storeClient, POImap);

            // Split road into multiple smaller segment and find the nearest_node
            NearestNodeFinder nearestNodeFinder = new NearestNodeFinder();

            // Create a table to store nearest_node
            nearestNodeFinder.insertPoiData(remoteRDBStoreClient, cumulativePOI);

            //Create geoserver layer
            GeoServerClient geoServerClient = GeoServerClient.getInstance();
            String workspaceName= "twa";
            String schema = "public";
            geoServerClient.createWorkspace(workspaceName);

            UpdatedGSVirtualTableEncoder virtualTable = new UpdatedGSVirtualTableEncoder();
            GeoServerVectorSettings geoServerVectorSettings = new GeoServerVectorSettings();
            virtualTable.setSql("SELECT CONCAT('https://www.theworldavatar.com/kg/',poi_tsp_iri) as iri, poi_tsp_type, nearest_node, geom FROM poi_tsp_nearest_node");
            virtualTable.setEscapeSql(true);
            virtualTable.setName("poi_tsp_nearest_node");
            virtualTable.addVirtualTableGeometry("geom", "Geometry", "4326"); // geom needs to match the sql query
            geoServerVectorSettings.setVirtualTable(virtualTable);
            geoServerClient.createPostGISDataStore(workspaceName,"poi_tsp_nearest_node" , dbName, schema);
            geoServerClient.createPostGISLayer(workspaceName, dbName,"poi_tsp_nearest_node" ,geoServerVectorSettings);


            /**
             *  Loop through the edgeTable SQL and generate two geoserver layer for each edgeTableSQL
             *  - TSP_route:  Gives the geometry of the shortest path for all TSP points
             *  - TSP_seq:  Gives the sequence of order to visit all the TSP points
             */
            if (tspFunction.equals("UR")){
                TSPRouteGenerator tspRouteGenerator = new TSPRouteGenerator();
                for (Map.Entry<String, String> entry : EdgesTableSQLMap.entrySet()) {
                    String layerName = "TSP_"+entry.getKey();
                    String sql = entry.getValue();
                    tspRouteGenerator.generateTSPLayer(geoServerClient, workspaceName, schema, dbName, layerName, sql);
                    tspRouteGenerator.generateSequenceLayer(geoServerClient, workspaceName, schema, dbName, layerName, sql);
                }
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