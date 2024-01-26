package uk.ac.cam.cares.jps.agent.isochroneagent;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
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
import com.cmclinnovations.stack.clients.ontop.OntopClient;

@WebServlet(urlPatterns = "/update")

public class IsochroneAgent extends JPSAgent {
    
    private static String isochroneFunction = null; 
    private static int timeThreshold; 
    private static int timeInterval; 
    private static final String PROPETIES_PATH = "/inputs/config.properties";
    private static final Path obdaFile = Path.of("/inputs/isochrone.obda");
    private final String FUNCTION_KEY = "function";
    private final String TIMETHRESHOLD_KEY = "timethreshold";
    private final String TIMEINTERVAL_KEY = "timeinterval";

    private static final Logger LOGGER = LogManager.getLogger(IsochroneAgent.class);

    private EndpointConfig endpointConfig = new EndpointConfig();
    private String dbName;
    private static String populationTables;
    private String kgEndpoint;
    private RemoteStoreClient storeClient;
    private RemoteRDBStoreClient remoteRDBStoreClient;
    public double segmentization_length;
    public ArrayList<String> populationTableList;

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
            this.segmentization_length = Double.parseDouble(prop.getProperty("segmentization_length"));
            this.kgEndpoint = prop.getProperty("kgEndpoint");

            this.populationTables = prop.getProperty("populationTables");
            // Split the string using the comma as the delimiter
            String[] tableNames = populationTables.split("\\s*,\\s*");
            this.populationTableList = new ArrayList<String>(Arrays.asList(tableNames));

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
     * 3) Segmentize road networks, find nearest_nodes of POIs
     * 4) Generate isochrones based on settings
     * 5) Map population to the isochrones
     * 6) Create geoserver layer using geoserverclient
     * 7) Upload .obda mapping using ontopclient
     * @param requestParams
     * @return
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {

        if (!validateInput(requestParams)) {
            throw new JPSRuntimeException("Unable to validate request sent to the agent.");
        }

        this.isochroneFunction = requestParams.getString(FUNCTION_KEY);
        this.timeThreshold = requestParams.getInt(TIMETHRESHOLD_KEY);
        this.timeInterval = requestParams.getInt(TIMEINTERVAL_KEY);
        LOGGER.info("Successfully set timeThreshold to " + timeThreshold);
        LOGGER.info("Successfully set timeInterval to " + timeInterval);
        LOGGER.info("Successfully set isochroneFunction to " + isochroneFunction);

        JSONObject response = new JSONObject();
        response.put("message", "Successfully set isochroneFunction to " + isochroneFunction+ ", timeInterval" + timeInterval+", timeThreshold" + timeThreshold);


        Path POI_PATH = Path.of("/inputs/"+isochroneFunction+"/POIqueries");
        Path EDGESTABLESQL_PATH = Path.of("/inputs/"+isochroneFunction+"/edgesSQLTable");


        try {
            init();
            // Read SPARQL and SQL files.
            Map<String, String> POImap = FileReader.readPOIsparql(POI_PATH);
            Map<String, String> EdgesTableSQLMap = FileReader.readEdgesTableSQL(EDGESTABLESQL_PATH);

            // Iterate through the SPARQL entries, execute the SPARQL queries and add POIs to the cumulative array
            JSONArray cumulativePOI = FileReader.getPOILocation(storeClient, POImap);

            // Split road into multiple smaller segment and find the nearest_node
            RouteSegmentization routeSegmentization = new RouteSegmentization();
            if (!routeSegmentization.doesTableExist(remoteRDBStoreClient)){
            //If segment table doesnt exist, segment table

            routeSegmentization.segmentize(remoteRDBStoreClient, segmentization_length);
                if (isochroneFunction.equals("UR")){
                    routeSegmentization.createFloodCost(remoteRDBStoreClient, 10);
                    routeSegmentization.createFloodCost(remoteRDBStoreClient, 30);
                    routeSegmentization.createFloodCost(remoteRDBStoreClient, 90);
                }
            }

            // Create a table to store nearest_node
            routeSegmentization.insertPoiData(remoteRDBStoreClient, cumulativePOI);


            // Isochrone generator SQL will take 4 inputs (remoteRDBStoreClient, timeThreshold, timeInterval, EdgesTableSQLMap)
            IsochroneGenerator isochroneGenerator = new IsochroneGenerator();
            isochroneGenerator.generateIsochrone(remoteRDBStoreClient, timeThreshold, timeInterval, EdgesTableSQLMap);
            isochroneGenerator.createIsochroneBuilding(remoteRDBStoreClient);

            // Population matcher
            PopulationMapper populationMapper = new PopulationMapper();
            populationMapper.addPostgisRasterAndGDALDriver(remoteRDBStoreClient, dbName);
            populationMapper.checkAndAddColumns(remoteRDBStoreClient, populationTableList);
            populationMapper.mapPopulation(remoteRDBStoreClient, populationTableList);

            //Create geoserver layer
            GeoServerClient geoServerClient = GeoServerClient.getInstance();
            String workspaceName= "twa";
            String schema = "public";
            geoServerClient.createWorkspace(workspaceName);

            
            UpdatedGSVirtualTableEncoder virtualTable = new UpdatedGSVirtualTableEncoder();
            GeoServerVectorSettings geoServerVectorSettings = new GeoServerVectorSettings();
            virtualTable.setSql("SELECT minute, transportmode, transportmode_iri, poi_type, CONCAT('https://www.theworldavatar.com/kg/ontoisochrone/',iri) as iri, CONCAT(transportmode,' (', poi_type,')') as name, roadcondition, roadcondition_iri, geometry_iri, "+populationTables+", ST_Force2D(geom) as geom FROM isochrone_aggregated");
            virtualTable.setEscapeSql(true);
            virtualTable.setName("isochrone_aggregated_virtualTable");
            virtualTable.addVirtualTableGeometry("geometry", "Geometry", "4326"); // geom needs to match the sql query
            geoServerVectorSettings.setVirtualTable(virtualTable);
            geoServerClient.createPostGISDataStore(workspaceName,"isochrone_aggregated" , dbName, schema);
            geoServerClient.createPostGISLayer(workspaceName, dbName,"isochrone_aggregated" ,geoServerVectorSettings);

            if (isochroneFunction.equals("UR")){
            UpdatedGSVirtualTableEncoder virtualTableUnreachable = new UpdatedGSVirtualTableEncoder();
            GeoServerVectorSettings geoServerVectorSettingsUnreachable = new GeoServerVectorSettings();
                virtualTableUnreachable.setSql("SELECT 'Unreachable Area' as name, af.minute, ABS(an.population - af.population) AS population, ST_UNION( ST_Intersection(ST_Difference(an.geom, af.geom), ST_ConcaveHull(ST_Points(fp30.geom),0.2, false)), ST_Difference(ST_ConcaveHull(ST_Points(fp30.geom),0.2, false), af.geom)) AS geom FROM (SELECT minute, ST_Union(geom) AS geom, SUM(population) AS population FROM isochrone_aggregated WHERE roadcondition = 'Flooded' GROUP BY minute) AS af JOIN (SELECT minute, ST_Union(geom) AS geom, SUM(population) AS population FROM isochrone_aggregated WHERE roadcondition = 'Normal' GROUP BY minute) AS an ON af.minute = an.minute CROSS JOIN flood_polygon_single_30cm AS fp30\n");
                virtualTableUnreachable.setEscapeSql(true);
                virtualTableUnreachable.setName("unreachable");
                virtualTableUnreachable.addVirtualTableGeometry("geom", "Geometry", "4326"); // geom needs to match the sql query
                geoServerVectorSettingsUnreachable.setVirtualTable(virtualTableUnreachable);
            geoServerClient.createPostGISDataStore(workspaceName,"unreachable" , dbName, schema);
            geoServerClient.createPostGISLayer(workspaceName, dbName,"unreachable" ,geoServerVectorSettingsUnreachable);


            }

            //Upload Isochrone Ontop mapping
            try {
                OntopClient ontopClient = OntopClient.getInstance();
                ontopClient.updateOBDA(obdaFile);
            } catch (Exception e) {
                System.out.println("Could not retrieve isochrone .obda file.");
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
        if (!requestParams.has(TIMEINTERVAL_KEY)) {
            LOGGER.error("TimeInterval is missing.");
            return false;
        }
        if (!requestParams.has(TIMETHRESHOLD_KEY)) {
            LOGGER.error("TimeThreshold is missing.");
            return false;
        }
        return true;
    }
}