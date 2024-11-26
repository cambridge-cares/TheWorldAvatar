package uk.ac.cam.cares.jps.agent.trafficincident;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.time.*;
import java.util.HashSet;
import java.util.Properties;
import java.util.concurrent.TimeUnit;
import java.nio.file.Path;

import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.cmclinnovations.stack.clients.geoserver.UpdatedGSVirtualTableEncoder;
import com.cmclinnovations.stack.clients.ontop.OntopClient;


public class TrafficIncidentAgent implements Runnable {
    private final Logger LOGGER = LogManager.getLogger(TrafficIncidentAgent.class);

    public static final String API_VALUES = "TRAFFICINCIDENT_API_PROPERTIES";
    public static final String GET_READINGS_ERROR_MSG = "Error when getting reading. Retry after 10 seconds ...";
    public static final String CONNECTOR_ERROR_MSG = "Error when working with APIConnector.";
    
    public static final ZoneOffset offset = ZoneId.of("UTC+8").getRules().getOffset(Instant.now());
    long timestamp = OffsetDateTime.now(TrafficIncidentAgent.offset).toInstant().getEpochSecond();
    private HashSet<TrafficIncident> ongoingTrafficIncidentSet = new HashSet<>();
    private HashSet<TrafficIncident> newTrafficIncidentSet = new HashSet<>();

    private TrafficIncidentPostgresAgent postgresAgent;

    private static final String PROPERTIES_PATH = "/inputs/config.properties";
    private static final Path obdaFile = Path.of("/inputs/trafficincident.obda");
    private String dbName;
    private String apiProperties;
    private String api_url;
    private static boolean hasRun = false;

    @Override
    public void run() {
        JSONObject jsonMessage = new JSONObject();

        try (InputStream input = new FileInputStream(PROPERTIES_PATH)) {
            Properties prop = new Properties();
            prop.load(input);
            this.apiProperties = prop.getProperty("trafficincident.accountKey"); // Make sure to pass the key
            this.dbName = prop.getProperty("db.name");
            this.api_url =prop.getProperty("trafficincident.api_url");

        } catch (FileNotFoundException e) {
            e.printStackTrace();
            throw new JPSRuntimeException("config.properties file not found");
        } catch (IOException e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

        jsonMessage = initializeAgent(apiProperties, api_url);
        jsonMessage.accumulate("Result","values has been extracted");
    }


    /**
     * Initializes the agent by:
     *   - initialize APIconnector with @param apiProperties
     *   - extract readings from the APIconnector initialized
     *   - connect to Postgres and initialize table
     *   - store extracted readings to Postgres
     *   - convert latitude, longitude pair to Geometry point
     *   - compare between current and (previously deemed) ongoing incidents
     *       and mark ended incidents as complete
     */
    public JSONObject initializeAgent(String apiProperties, String api_url) {
        JSONObject jsonMessage = new JSONObject();
        // retrieve readings from data API and connector
        APIConnector connector;
        connector = new APIConnector(api_url, apiProperties);

        LOGGER.info("API Connector Object Initialized");
        jsonMessage.accumulate("Result","API Connector object Initialized");

        JSONObject readings;
        // when APIConnector fails, sleep for 10s and retry again
        while (true) {
            
            // timestamp records current time to get data from API
            this.timestamp = OffsetDateTime.now(TrafficIncidentAgent.offset).toInstant().getEpochSecond();
            readings = connector.getReadings();
            
            if (readings == null) {
                LOGGER.error(GET_READINGS_ERROR_MSG);
                try {
                    TimeUnit.SECONDS.sleep(10);
                } catch (InterruptedException err) {
                    continue;
                }
                continue;
            } else {
                break;
            }
        }

        LOGGER.info(String.format("Retrieved %d incident readings", readings.getJSONArray("value").length()));
        jsonMessage.accumulate("Result","Retrieved "+readings.getJSONArray("value").length()+" incident readings");

        // Get the property values and assign
        setRdbParameters();
        this.postgresAgent.connect();
        this.postgresAgent.createSchemaIfNotExists();

        this.ongoingTrafficIncidentSet = this.postgresAgent.retrieveOngoingIncidents();
        
        JSONArray jsArr = readings.getJSONArray("value");
        this.newTrafficIncidentSet = new HashSet<>();
        LOGGER.info("Adding new traffic incidents to Postgres:");
        for(int i=0; i<jsArr.length(); i++) {
            JSONObject currentEntry = jsArr.getJSONObject(i);
            // Note below the field name follows the API format by LTA data mall
            Double latitude = (Double) currentEntry.get("Latitude");
            Double longitude = (Double) currentEntry.get("Longitude");
            String incidentType = (String) currentEntry.get("Type");
            String message = (String) currentEntry.get("Message");
            Long ts = TrafficIncidentAgent.parseMessageStringToTimestamp(message);
            TrafficIncident curr = new TrafficIncident(incidentType, latitude, 
                longitude, message, ts, true);
            this.newTrafficIncidentSet.add(curr);
            // only update when the traffic incident not present
            if (!this.ongoingTrafficIncidentSet.contains(curr)) {
                // database needs to be created in PgAdmin beforehand
                this.postgresAgent.insertValuesIntoPostgres(curr);
                LOGGER.info(curr);
            }
        }
        this.postgresAgent.convertLongLatPairToGeom();
        LOGGER.info("Above is/are newly occurred traffic incidents.");
        
        LOGGER.info("Checking whether any traffic incident has ended ...");
        for (TrafficIncident ti : this.ongoingTrafficIncidentSet) {
            if (!this.newTrafficIncidentSet.contains(ti)) {
                ti.setEndTime(this.timestamp);
                ti.setStatus(false);
                LOGGER.info("Updating endtime for " + ti.toString());
                this.postgresAgent.updateTrafficIncidentEndTimeStatusPostgres(ti);
            }
        }
        LOGGER.info("Above is/are ended traffic incidents.");
        if (!hasRun) {
            createGeoserverLayer();
            createOntopMapping(); // Call the function
            hasRun = true;         // Set the flag to true after execution
        } else {
            System.out.println("createGeoserverLayer() and createOntopMapping() has already been run.");
        }

        this.postgresAgent.disconnect();
        return jsonMessage;
    }


    /**
     * Parses the @param message and returns the start time as specified
     */
    private static long parseMessageStringToTimestamp(String message) {
        // eg: (15/6)14:25 Roadworks on ECP (towards City) after Fort Rd Exit. Avoid lane 1.
        int year = Year.now().getValue();
        String dateTimeRawString = message.trim().split(" ")[0];
        String dateRawString = dateTimeRawString.split("\\)")[0];
        String timeRawString = dateTimeRawString.split("\\)")[1];
        int month = Integer.parseInt(dateRawString.split("/")[1]);
        int day = Integer.parseInt(dateRawString.split("/")[0].substring(1));
        int hour = Integer.parseInt(timeRawString.split(":")[0]);
        int minute = Integer.parseInt(timeRawString.split(":")[1]);
        // the parsed time is already in SGT and no need to offset
        OffsetDateTime result = OffsetDateTime.of(year, month, day, hour, minute, 0, 0, TrafficIncidentAgent.offset);
        return result.toInstant().getEpochSecond();
    }

    private void setRdbParameters() {
        EndpointConfig endpointConfig = new EndpointConfig();
        this.postgresAgent = new TrafficIncidentPostgresAgent(
            endpointConfig.getDbUrl(dbName),
            endpointConfig.getDbUser(),
            endpointConfig.getDbPassword());
    }


    private void createGeoserverLayer (){
        //Create geoserver layer
        GeoServerClient geoServerClient = GeoServerClient.getInstance();
        String workspaceName= "twa";
        String schema = "public";
        geoServerClient.createWorkspace(workspaceName);

        UpdatedGSVirtualTableEncoder virtualTable = new UpdatedGSVirtualTableEncoder();
        GeoServerVectorSettings geoServerVectorSettings = new GeoServerVectorSettings();
        virtualTable.setSql("SELECT \n" +
                        "    iri,\n" +
                        "    type AS name,\n" +
                        "    message AS description,\n" +
                        "    TO_TIMESTAMP(start_time) AT TIME ZONE 'Asia/Singapore' AS start_time,\n" +
                        "    CASE \n" +
                        "        WHEN end_time = 0 THEN NULL\n" +
                        "        ELSE TO_TIMESTAMP(end_time) AT TIME ZONE 'Asia/Singapore'\n" +
                        "    END AS end_time,\n" +
                        "    CASE \n" +
                        "        WHEN status = 't' THEN 'Ongoing'\n" +
                        "        ELSE 'Ended'\n" +
                        "    END AS status,\n" +
                        "    geom\n" +
                        "FROM trafficincident");
        virtualTable.setEscapeSql(true);
        virtualTable.setName("traffic_incident_virtual_table");
        virtualTable.addVirtualTableGeometry("location", "Geometry", "4326"); //
        geoServerVectorSettings.setVirtualTable(virtualTable);
        geoServerClient.createPostGISDataStore(workspaceName,"trafficincident", dbName, schema);
        geoServerClient.createPostGISLayer(workspaceName, dbName, "public", "trafficincident", geoServerVectorSettings);
    }

    private void createOntopMapping(){
        //Upload Isochrone Ontop mapping
        try {
            OntopClient ontopClient = OntopClient.getInstance();
            ontopClient.updateOBDA(obdaFile);
        } catch (Exception e) {
            System.out.println("Could not retrieve isochrone .obda file.");
        }
    }
}