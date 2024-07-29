package uk.ac.cam.cares.jps.agent.trajectoryqueryagent;

import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.cmclinnovations.stack.clients.geoserver.UpdatedGSVirtualTableEncoder;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;


@WebServlet(urlPatterns = "/createlayer")
public class TrajectoryQueryAgent extends JPSAgent {
    private KGQueryClient kgQueryClient;
    private RemoteStoreClient storeClient;
    private RemoteRDBStoreClient remoteRDBStoreClient;
    private final String USER_ID = "userID";
    private static final Logger LOGGER = LogManager.getLogger(TrajectoryQueryAgent.class);
    private String userID;
    private String userAgentUrl;


    public void init() {
        EndpointConfig endpointConfig = new EndpointConfig();
        storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        kgQueryClient = new KGQueryClient(storeClient);
        remoteRDBStoreClient = new RemoteRDBStoreClient(endpointConfig.getDburl(), endpointConfig.getDbuser(), endpointConfig.getDbpassword());
        userAgentUrl = endpointConfig.getUserAgentUrl();
    }

    /**
     * 1) Receive userID
     * 2) SPARQL query for pointIRI based on userID
     * 3) Create geoserver layer
     * 4) Return pointIRI to application as response
     *
     * @param requestParams
     * @return
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {

        if (!validateInput(requestParams)) {
            throw new JPSRuntimeException("Unable to validate request sent to the agent.");
        }

        //Retrieve params (Although currently receiver deviceID)
        this.userID = requestParams.getString(USER_ID);

        //Retrieve phoneId from User Agent with userId
        String getPhoneIds = userAgentUrl + "getPhoneIds";
        JSONObject phoneIdResponse = new JSONObject(AgentCaller.executeGet(getPhoneIds, "userId", userID));
        JSONArray phoneIds = phoneIdResponse.getJSONArray("PhoneIds");
        if (phoneIds.isEmpty()) {
            JSONObject response = new JSONObject();
            response.put("message", "No phone id on this user.");
            return response;
        }

        List<String> pointIRIs = new ArrayList<>();
        List<String> altitudeIRIs = new ArrayList<>();
        List<String> speedIRIs = new ArrayList<>();
        List<String> bearingIRIs = new ArrayList<>();

        for (int i = 0; i < phoneIdResponse.getJSONArray("PhoneIds").length(); i ++) {
            String phoneIri = phoneIdResponse.getJSONArray("PhoneIds").getString(i);

            //SPARQL query for pointIRI based on userID - Note currently userID is deviceID, needs to be changed
            Node smartphoneIRI = NodeFactory.createURI(phoneIri);
            String pointIRI = kgQueryClient.getIRIfromJSONarray(kgQueryClient.getPointIRIArray(smartphoneIRI));
            String altitudeIRI = kgQueryClient.getIRIfromJSONarray(kgQueryClient.getAltitudeIRIArray(smartphoneIRI));
            String speedIRI = kgQueryClient.getIRIfromJSONarray(kgQueryClient.getSpeedIRIArray(smartphoneIRI));
            String bearingIRI = kgQueryClient.getIRIfromJSONarray(kgQueryClient.getBearingIRIArray(smartphoneIRI));

            if (pointIRI == null || altitudeIRI == null || speedIRI == null || bearingIRI == null) {
                continue;
            }

            pointIRIs.add(pointIRI);
            altitudeIRIs.add(altitudeIRI);
            speedIRIs.add(speedIRI);
            bearingIRIs.add(bearingIRI);
        }

        if (pointIRIs.isEmpty() || altitudeIRIs.isEmpty() || speedIRIs.isEmpty() || bearingIRIs.isEmpty()) {
            JSONObject response = new JSONObject();
            response.put("message", "Measurement IRI is missing");
            return response;
        }

        //Create Geoserver layer
        createGeoserver(pointIRIs.get(0), altitudeIRIs.get(0), speedIRIs.get(0), bearingIRIs.get(0));

        //Return pointIRI, altitudeIRI, speedIRI, bearingIRI to app as response
        JSONObject response = new JSONObject();
        response.put("message", "Layer created");
        response.put("pointIRI", String.join(",", pointIRIs));
        response.put("altitudeIRI", String.join(",", altitudeIRIs));
        response.put("speedIRI", String.join(",", speedIRIs));
        response.put("bearingIRI", String.join(",", bearingIRIs));
        return response;
    }


    private void createGeoserver(String pointIRI, String altitudeIRI, String speedIRI, String bearingIRI) {
        //Function retrieves column_name from dbTable given IRI
        String getColumnNameFunction = "CREATE OR REPLACE FUNCTION getColumnName(iri VARCHAR)\n" +
                "RETURNS VARCHAR AS\n" +
                "$$\n" +
                "DECLARE\n" +
                "    column_name VARCHAR;\n" +
                "BEGIN\n" +
                "    SELECT \"columnName\" INTO column_name FROM \"dbTable\" WHERE \"dataIRI\" = iri;\n" +
                "    RETURN column_name;\n" +
                "END;\n" +
                "$$\n" +
                "LANGUAGE plpgsql;";

        //Function retrieves table_name from dbTable given IRI
        String getTableNameFunction = "CREATE OR REPLACE FUNCTION getTableName(iri VARCHAR)\n" +
                "RETURNS VARCHAR AS\n" +
                "$$\n" +
                "DECLARE\n" +
                "    table_name VARCHAR;\n" +
                "BEGIN\n" +
                "    SELECT \"tableName\" INTO table_name FROM \"dbTable\" WHERE \"dataIRI\" = iri;\n" +
                "    RETURN table_name;\n" +
                "END;\n" +
                "$$\n" +
                "LANGUAGE plpgsql;";

        //Function retrieves the combined locationTable given pointiris, speediris, altitudeiris, bearingiris. Each iris is a string of iri separated by ','
        //example of pointiris: 'https://www.theworldavatar.com/kg/sensorloggerapp/point_1,https://www.theworldavatar.com/kg/sensorloggerapp/point_2'
        String getLocationTableFunction = "CREATE OR REPLACE FUNCTION getLocationTable(pointiri TEXT, speediri TEXT, altitudeiri TEXT, bearingiri TEXT)\n" +
                "RETURNS TABLE (\"time\" timestamptz, \"geom\" geometry, \"speed\" double precision, \"altitude\" double precision, \"bearing\" double precision) AS $$\n" +
                "DECLARE\n" +
                "pointIriArray TEXT[];\n" +
                "altitudeIriArray TEXT[];\n" +
                "speedIriArray TEXT[];\n" +
                "bearingIriArray TEXT[];\n" +
                "query TEXT := '';\n" +
                "BEGIN\n" +
                "pointIriArray := string_to_array(pointiri, ',');\n" +
                "altitudeIriArray := string_to_array(altitudeiri, ',');\n" +
                "speedIriArray := string_to_array(speediri, ',');\n" +
                "bearingIriArray := string_to_array(bearingiri, ',');\n" +
                "\n" +
                "FOR i IN 1..array_length(pointIriArray, 1) LOOP\n" +
                "    IF i > 1 THEN\n" +
                "            query := query || ' UNION ALL ';\n" +
                "    END IF;\n" +
                "    query := query || format('SELECT time, %I AS geom, %I AS speed, %I AS altitude, %I AS bearing FROM %I', getColumnName(pointIriArray[i]), getColumnName(speedirIArray[i]), getColumnName(altitudeIriArray[i]), getColumnName(bearingIriArray[i]), getTableName(pointIriArray[i]));\n" +
                "END LOOP;\n" +
                "RETURN QUERY EXECUTE query;\n" +
                "END $$ LANGUAGE plpgsql;";

        try (Connection connection = remoteRDBStoreClient.getConnection()) {
            executeSql(connection, getColumnNameFunction);
            executeSql(connection, getTableNameFunction);
            executeSql(connection, getLocationTableFunction);
            System.out.println("Created get_geometry_table function.");
        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

        //Create geoserver point layer
        GeoServerClient geoServerClient = GeoServerClient.getInstance();
        String workspaceName = "twa";
        String schema = "public";
        String dbName = "postgres";
        geoServerClient.createWorkspace(workspaceName);
        UpdatedGSVirtualTableEncoder virtualTable = new UpdatedGSVirtualTableEncoder();
        GeoServerVectorSettings geoServerVectorSettings = new GeoServerVectorSettings();
        virtualTable.setSql("SELECT timeseries.time AS time,\n" +
                "timeseries.speed AS speed,\n" +
                "timeseries.altitude AS altitude,\n" +
                "timeseries.geom AS geom,\n" +
                "timeseries.bearing AS bearing\n" +
                "FROM public.getLocationTable('%pointiri%','%speediri%','%altitudeiri%','%bearingiri%') as timeseries\n" +
                "WHERE\n" +
                "time > '%date%'::TIMESTAMPTZ AND time < '%date%'::TIMESTAMPTZ + INTERVAL '23 hours 59 minutes 59 seconds 999 milliseconds'\n");
        virtualTable.setEscapeSql(true);
        virtualTable.setName("trajectoryPointVirtualTable");
        virtualTable.addVirtualTableGeometry("geom", "Geometry", "4326"); // geom needs to match the sql query
        virtualTable.addVirtualTableParameter("pointiri", pointIRI, ".*");
        virtualTable.addVirtualTableParameter("speediri", speedIRI, ".*");
        virtualTable.addVirtualTableParameter("altitudeiri", altitudeIRI, ".*");
        virtualTable.addVirtualTableParameter("bearingiri", bearingIRI, ".*");
        virtualTable.addVirtualTableParameter("date", "0001-01-01 00:00:00.000+00", "^\\d{4,}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}.\\d{2,}\\+\\d{2}$");
        geoServerVectorSettings.setVirtualTable(virtualTable);
        geoServerClient.createPostGISDataStore(workspaceName, "trajectoryPoint", dbName, schema);
        geoServerClient.createPostGISLayer(workspaceName, dbName, "trajectoryPoint", geoServerVectorSettings);

        //Create line layer
        UpdatedGSVirtualTableEncoder virtualTableLine = new UpdatedGSVirtualTableEncoder();
        GeoServerVectorSettings geoServerVectorLineSettings = new GeoServerVectorSettings();
        virtualTableLine.setSql(
                "SELECT\n" +
                "    ST_MakeLine(geom) AS geom\n" +
                "    FROM\n" +
                "        public.getLocationTable('%pointiri%','%speediri%','%altitudeiri%','%bearingiri%')\n" +
                "    WHERE\n" +
                "        time > '%date%'::TIMESTAMPTZ AND time < '%date%'::TIMESTAMPTZ + INTERVAL '23 hours 59 minutes 59 seconds 999 milliseconds'\n");
        virtualTableLine.setEscapeSql(true);
        virtualTableLine.setName("trajectoryLineVirtualTable");
        virtualTableLine.addVirtualTableGeometry("geom", "Geometry", "4326"); // geom needs to match the sql query
        virtualTableLine.addVirtualTableParameter("pointiri", pointIRI, ".*");
        virtualTableLine.addVirtualTableParameter("speediri", speedIRI, ".*");
        virtualTableLine.addVirtualTableParameter("altitudeiri", altitudeIRI, ".*");
        virtualTableLine.addVirtualTableParameter("bearingiri", bearingIRI, ".*");
        virtualTableLine.addVirtualTableParameter("date", "0001-01-01 00:00:00.000+00", "^\\d{4,}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}.\\d{2,}\\+\\d{2}$");
        geoServerVectorLineSettings.setVirtualTable(virtualTableLine);
        geoServerClient.createPostGISDataStore(workspaceName, "trajectoryLine", dbName, schema);
        geoServerClient.createPostGISLayer(workspaceName, dbName, "trajectoryLine", geoServerVectorLineSettings);


        //Sample point request - specify pointIRI, speedIRI, altitudeIRI, bearingIRI
        //http://localhost:3838/geoserver/twa/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=twa%3AtrajectoryPoint&outputFormat=application%2Fjson&viewparams=pointiri:https://www.theworldavatar.com/kg/sensorloggerapp/point_3bfa75a3-5b2c-45d3-b05a-63879a2e7b94;speediri:https://www.theworldavatar.com/kg/sensorloggerapp/measure_speed_b9f3ee65-7269-4aef-a738-fd7bf9485143;altitudeiri=https://www.theworldavatar.com/kg/sensorloggerapp/measure_altitude_86b4c979-4d94-42ff-8844-a64ee1cb1229;bearingiri:https://www.theworldavatar.com/kg/sensorloggerapp/measure_bearing_4733199d-46de-429e-ac1d-c94fca537e7a;

        //Sample line request - specify pointIRI, speedIRI, altitudeIRI, bearingIRI
        //http://localhost:3838/geoserver/twa/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=twa%3AtrajectoryLine&outputFormat=application%2Fjson&viewparams=pointiri:https://www.theworldavatar.com/kg/sensorloggerapp/point_3bfa75a3-5b2c-45d3-b05a-63879a2e7b94;speediri:https://www.theworldavatar.com/kg/sensorloggerapp/measure_speed_b9f3ee65-7269-4aef-a738-fd7bf9485143;altitudeiri=https://www.theworldavatar.com/kg/sensorloggerapp/measure_altitude_86b4c979-4d94-42ff-8844-a64ee1cb1229;bearingiri:https://www.theworldavatar.com/kg/sensorloggerapp/measure_bearing_4733199d-46de-429e-ac1d-c94fca537e7a;
    }

    /**
     * Create connection to remoteStoreClient and execute SQL statement
     *
     * @param connection PostgreSQL connection object
     * @param sql        SQl statement to execute
     */
    private void executeSql(Connection connection, String sql) throws SQLException {
        try (Statement statement = connection.createStatement()) {
            statement.execute(sql);
        }
    }

    /**
     * Check if the JSONObject in the processRequestParameters inputs are correct or missing.
     *
     * @param requestParams
     * @return
     * @throws BadRequestException
     */
    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (!requestParams.has(USER_ID)) {
            LOGGER.error("userID is missing.");
            return false;
        }
        return true;
    }
}