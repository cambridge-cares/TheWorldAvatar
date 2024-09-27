package uk.ac.cam.cares.jps.agent.trajectoryqueryagent;

import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.cmclinnovations.stack.clients.geoserver.UpdatedGSVirtualTableEncoder;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import org.json.JSONArray;
import org.json.JSONObject;
import org.springframework.core.io.ClassPathResource;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import org.apache.commons.io.IOUtils;

@WebServlet(urlPatterns = { TrajectoryQueryAgent.CREATE_LAYER_ROUTE, "/getDatesWithData", "/feature-info-agent/get" })
public class TrajectoryQueryAgent extends JPSAgent {
    private RemoteRDBStoreClient remoteRDBStoreClient;
    private static final String USER_ID = "userID";
    private static final String TIMEZONE = "timezone";
    private static final Logger LOGGER = LogManager.getLogger(TrajectoryQueryAgent.class);
    public static final String CREATE_LAYER_ROUTE = "/createLayer";
    public static final String GET_DATES_ROUTE = "/getDatesWithData";

    @Override
    public void init() {
        EndpointConfig endpointConfig = new EndpointConfig();
        remoteRDBStoreClient = new RemoteRDBStoreClient(endpointConfig.getDburl(), endpointConfig.getDbuser(),
                endpointConfig.getDbpassword());
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        String url = request.getRequestURI();

        if (request.getServletPath().contentEquals(GET_DATES_ROUTE)) {
            if (!validateInput(requestParams, url)) {
                throw new JPSRuntimeException("Unable to validate request sent to the agent.");
            }
            return getDatesWithData(requestParams);
        } else if (request.getServletPath().contentEquals(CREATE_LAYER_ROUTE)) {
            return createLayer();
        }

        JSONObject response = new JSONObject();
        response.put("message", "Path invalid, no operation performed");
        return response;
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
    public JSONObject createLayer() {
        createGeoserver();

        JSONObject response = new JSONObject();
        response.put("message", "Layer created");

        return response;
    }

    public JSONObject getDatesWithData(JSONObject requestParams) {
        String sqlTemplate = null;
        try (InputStream is = new ClassPathResource("get_dates_with_data.sql").getInputStream()) {
            sqlTemplate = IOUtils.toString(is, StandardCharsets.UTF_8);
        } catch (IOException e) {
            LOGGER.error("Failed to read get_dates_with_data.sql");
            LOGGER.error(e.getMessage());
        }

        JSONObject response = new JSONObject();
        if (sqlTemplate != null) {
            String sql = String.format(sqlTemplate, requestParams.getString(TIMEZONE),
                    requestParams.getString(USER_ID));
            JSONArray result = remoteRDBStoreClient.executeQuery(sql);
            response.put("message", "Succeed");
            response.put("result", result);
        } else {
            response.put("message", "Faile");
        }

        return response;
    }

    /**
     * initialise custom functions
     */
    private void createPostgresFunctions() {
        String sql = null;
        try (InputStream is = new ClassPathResource("functions.sql").getInputStream()) {
            sql = IOUtils.toString(is, StandardCharsets.UTF_8);
        } catch (IOException e) {
            LOGGER.error("Failed to read functions.sql");
            LOGGER.error(e.getMessage());
        }

        if (sql != null) {
            try (Connection connection = remoteRDBStoreClient.getConnection()) {
                executeSql(connection, sql);
            } catch (SQLException e) {
                LOGGER.error("Error executing functions.sql");
                throw new JPSRuntimeException(e);
            }
        }
    }

    private void createGeoserver() {
        createPostgresFunctions();

        String lineLayerDeviceId = null;
        try (InputStream is = new ClassPathResource("line_layer_device_id.sql").getInputStream()) {
            lineLayerDeviceId = IOUtils.toString(is, StandardCharsets.UTF_8);
        } catch (IOException e) {
            LOGGER.error("Failed to read line_layer_device_id.sql");
            LOGGER.error(e.getMessage());
        }

        String workspaceName = "twa";
        String schema = "public";
        String dbName = "postgres";

        GeoServerClient geoServerClient = GeoServerClient.getInstance();

        if (lineLayerDeviceId != null) {
            geoServerClient.createWorkspace(workspaceName);
            UpdatedGSVirtualTableEncoder virtualTable = new UpdatedGSVirtualTableEncoder();
            GeoServerVectorSettings geoServerVectorSettings = new GeoServerVectorSettings();
            virtualTable.setSql(lineLayerDeviceId);
            virtualTable.setEscapeSql(true);
            virtualTable.setName("line_layer_device_id_table");
            virtualTable.addVirtualTableParameter("device_id", "", ".*");
            virtualTable.addVirtualTableGeometry("geom", "Geometry", "4326"); // geom needs to match the sql query
            geoServerVectorSettings.setVirtualTable(virtualTable);
            geoServerClient.createPostGISDataStore(workspaceName, "trajectory", dbName, schema);
            geoServerClient.createPostGISLayer(workspaceName, dbName, "trajectoryDeviceId", geoServerVectorSettings);
        }

        String lineLayerUserId = null;
        try (InputStream is = new ClassPathResource("line_layer_user_id.sql").getInputStream()) {
            lineLayerUserId = IOUtils.toString(is, StandardCharsets.UTF_8);
        } catch (IOException e) {
            LOGGER.error("Failed to read line_layer_user_id.sql");
            LOGGER.error(e.getMessage());
        }

        if (lineLayerUserId != null) {
            geoServerClient.createWorkspace(workspaceName);
            UpdatedGSVirtualTableEncoder virtualTable = new UpdatedGSVirtualTableEncoder();
            GeoServerVectorSettings geoServerVectorSettings = new GeoServerVectorSettings();
            virtualTable.setSql(lineLayerUserId);
            virtualTable.setEscapeSql(true);
            virtualTable.setName("line_layer_device_id_table");
            virtualTable.addVirtualTableGeometry("geom", "Geometry", "4326");
            virtualTable.addVirtualTableParameter("user_id", "", ".*");
            geoServerVectorSettings.setVirtualTable(virtualTable);
            geoServerClient.createPostGISDataStore(workspaceName, "trajectory", dbName, schema);
            geoServerClient.createPostGISLayer(workspaceName, dbName, "trajectoryUserId", geoServerVectorSettings);
        }

        String bufferedLine = null;
        try (InputStream is = new ClassPathResource("buffered_line_layer.sql").getInputStream()) {
            bufferedLine = IOUtils.toString(is, StandardCharsets.UTF_8);
        } catch (IOException e) {
            LOGGER.error("Failed to read buffered_line_layer.sql");
            LOGGER.error(e.getMessage());
        }

        if (bufferedLine != null) {
            geoServerClient.createWorkspace(workspaceName);
            UpdatedGSVirtualTableEncoder virtualTable = new UpdatedGSVirtualTableEncoder();
            GeoServerVectorSettings geoServerVectorSettings = new GeoServerVectorSettings();
            virtualTable.setSql(bufferedLine);
            virtualTable.setEscapeSql(true);
            virtualTable.setName("buffered_line_table");
            geoServerVectorSettings.setVirtualTable(virtualTable);
            geoServerClient.createPostGISDataStore(workspaceName, "trajectory", dbName, schema);
            geoServerClient.createPostGISLayer(workspaceName, dbName, "bufferedLine", geoServerVectorSettings);
        }
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
     * Check if the JSONObject in the processRequestParameters inputs are correct or
     * missing based on the input path.
     * All path retrieving trajectory related data will need user id
     * 
     * @param requestParams
     * @return
     * @throws BadRequestException
     */
    private boolean validateInput(JSONObject requestParams, String url) {
        if (!requestParams.has(USER_ID)) {
            LOGGER.error("userID is missing.");
            return false;
        }
        if (url.contains("getDatesWithData")) {
            if (!requestParams.has(TIMEZONE)) {
                LOGGER.error("timezone is missing.");
                return false;
            }
        }
        return true;
    }
}