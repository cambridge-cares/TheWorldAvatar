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
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Iterator;

import org.apache.commons.io.IOUtils;

import com.auth0.jwk.InvalidPublicKeyException;
import com.auth0.jwk.Jwk;
import com.auth0.jwk.JwkException;
import com.auth0.jwk.JwkProvider;
import com.auth0.jwt.JWT;
import com.auth0.jwt.exceptions.JWTVerificationException;
import com.auth0.jwt.algorithms.Algorithm;
import com.auth0.jwt.interfaces.DecodedJWT;
import com.auth0.jwt.interfaces.JWTVerifier;
import java.security.interfaces.RSAPublicKey;

@WebServlet(urlPatterns = { TrajectoryQueryAgent.CREATE_LAYER_ROUTE, "/getDatesWithData" })
public class TrajectoryQueryAgent extends JPSAgent {
    private RemoteRDBStoreClient remoteRDBStoreClient;
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
        if (request.getServletPath().contentEquals(GET_DATES_ROUTE)) {
            if (!requestParams.has(TIMEZONE)) {
                String errmsg = "timezone parameter is missing";
                LOGGER.error(errmsg);
                throw new JPSRuntimeException(errmsg);
            }

            String timezone = requestParams.getString(TIMEZONE);
            String userId = getUserId(request);

            return getDatesWithData(timezone, userId);
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

    public JSONObject getDatesWithData(String timezone, String userId) {
        String sqlTemplate = null;
        try (InputStream is = new ClassPathResource("get_dates_with_data.sql").getInputStream()) {
            sqlTemplate = IOUtils.toString(is, StandardCharsets.UTF_8);
        } catch (IOException e) {
            LOGGER.error("Failed to read get_dates_with_data.sql");
            LOGGER.error(e.getMessage());
        }

        JSONObject response = new JSONObject();
        if (sqlTemplate != null) {
            String sql = String.format(sqlTemplate, timezone, userId);
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
            virtualTable.addVirtualTableParameter("device_id", "null", ".*");
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
            virtualTable.addVirtualTableParameter("user_id", "null", ".*");
            virtualTable.addVirtualTableParameter("upperbound", "0", "^(0|[1-9][0-9]*)$");
            virtualTable.addVirtualTableParameter("lowerbound", "0", "^(0|[1-9][0-9]*)$");
            geoServerVectorSettings.setVirtualTable(virtualTable);
            geoServerClient.createPostGISDataStore(workspaceName, "trajectory", dbName, schema);
            geoServerClient.createPostGISLayer(workspaceName, dbName, "trajectoryUserId", geoServerVectorSettings);
        }

        String bufferedLineDeviceId = null;
        try (InputStream is = new ClassPathResource("buffered_line_layer_device_id.sql").getInputStream()) {
            bufferedLineDeviceId = IOUtils.toString(is, StandardCharsets.UTF_8);
        } catch (IOException e) {
            LOGGER.error("Failed to read buffered_line_layer_device_id.sql");
            LOGGER.error(e.getMessage());
        }

        if (bufferedLineDeviceId != null) {
            geoServerClient.createWorkspace(workspaceName);
            UpdatedGSVirtualTableEncoder virtualTable = new UpdatedGSVirtualTableEncoder();
            GeoServerVectorSettings geoServerVectorSettings = new GeoServerVectorSettings();
            virtualTable.setSql(bufferedLineDeviceId);
            virtualTable.setEscapeSql(true);
            virtualTable.setName("buffered_line_device_id_table");
            virtualTable.addVirtualTableParameter("device_id", "null", ".*");
            virtualTable.addVirtualTableParameter("upperbound", "0", "^(0|[1-9][0-9]*)$");
            virtualTable.addVirtualTableParameter("lowerbound", "0", "^(0|[1-9][0-9]*)$");
            geoServerVectorSettings.setVirtualTable(virtualTable);
            geoServerClient.createPostGISDataStore(workspaceName, "trajectory", dbName, schema);
            geoServerClient.createPostGISLayer(workspaceName, dbName, "bufferedLineDeviceId", geoServerVectorSettings);
        }

        String lineLayerUserIdLineSegments = null;
        try (InputStream is = new ClassPathResource("line_layer_user_id_line_segments.sql").getInputStream()) {
            lineLayerUserIdLineSegments = IOUtils.toString(is, StandardCharsets.UTF_8);
        } catch (IOException e) {
            LOGGER.error("Failed to read line_layer_user_id_line_segments.sql");
            LOGGER.error(e.getMessage());
        }

        if (lineLayerUserIdLineSegments != null) {
            geoServerClient.createWorkspace(workspaceName);
            UpdatedGSVirtualTableEncoder virtualTable = new UpdatedGSVirtualTableEncoder();
            GeoServerVectorSettings geoServerVectorSettings = new GeoServerVectorSettings();
            virtualTable.setSql(lineLayerUserIdLineSegments);
            virtualTable.setEscapeSql(true);
            virtualTable.setName("line_layer_user_id_segments_table");
            virtualTable.addVirtualTableGeometry("geom", "Geometry", "4326");
            virtualTable.addVirtualTableParameter("user_id", "null", ".*");
            geoServerVectorSettings.setVirtualTable(virtualTable);
            geoServerClient.createPostGISDataStore(workspaceName, "trajectory", dbName, schema);
            geoServerClient.createPostGISLayer(workspaceName, dbName, "trajectoryUserIdLineSegments",
                    geoServerVectorSettings);
        }


        String lineLayerUserIdByActivity = null;
        try (InputStream is = new ClassPathResource("line_layer_user_id_by_activity.sql").getInputStream()) {
            lineLayerUserIdByActivity = IOUtils.toString(is, StandardCharsets.UTF_8);
        } catch (IOException e) {
            LOGGER.error("failed to read line_layer_user_id_by_activity.sql");
            LOGGER.error(e.getMessage());
        }

        if (lineLayerUserIdByActivity != null) {
            geoServerClient.createWorkspace(workspaceName);
            UpdatedGSVirtualTableEncoder virtualTable = new UpdatedGSVirtualTableEncoder();
            GeoServerVectorSettings geoServerVectorSettings = new GeoServerVectorSettings();
            virtualTable.setSql(lineLayerUserIdByActivity);
            virtualTable.setEscapeSql(true);
            virtualTable.setName("line_layer_user_id_by_activity_table");
            virtualTable.addVirtualTableGeometry("geom", "Geometry", "4326");
            virtualTable.addVirtualTableParameter("user_id", "null", ".*");
            virtualTable.addVirtualTableParameter("upperbound", "0", "^(0|[1-9][0-9]*)$");
            virtualTable.addVirtualTableParameter("lowerbound", "0", "^(0|[1-9][0-9]*)$");
            geoServerVectorSettings.setVirtualTable(virtualTable);
            geoServerClient.createPostGISDataStore(workspaceName, "trajectory", dbName, schema);
            geoServerClient.createPostGISLayer(workspaceName, dbName, "trajectoryUserIdByActivity", geoServerVectorSettings);
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

    String getUserId(HttpServletRequest request) {
        String token = null;
        Iterator<String> headerIterator = request.getHeaders("Authorization").asIterator();
        while (headerIterator.hasNext() && token == null) {
            String header = headerIterator.next();
            if (header.startsWith("Bearer ")) {
                token = header.substring(7);
            }
        }

        DecodedJWT verifiedJwt = validateSignature(token);

        return verifiedJwt.getSubject();
    }

    private DecodedJWT validateSignature(String token) {
        // check signature using public key from KeyCloak server
        JwkProvider provider = JwkProviderSingleton.getInstance();

        DecodedJWT unverifiedDecodedJWT = JWT.decode(token);

        String keyId = unverifiedDecodedJWT.getKeyId();
        Jwk jwk;
        try {
            jwk = provider.get(keyId);
        } catch (JwkException e) {
            String errmsg = "Cannot find key ID from token";
            LOGGER.error(e.getMessage());
            LOGGER.error(errmsg);
            throw new RuntimeException(errmsg, e);
        }

        RSAPublicKey publicKey;
        try {
            publicKey = (RSAPublicKey) jwk.getPublicKey();
        } catch (InvalidPublicKeyException e) {
            String errmsg = "Cannot get public key from provider";
            LOGGER.error(e.getMessage());
            LOGGER.error(errmsg);
            throw new RuntimeException(errmsg, e);
        }

        Algorithm algorithm = Algorithm.RSA256(publicKey);

        // Create the JWT verifier
        JWTVerifier verifier = JWT.require(algorithm).build();

        DecodedJWT verifiedDecodedJWT;

        try {
            verifiedDecodedJWT = verifier.verify(unverifiedDecodedJWT);
        } catch (JWTVerificationException e) {
            String errmsg = "Failed to verify token";
            LOGGER.error(e.getMessage());
            LOGGER.error(errmsg);
            throw new RuntimeException(errmsg, e);
        }

        return verifiedDecodedJWT;
    }
}