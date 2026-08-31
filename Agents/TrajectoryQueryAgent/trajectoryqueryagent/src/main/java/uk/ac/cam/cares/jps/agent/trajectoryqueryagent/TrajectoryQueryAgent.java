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

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Iterator;

import org.apache.commons.io.IOUtils;

import com.auth0.jwk.Jwk;
import com.auth0.jwk.JwkException;
import com.auth0.jwk.JwkProvider;
import com.auth0.jwt.JWT;
import com.auth0.jwt.exceptions.JWTVerificationException;
import com.auth0.jwt.algorithms.Algorithm;
import com.auth0.jwt.interfaces.DecodedJWT;
import com.auth0.jwt.interfaces.JWTVerifier;
import java.security.interfaces.RSAPublicKey;

@WebServlet(urlPatterns = { TrajectoryQueryAgent.CREATE_LAYER_ROUTE, TrajectoryQueryAgent.GET_DATES_ROUTE })
public class TrajectoryQueryAgent extends HttpServlet {
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
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        response.setCharacterEncoding(StandardCharsets.UTF_8.name());
        response.setContentType("application/json");

        try {
            JSONObject result;
            if (request.getServletPath().contentEquals(GET_DATES_ROUTE)) {
                String timezone = request.getParameter(TIMEZONE);
                if (timezone == null || timezone.isBlank()) {
                    writeError(response, HttpServletResponse.SC_BAD_REQUEST, "timezone parameter is missing");
                    return;
                }

                String userId = getUserId(request);
                result = getDatesWithData(timezone, userId);
            } else if (request.getServletPath().contentEquals(CREATE_LAYER_ROUTE)) {
                getUserId(request); // authenticate before making any changes
                result = createLayer();
            } else {
                writeError(response, HttpServletResponse.SC_NOT_FOUND, "Path invalid, no operation performed");
                return;
            }

            writeJson(response, HttpServletResponse.SC_OK, result);
        } catch (AuthenticationException e) {
            LOGGER.warn("Authentication failed: {}", e.getMessage());
            response.setHeader("WWW-Authenticate", "Bearer");
            writeError(response, HttpServletResponse.SC_UNAUTHORIZED, e.getMessage());
        } catch (RuntimeException e) {
            LOGGER.error("Failed to process request", e);
            writeError(response, HttpServletResponse.SC_INTERNAL_SERVER_ERROR, "Internal server error");
        }
    }

    private void writeError(HttpServletResponse response, int status, String message) throws IOException {
        JSONObject body = new JSONObject();
        body.put("message", message);
        writeJson(response, status, body);
    }

    private void writeJson(HttpServletResponse response, int status, JSONObject body) throws IOException {
        response.setStatus(status);
        response.getWriter().write(body.toString());
    }

    /**
     * 1) Receive userID
     * 2) SPARQL query for pointIRI based on userID
     * 3) Create geoserver layer
     * 4) Return pointIRI to application as response
     *
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
            response.put("message", "Failed");
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
            geoServerClient.createPostGISLayer(workspaceName, dbName, schema, "trajectoryDeviceId",
                    geoServerVectorSettings);
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
            geoServerClient.createPostGISLayer(workspaceName, dbName, schema, "trajectoryUserId",
                    geoServerVectorSettings);
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
            geoServerClient.createPostGISLayer(workspaceName, dbName, schema, "bufferedLineDeviceId",
                    geoServerVectorSettings);
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
            geoServerClient.createPostGISLayer(workspaceName, dbName, schema, "trajectoryUserIdLineSegments",
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
            geoServerClient.createPostGISLayer(workspaceName, dbName, schema, "trajectoryUserIdByActivity",
                    geoServerVectorSettings);
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
            String header = headerIterator.next().trim();
            if (header.regionMatches(true, 0, "Bearer ", 0, 7)) {
                token = header.substring(7).trim();
            }
        }

        if (token == null || token.isEmpty()) {
            throw new AuthenticationException("Bearer token is missing");
        }

        DecodedJWT verifiedJwt = validateSignature(token);
        String subject = verifiedJwt.getSubject();
        if (subject == null || subject.isBlank()) {
            throw new AuthenticationException("Bearer token subject is missing");
        }

        return subject;
    }

    private DecodedJWT validateSignature(String token) {
        try {
            // Use the unverified key ID only to select a public key from the trusted
            // Keycloak JWK endpoint. The token is verified before any claim is used.
            JwkProvider provider = JwkProviderSingleton.getInstance();
            DecodedJWT unverifiedDecodedJWT = JWT.decode(token);
            String keyId = unverifiedDecodedJWT.getKeyId();
            if (keyId == null || keyId.isBlank()) {
                throw new AuthenticationException("Bearer token key ID is missing");
            }

            Jwk jwk = provider.get(keyId);
            RSAPublicKey publicKey = (RSAPublicKey) jwk.getPublicKey();
            Algorithm algorithm = Algorithm.RSA256(publicKey);
            JWTVerifier verifier = JWT.require(algorithm).build();

            return verifier.verify(unverifiedDecodedJWT);
        } catch (AuthenticationException e) {
            throw e;
        } catch (JWTVerificationException | JwkException | ClassCastException | IllegalArgumentException e) {
            throw new AuthenticationException("Invalid or expired bearer token", e);
        }
    }

    private static class AuthenticationException extends RuntimeException {
        AuthenticationException(String message) {
            super(message);
        }

        AuthenticationException(String message, Throwable cause) {
            super(message, cause);
        }
    }
}
