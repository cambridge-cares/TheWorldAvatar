package uk.ac.cam.cares.jps.agent.useragent;

import com.cmclinnovations.stack.clients.ontop.OntopClient;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.hc.client5.http.classic.methods.HttpGet;
import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.client5.http.impl.classic.CloseableHttpResponse;
import org.apache.hc.client5.http.impl.classic.HttpClients;
import org.apache.hc.core5.http.ParseException;
import org.apache.hc.core5.http.io.entity.EntityUtils;
import org.apache.http.HttpResponse;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import org.keycloak.authorization.client.AuthzClient;
import org.keycloak.authorization.client.representation.TokenIntrospectionResponse;
import org.keycloak.representations.idm.authorization.AuthorizationRequest;
import org.keycloak.representations.idm.authorization.AuthorizationResponse;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import java.io.IOException;
import java.nio.file.Path;
import java.util.stream.Collectors;


@WebServlet(urlPatterns = {"/registerPhone", "/getPhoneIds", "/registerOuraRing", "/status"})
public class UserAgent extends JPSAgent {
    private TimelineRDBStoreHelper timelineRdbStoreHelper;
    private KGQueryClient kgQueryClient;
    private final Logger LOGGER = LogManager.getLogger(UserAgent.class);

    private static final Path obdaFile = Path.of("/inputs/user.obda");
    AuthzClient authzClient;

    public void init() {
        LOGGER.debug("This is a debug message.");
        LOGGER.info("This is an info message.");
        LOGGER.warn("This is a warn message.");
        LOGGER.error("This is an error message.");
        LOGGER.fatal("This is a fatal message.");

        EndpointConfig endpointConfig = new EndpointConfig();

        kgQueryClient = new KGQueryClient(new RemoteStoreClient(endpointConfig.getOntopurl()));
        authzClient = AuthzClient.create();

        LOGGER.info("initializing rdb");
        RemoteRDBStoreClient postgresRdbClient = new RemoteRDBStoreClient(endpointConfig.getDburl(), endpointConfig.getDbuser(), endpointConfig.getDbpassword());
        timelineRdbStoreHelper = new TimelineRDBStoreHelper(postgresRdbClient);

        try {
            OntopClient ontopClient = OntopClient.getInstance();
            LOGGER.info("updating obda");
            ontopClient.updateOBDA(obdaFile);

            // wait for ontop to be restarted
            Thread.sleep(1500);
        } catch (Exception e) {
            System.out.println(e.getMessage());
            System.out.println("Could not retrieve user.obda file.");
        }
    }


    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        if (request.getRequestURI().contains("status")) {
            JSONObject jo = new JSONObject();
            jo.put("Message", "User agent is up and running");
            return jo;
        }

        TokenIntrospectionResponse tokenIntrospectionResponse = getTokenIntrospectResponse(request);
        if (request.getRequestURI().contains("registerPhone")) {
            if (tokenIntrospectionResponse.getPermissions().stream().map(p -> p.getResourceName()).collect(Collectors.toList()).contains("registerPhone")) {
                return registerPhone(requestParams, getUserId(request));
            }
        } else if (request.getRequestURI().contains("getPhoneIds")) {
            if (tokenIntrospectionResponse.getPermissions().stream().map(p -> p.getResourceName()).collect(Collectors.toList()).contains("getPhoneIds")) {
                return getPhoneIds(requestParams, getUserId(request));
            }
        } else if (request.getRequestURI().contains("registerOuraRing")) {
            if (tokenIntrospectionResponse.getPermissions().stream().map(p -> p.getResourceName()).collect(Collectors.toList()).contains("registerOuraRing")) {
                return registerOuraRing(requestParams, getUserId(request));
            }
        }
        throw new JPSRuntimeException("Permission denied");
    }

    private TokenIntrospectionResponse getTokenIntrospectResponse(HttpServletRequest httpRequest) {
        AuthorizationRequest request = new AuthorizationRequest();
        AuthorizationResponse response = authzClient.authorization(httpRequest.getHeader("Authorization").replace("Bearer ", "")).authorize(request);

        TokenIntrospectionResponse tokenIntrospectionResponse = authzClient.protection().introspectRequestingPartyToken(response.getToken());
        return tokenIntrospectionResponse;
    }

    private String getUserId(HttpServletRequest httpRequest) {
        // NOTICE: the function won't retrieve the latest token if the provided one has expired
        try (CloseableHttpClient httpClient = HttpClients.createDefault()) {
            String userinfoEndpoint = authzClient.getServerConfiguration().getIssuer() + "/protocol/openid-connect/userinfo";
            HttpGet request = new HttpGet(userinfoEndpoint);
            request.addHeader("Authorization", httpRequest.getHeader("Authorization"));

            CloseableHttpResponse response = httpClient.execute(request);
            int statusCode = response.getCode();

            if (statusCode == 200) {
                String jsonResponse = EntityUtils.toString(response.getEntity());
                ObjectMapper objectMapper = new ObjectMapper();
                JsonNode userInfo = objectMapper.readTree(jsonResponse);
                return userInfo.findValue("sub").asText();
            }
        } catch (IOException | ParseException e) {
            throw new RuntimeException(e);
        }
        throw new RuntimeException("Not able to get the user id");
    }


    private JSONObject getPhoneIds(JSONObject requestParams, String userId) {
        JSONArray phoneIds = kgQueryClient.getPhoneIds(userId);
        JSONObject result = new JSONObject();
        result.put("PhoneIds", phoneIds);
        result.put("Comment", "Phone ids are retrieved.");
        return result;
    }


    private JSONObject registerPhone(JSONObject requestParams, String userId) {
        validateInputRegisterPhone(requestParams);
        String phoneId = requestParams.getString("phoneId");

        JSONArray jsonArray = timelineRdbStoreHelper.getExistingPhoneIdRecord(phoneId);
        if (!jsonArray.isEmpty()) {
            if (jsonArray.getJSONObject(0).getString("user_id").equals(userId)) {
                LOGGER.info("Phone has already been registered");
                JSONObject result = new JSONObject();
                result.put("Comment", "Phone has already been registered");
                return result;
            } else {
                LOGGER.error("Phone is already registered with another userId: " + jsonArray.getJSONObject(0).getString("user_id"));
                throw new JPSRuntimeException("Phone is already registered with another userId");
            }
        }

        timelineRdbStoreHelper.registerPhone(phoneId, userId);
        JSONObject result = new JSONObject();
        result.put("Comment", "Phone is registered successfully.");
        LOGGER.info(phoneId + " is registered to " + userId);
        return result;
    }

    private JSONObject registerOuraRing(JSONObject requestParams, String userId) {
        validateInputRegisterOuraRing(requestParams);
        String ouraRingApi = requestParams.getString("ouraRingApiKey");
        JSONArray jsonArray = timelineRdbStoreHelper.getExistingOuraRingRecord(ouraRingApi);
        if (!jsonArray.isEmpty()) {
            if (jsonArray.getJSONObject(0).getString("user_id").equals(userId)) {
                LOGGER.info("Oura ring has already been registered");
                JSONObject result = new JSONObject();
                result.put("Comment", "Oura ring has already been registered");
                return result;
            } else {
                LOGGER.error("Oura ring is already registered with another userId: " + jsonArray.getJSONObject(0).getString("user_id"));
                throw new JPSRuntimeException("Oura ring is already registered with another userId");
            }
        }

        timelineRdbStoreHelper.registerOuraRing(ouraRingApi, userId);
        JSONObject result = new JSONObject();
        result.put("Comment", "Oura ring is registered successfully.");
        LOGGER.info(ouraRingApi + " is registered to " + userId);
        return result;
    }

    private void validateInputRegisterPhone(JSONObject requestParams) throws BadRequestException {
        LOGGER.debug("Request received: " + requestParams.toString());
        if (!requestParams.has("phoneId")) {
            throw new BadRequestException("No phone id provided");
        }

    }

    private void validateInputRegisterOuraRing(JSONObject requestParams) throws BadRequestException {
        LOGGER.debug("Request received: " + requestParams.toString());
        if (!requestParams.has("ouraRingApiKey")) {
            throw new BadRequestException("No oura ring api key provided");
        }
    }
}