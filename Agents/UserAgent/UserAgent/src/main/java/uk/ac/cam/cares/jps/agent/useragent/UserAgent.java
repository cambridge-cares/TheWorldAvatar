package uk.ac.cam.cares.jps.agent.useragent;

import com.cmclinnovations.stack.clients.ontop.OntopClient;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import java.nio.file.Path;


@WebServlet(urlPatterns = {"/registerPhone", "/getPhoneIds", "/registerOuraRing", "/status"})
public class UserAgent extends JPSAgent {
    private TimelineRDBStoreHelper timelineRdbStoreHelper;
    private KGQueryClient kgQueryClient;
    private final Logger LOGGER = LogManager.getLogger(UserAgent.class);

    private static final Path obdaFile = Path.of("/inputs/user.obda");

    public void init() {
        LOGGER.debug("This is a debug message.");
        LOGGER.info("This is an info message.");
        LOGGER.warn("This is a warn message.");
        LOGGER.error("This is an error message.");
        LOGGER.fatal("This is a fatal message.");

        EndpointConfig endpointConfig = new EndpointConfig();

        kgQueryClient = new KGQueryClient(new RemoteStoreClient(endpointConfig.getOntopurl()));

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
        else if (request.getRequestURI().contains("registerPhone")) {
            validateInputRegisterPhone(requestParams);

            return registerPhone(requestParams);

        } else if (request.getRequestURI().contains("getPhoneIds")) {
            validateInputGetPhoneIds(requestParams);
            JSONObject result = getPhoneIds(requestParams);
            return result;
        } else if (request.getRequestURI().contains("registerOuraRing")) {
            validateInputRegisterOuraRing(requestParams);
            return registerOuraRing(requestParams);
        }

        return processRequestParameters(requestParams);
    }


    private JSONObject getPhoneIds(JSONObject requestParams) {
        JSONArray phoneIds = kgQueryClient.getPhoneIds(requestParams.getString("userId"));
        JSONObject result = new JSONObject();
        result.put("PhoneIds", phoneIds);
        result.put("Comment", "Phone ids are retrieved.");
        return result;
    }


    private JSONObject registerPhone(JSONObject requestParams) {
        String userId = requestParams.getString("userId");
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

        timelineRdbStoreHelper.registerPhone(requestParams.getString("phoneId"), requestParams.getString("userId"));
        JSONObject result = new JSONObject();
        result.put("Comment", "Phone is registered successfully.");
        LOGGER.info(phoneId + " is registered to " + userId);
        return result;
    }

    private JSONObject registerOuraRing(JSONObject requestParams) {
        String userId = requestParams.getString("userId");
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
        if (!requestParams.has("userId")) {
            throw new BadRequestException("No user id provided");
        }
        if (!requestParams.has("phoneId")) {
            throw new BadRequestException("No phone id provided");
        }
        
    }

    private void validateInputRegisterOuraRing(JSONObject requestParams) throws BadRequestException {
        LOGGER.debug("Request received: " + requestParams.toString());
        if (!requestParams.has("userId")) {
            throw new BadRequestException("No user id provided");
        }
        if (!requestParams.has("ouraRingApiKey")) {
            throw new BadRequestException("No oura ring api key provided");
        }
    }

    private void validateInputGetPhoneIds(JSONObject requestParams) throws BadRequestException {
        LOGGER.debug("Request received: " + requestParams.toString());
        if (!requestParams.has("userId")) {
            throw new BadRequestException("No user id provided");
        }
    }
}