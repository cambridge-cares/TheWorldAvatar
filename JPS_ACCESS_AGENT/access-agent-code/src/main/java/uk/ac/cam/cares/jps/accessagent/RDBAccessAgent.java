package uk.ac.cam.cares.jps.accessagent;

import org.apache.http.client.methods.HttpGet;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RDBStoreRouter;
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.base.util.MiscUtil;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

@WebServlet(urlPatterns = {RDBAccessAgent.ACCESS_RDB_URL})
public class RDBAccessAgent extends JPSAgent {
    public static final String ACCESS_RDB_URL = "/rdbaccess";

    /**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(AccessAgent.class);

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        JSONObject result = processRequestParameters(requestParams,null);
        return result;
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {

        if (!validateInput(requestParams)) {
            throw new JSONException("RDBAccessAgent: Input parameters not valid.\n");
        }

        String method = MiscUtil.optNullKey(requestParams, JPSConstants.METHOD);

        JSONObject JSONResult = new JSONObject();

        LOGGER.info("Initialising StoreAccessHandler to perform "+method+" request.");

        switch (method) {
            case HttpGet.METHOD_NAME:
                JSONResult = new JSONObject().put("result", performGet(requestParams));
                break;
        }
        return JSONResult;
    }

    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {

        if (requestParams.isEmpty()) {
            throw new BadRequestException();
        }
        try {

            //GET
            String method = MiscUtil.optNullKey(requestParams,JPSConstants.METHOD);
            if (!method.equals(HttpGet.METHOD_NAME)) {
                LOGGER.error("Invalid input parameters: Not HTTP GET!");
                return false;
            }

            //targetResourceRequired
            String targetiri = MiscUtil.optNullKey(requestParams,JPSConstants.TARGETIRI);
            if (targetiri == null) {
                LOGGER.error("Invalid input parameters: targetResourceID not provided!");
                return false;
            }

            boolean q = InputValidator.checkIfURLpattern(requestParams.getString(JPSConstants.REQUESTURL));
            if(!q) {return false;};

            return true;

        }catch (JSONException ex) {
            return false;
        }
    }

    public String performGet(JSONObject requestParams) {

        String targetIRI = requestParams.getString(JPSConstants.TARGETIRI);

        try {
            logInputParams(requestParams, false);
            String rdbUrl = RDBStoreRouter.getRDBUrl(targetIRI);
            return rdbUrl;

        } catch (RuntimeException e) {
            logInputParams(requestParams, true);
            throw new JPSRuntimeException(e);
        }
    }

    protected void logInputParams(JSONObject requestParams, boolean hasErrorOccured) {

        String method = MiscUtil.optNullKey(requestParams, JPSConstants.METHOD);
        String path = MiscUtil.optNullKey(requestParams, JPSConstants.PATH);
        String requestUrl = MiscUtil.optNullKey(requestParams, JPSConstants.REQUESTURL);
        String contentType = MiscUtil.optNullKey(requestParams, JPSConstants.CONTENTTYPE);
        String targetIRI = requestParams.getString(JPSConstants.TARGETIRI);
        String graphIRI = MiscUtil.optNullKey(requestParams, JPSConstants.TARGETGRAPH);

        StringBuffer b = new StringBuffer(method);
        b.append(" with requestedUrl=").append(requestUrl);
        b.append(", path=").append(path);
        b.append(", contentType=").append(contentType);
        b.append(", targetiri=").append(targetIRI);
        b.append(", targetgraph=").append(graphIRI);
        if (hasErrorOccured) {
            LOGGER.error(b.toString());
        } else {
            LOGGER.info(b.toString());
        }
    }
}
