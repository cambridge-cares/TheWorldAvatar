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

@WebServlet(urlPatterns = {RDBAccessAgent.ACCESS_RDB_URL, RDBAccessAgent.CLEAR_CACHE_URL})
public class RDBAccessAgent extends JPSAgent {
    public static final String ACCESS_RDB_URL = "/rdbaccess";
    public static final String CLEAR_CACHE_URL = "/rdbclearcache";

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

        String method = MiscUtil.optNullKey(requestParams, JPSConstants.METHOD);

        // Clear cache
        if(request.getServletPath().equals(CLEAR_CACHE_URL)) {
            if (method.equals(HttpGet.METHOD_NAME)) {
                return clearCache();
            } else {
                throw new JPSRuntimeException("RDBAccessAgent: Input parameters not valid.\n");
            }
        }
        else {
            if (!validateInput(requestParams)) {
                throw new JSONException("RDBAccessAgent: Input parameters not valid.\n");
            }

            JSONObject JSONResult = new JSONObject();

            switch (method) {
                case HttpGet.METHOD_NAME:
                    JSONResult = new JSONObject().put("result", performGet(requestParams));
                    break;
            }
            return JSONResult;
        }
    }

    /**
     * Clear RDBStoreRouter cache
     * @return
     */
    public JSONObject clearCache() {
        RDBStoreRouter.getInstance().clearCache();
        JSONObject JSONresult = new JSONObject().put(JPSConstants.RESULT_KEY, "Cache cleared.");
        return JSONresult;
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
            String rdbUrl = getRDBUrl(targetIRI);
            return rdbUrl;

        } catch (RuntimeException e) {
            logInputParams(requestParams, true);
            throw new JPSRuntimeException(e);
        }
    }

    public String getRDBUrl(String targetIRI){
        try {
            String url = RDBStoreRouter.getRDBUrl(targetIRI);
            return url;
        }catch (RuntimeException e){
            LOGGER.error("Failed to obtain RDB url");
            throw new JPSRuntimeException("Failed to obtain RDB url");
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
