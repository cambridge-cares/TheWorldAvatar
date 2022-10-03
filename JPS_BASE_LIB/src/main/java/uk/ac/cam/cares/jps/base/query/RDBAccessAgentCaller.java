package uk.ac.cam.cares.jps.base.query;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.http.Http;

import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLDecoder;

public class RDBAccessAgentCaller {
    /**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(RDBAccessAgentCaller.class);

    public static final String ACCESSAGENT_HOST_NAME = "ACCESSAGENT_HOST";
    public static String accessAgentHost;

    //Set the default value for the access agent host
    static{
        accessAgentHost = System.getenv(ACCESSAGENT_HOST_NAME);
        if(accessAgentHost == null) {
            // Try get the access agent host from the environment variables
            // if not found, then get from the jps.properties file
            LOGGER.info("ACCESSAGENT_HOST not found in environment variables..."
                    + " Using jps.properties.");
            accessAgentHost = KeyValueMap.getInstance().get(IKeys.URL_ACCESSAGENT_HOST);
        }
        LOGGER.info("Default ACCESSAGENT_HOST set to "+accessAgentHost);
    }

    /**
     * Default constructor
     */
    public RDBAccessAgentCaller() {}

    public static String getRDBUrl(String targetUrl) {

        LOGGER.info("get url for targetUrl=" + targetUrl);

        Object[] a = createRDBAccessRequestUrl(targetUrl);

        String requestUrl = (String) a[0];
        JSONObject joparams = (JSONObject) a[1];

        return (String) new JSONObject(Http.execute(Http.get(requestUrl, null,joparams))).get("result");
    }

    public static Object[] createRDBAccessRequestUrl(String targetUrl) {

        String requestUrl;

        JSONObject joparams ;

        requestUrl = getBaseWorldUrlforRDBAccess(targetUrl);

        joparams = new JSONObject();
        joparams.put(JPSConstants.TARGETIRI, AccessAgentCaller.cutHashFragment(targetUrl));

        Object[] a = new Object[] {requestUrl, joparams};
        return a;
    }
    public static String getBaseWorldUrlforRDBAccess(String url) {

        URI requestUrl = null;
        try {
            URI uri = new URI(URLDecoder.decode(url,"UTF-8"));
            String scheme = uri.getScheme();
            String authority = uri.getAuthority();

            //If no scheme is provided then default to HTTP
            if(scheme == null) {
                scheme = "http";
            }

            //If no authority is given then get the default host
            if(authority == null) {
                authority = accessAgentHost;
            }

            requestUrl = new URI(scheme,authority,JPSConstants.RDB_ACCESS_AGENT_PATH,null,null);

        } catch (UnsupportedEncodingException e) {
            throw new JPSRuntimeException(e);
        } catch (URISyntaxException e) {
            throw new JPSRuntimeException(e);
        }

        return requestUrl.toString();
    }

}
