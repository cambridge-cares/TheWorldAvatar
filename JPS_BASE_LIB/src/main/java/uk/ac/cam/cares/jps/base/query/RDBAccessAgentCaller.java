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

/**
 *  The RDBAccessAgentCaller class is used to send HTTP requests to the RDBAccessAgent
 *  to obtain the RDB url used to create an instance of {@link uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient} RemoteRDBStoreClient
 *  using the method {@link uk.ac.cam.cares.jps.base.query.RDBAccessAgentCaller#getRDBUrl}  getRDBUrl}
 *  <br>
 *  This method can also be accessed in the {@link uk.ac.cam.cares.jps.base.agent.JPSAgent} class.
 *  <br>
 *  The access agent host:port should be set using the environment variable ACCESSAGENT_HOST.
 *  Otherwise, the default host from the jps.properties file is used. Note that if a url is
 *  supplied as the targetResourceID then the host in the url is used.
 *
 *  @author Mehal Agarwal
 */

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

    /**
     * Sends an Http GET request to the RDBAccessAgent to obtain the RDB Url
     * @param targetUrl the target namespace or IRI <br>
     * 			        Note: If the targetUrl is a URL/IRI (e.g. "http://localhost:8080/test"),
     * 					the request will be sent to the host given in the URL (i.e. localhost:8080).
     * 	                If no host is provided (e.g. targetUrl = "test"), the request is sent
     *  				to the host given by the environment variable "ACCESSAGENT_HOST"
     * 					or that in jps.properties, if the environment variable is not set.
     * @return RDB Url
     */
    public static String getRDBUrl(String targetUrl) {

        LOGGER.info("get url for targetUrl=" + targetUrl);

        Object[] a = createRDBAccessRequestUrl(targetUrl);

        String requestUrl = (String) a[0];
        JSONObject joparams = (JSONObject) a[1];

        return (String) new JSONObject(Http.execute(Http.get(requestUrl, null,joparams))).get("result");
    }

    /**
     * Creates the request url and request parameters containing the target resource IRI.
     * The request is directed to the RDBAccessAgent.
     * @param targetUrl
     * @return
     */

    public static Object[] createRDBAccessRequestUrl(String targetUrl) {

        String requestUrl;

        JSONObject joparams ;

        requestUrl = getBaseWorldUrlforRDBAccess(targetUrl);

        joparams = new JSONObject();
        joparams.put(JPSConstants.TARGETIRI, AccessAgentCaller.cutHashFragment(targetUrl));

        Object[] a = new Object[] {requestUrl, joparams};
        return a;
    }

    /**
     * Get the base world RDB Access Agent url.
     * The scheme, host and port of the target url is preserved, while
     * the path is changed to the RDB Access Agent Path.
     * If no scheme, host or port is provided then url will default to the values
     * provided by JPSConstants e.g. "http://www.theworldavatar.com:83/access-agent/rdbaccess"
     * @param url
     * @return
     */
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
