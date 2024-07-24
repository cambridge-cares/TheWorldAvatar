package uk.ac.cam.cares.jps.agent.pips;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpRequest;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.*;

import org.apache.http.ssl.SSLContextBuilder;
import org.apache.http.conn.ssl.NoopHostnameVerifier;

import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateException;

public class PIPSTSAgentAPIConnector {
    private static final Logger LOGGER = LogManager.getLogger(PIPSTSAgentAPIConnector.class);

    // variables
    private String pipsAgentTimeSeriesPath;
    private static final String CERT_FILE_ENV = "CLIENT_CERT";
    private static final String CERT_PASSWORD_ENV = "CLIENT_CERT_PASSWORD";

    // error messages
    private static final String RETRIEVE_CLIENT_CERT_ERROR_MSG = "Could not retrieve client certificate. ";
    private static final String SEND_CLIENT_CERT_ERROR_MSG = "Could not send request loaded with client certificate. ";

    // error variables
    private static final String JSON_ERROR_KEY = "Error";

    /**
     * Constructor for PIPSTSAgentAPIConnector
     * @throws IOException 
     * @throws FileNotFoundException 
     * @throws KeyStoreException 
     * @throws CertificateException 
     * @throws NoSuchAlgorithmException 
     */
    public PIPSTSAgentAPIConnector(){
        pipsAgentTimeSeriesPath = System.getenv("PIPS_AGENT_TIMESERIES_PATH");
    }

    /**
     * Get timeseries data from pips-timeseries-agent
     * @return result of request (e.g. unauthorized, authorized, invalid token)
     * @throws JSONException
     * @throws IOException
     */
    public JSONObject getTimeSeries(String accessToken, String source, int num, Boolean include_client_cert) throws JSONException, IOException {
        return retrieveTimeSeriesData(accessToken, source, num, include_client_cert);
    }

    /**
     * send get request to pips-timeseries-agent with bearer token
     * @param accessToken token to attached as bearer token
     * @return response from pips-timeseries-agent
     * @throws IOException
     */
    private JSONObject retrieveTimeSeriesData(String accessToken, String source, int num, Boolean include_client_cert) throws IOException {
        String newPath = pipsAgentTimeSeriesPath + "?source=" + source + "&num=" + num;
        if (!include_client_cert) {
            LOGGER.info("Attempting to send request without client certificates...");
            try (CloseableHttpClient httpClient = HttpClients.createDefault()) {
                HttpGet readingRequest = new HttpGet(newPath);
                setTokenAuthorization(readingRequest, accessToken);
    
                try (CloseableHttpResponse response = httpClient.execute(readingRequest)) {
                    int status = response.getStatusLine().getStatusCode();
                    if (status == 200) {
                        return new JSONObject(EntityUtils.toString(response.getEntity()));
                    }
                    else {
                        JSONObject error_response = new JSONObject();
                        error_response.put(JSON_ERROR_KEY, response.getStatusLine().getReasonPhrase());
                        LOGGER.error(error_response);
                        return error_response;
                    }
                }
            }
        } else {
            // Load the client certificate
            KeyStore keyStore;
            SSLContextBuilder sslContextBuilder = SSLContextBuilder.create();
            try {
                keyStore = KeyStore.getInstance("PKCS12");
                Utils utils = new Utils();
                try (FileInputStream keyStoreStream = new FileInputStream(System.getenv(CERT_FILE_ENV))) {
                    keyStore.load(keyStoreStream, utils.readFromFile(System.getenv(CERT_PASSWORD_ENV)).toCharArray());
                }
                // Create SSLContext with the client certificate
                sslContextBuilder.loadKeyMaterial(keyStore,  utils.readFromFile(System.getenv(CERT_PASSWORD_ENV)).toCharArray());
            } catch (Exception e) {
                JSONObject error_response = new JSONObject();
                error_response.put(JSON_ERROR_KEY, RETRIEVE_CLIENT_CERT_ERROR_MSG);
                LOGGER.error(error_response);
                return error_response;
            }

            // Create HttpClient with the SSLContext
            try {
                LOGGER.info("Attempting to send request with client certificates...");
                CloseableHttpClient httpClient = HttpClients.custom()
                .setSSLContext(sslContextBuilder.build())
                .setSSLHostnameVerifier(NoopHostnameVerifier.INSTANCE)
                .build();
                HttpGet readingRequest = new HttpGet(newPath);
                setTokenAuthorization(readingRequest, accessToken);
    
                try (CloseableHttpResponse response = httpClient.execute(readingRequest)) {
                    int status = response.getStatusLine().getStatusCode();
                    if (status == 200) {
                        return new JSONObject(EntityUtils.toString(response.getEntity()));
                    }
                    else {
                        JSONObject error_response = new JSONObject();
                        error_response.put(JSON_ERROR_KEY, response.getStatusLine().getReasonPhrase());
                        LOGGER.error(error_response);
                        return error_response;
                    }
                }
            } catch (Exception e) {
                throw new JPSRuntimeException(SEND_CLIENT_CERT_ERROR_MSG, e);
            }
        }
    }

    /**
     * Sets the current token as authorization in the header of the request
     * @param request The request to which to add the token authorization
     */
    private void setTokenAuthorization(HttpRequest request, String accessToken) {
        String authHeader = "Bearer " + accessToken;
        request.setHeader(HttpHeaders.AUTHORIZATION, authHeader);
    }

}
