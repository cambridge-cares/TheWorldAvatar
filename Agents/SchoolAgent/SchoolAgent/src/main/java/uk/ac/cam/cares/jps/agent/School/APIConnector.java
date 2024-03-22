package uk.ac.cam.cares.jps.agent.School;

import org.apache.http.client.HttpResponseException;
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
import java.util.Properties;

public class APIConnector {
    private static final String API_ERROR_MSG = "Data cannot be retrieved from the following: ";
    private static final Logger LOGGER = LogManager.getLogger(APIConnector.class);

    String schoolGeneralInfoUrl;
    String schoolProgrammesUrl;
    String schoolCcaUrl;

    // Constructor to initialise the variables according to the Properties file
    public APIConnector(String filepath) throws IOException {
        loadAPIConfigs(filepath);
    }

    /**
     * Retrieve general information of the schools
     * @return general information of the schools
     */
    public JSONObject getGeneralReadings() {
        try {
            return retrieveGeneralData();
        } catch (IOException e) {
            LOGGER.error(API_ERROR_MSG);
            throw new JPSRuntimeException(API_ERROR_MSG + "school general information", e);
        }
    }

    /**
     * Retrieve programmes of the schools
     * @return programmes offered by the schools
     */
    public JSONObject getProgrammes() {
        try {
            return retreiveProgrammes();
        } catch (Exception e) {
            LOGGER.error(API_ERROR_MSG);
            throw new JPSRuntimeException(API_ERROR_MSG + "school programmes", e);
        }
    }

    /**
     * Retrieve available CCAs in the schools
     * @return available CCAs in the schools
     */
    public JSONObject getCCA() {
        try {
            return retrieveCCA();
        } catch (Exception e) {
            LOGGER.error(API_ERROR_MSG);
            throw new JPSRuntimeException(API_ERROR_MSG + "school CCAs", e);
        }
    }

    /**
     * Retrieve general information of the schools via the API
     * @return general information of the schools as a JSONObject
     * @throws IOException
     * @throws JSONException
     */
    private JSONObject retrieveGeneralData() throws IOException, JSONException {
        try (CloseableHttpClient httpclient = HttpClients.createDefault()) {
            HttpGet readrequest = new HttpGet(schoolGeneralInfoUrl);
            try (CloseableHttpResponse response = httpclient.execute(readrequest)) {
                int status = response.getStatusLine().getStatusCode();
                if (status == 200) {
                    return new JSONObject(EntityUtils.toString(response.getEntity()));
                } else {
                    throw new HttpResponseException(status, "Data could not be retrieved due to a server error");
                }
            }
        }

    }

    /**
     * Retrieve programmes offered by the schools via the API
     * @return JSONObject containing programmes offered by the schools
     * @throws IOException
     * @throws JSONException
     */
    private JSONObject retreiveProgrammes() throws IOException, JSONException {
        try (CloseableHttpClient httpClient = HttpClients.createDefault()) {
            HttpGet readrequest = new HttpGet(schoolProgrammesUrl);
            try (CloseableHttpResponse response = httpClient.execute(readrequest)) {
                int status = response.getStatusLine().getStatusCode();
                if (status == 200) {
                    return new JSONObject(EntityUtils.toString(response.getEntity()));
                } else {
                    throw new HttpResponseException(status, "Programmes Data could not be retrieved due to a server");
                }
            }
        }
    }

    /**
     * Retreive CCAs offered by the schools via the API
     * @return JSONObject containing CCAs offered by the school
     * @throws IOException
     * @throws JSONException
     */
    private JSONObject retrieveCCA() throws IOException, JSONException {
        try (CloseableHttpClient httpClient = HttpClients.createDefault()) {
            HttpGet readrequest = new HttpGet(schoolCcaUrl);
            try (CloseableHttpResponse response = httpClient.execute(readrequest)) {
                int status = response.getStatusLine().getStatusCode();
                if (status == 200) {
                    return new JSONObject(EntityUtils.toString(response.getEntity()));
                } else {
                    throw new HttpResponseException(status, "CCA Data could not be retrieved due to a server");
                }
            }
        }
    }

    /**
     * Retrieve API configs from properties file
     * @param filepath filepath of where the properties file is located at
     * @throws IOException
     */
    private void loadAPIConfigs(String filepath) throws IOException {
        File file = new File(filepath);
        if (!file.exists()) {
            throw new FileNotFoundException("There was no file found in the path");
        }

        try (InputStream input = new FileInputStream(file)) {
            Properties prop = new Properties();
            prop.load(input);

            if (prop.containsKey("school.general.info.url")) {
                this.schoolGeneralInfoUrl = prop.getProperty("school.general.info.url");
            } else {
                throw new IOException("The file is missing: \"school.general.info.url=<api_url>\"");
            }

            if (prop.containsKey("school.programmes.url")) {
                this.schoolProgrammesUrl = prop.getProperty("school.programmes.url");
            } else {
                throw new IOException("The file is missing: \"school.programmes.url=<api_url>\"");
            }

            if (prop.containsKey("school.cca.url")) {
                this.schoolCcaUrl = prop.getProperty("school.cca.url");
            } else {
                throw new IOException("The file is missing: \"school.cca.url=<api_url>\"");
            }

        }
    }

}
