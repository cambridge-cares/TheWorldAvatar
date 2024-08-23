package uk.ac.cam.cares.jps.agent.solarkatasteragent;

import java.sql.Connection;
import java.time.OffsetDateTime;
import java.util.*;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;

import java.time.Duration;
import java.time.temporal.ChronoUnit;

@WebServlet(urlPatterns = {"/run"})
public class SolarkatasterAgent extends JPSAgent {
    public static final String KEY_OID = "oid";
    public static final String KEY_MOD = "mod_id";
    public static final String KEY_JAN = "jan_median";
    public static final String KEY_FEB = "feb_median";
    public static final String KEY_MRZ = "mrz_median";
    public static final String KEY_APR = "apr_median";
    public static final String KEY_MAI = "mai_median";
    public static final String KEY_JUN = "jun_median";
    public static final String KEY_JUL = "jul_median";
    public static final String KEY_AUG = "aug_median";
    public static final String KEY_SEP = "sep_median";
    public static final String KEY_OKT = "okt_median";
    public static final String KEY_NOV = "nov_median";
    public static final String KEY_DEZ = "dez_median";
    public List<String> TIME_SERIES = Arrays.asList(KEY_JAN, KEY_FEB, KEY_MRZ, KEY_APR, KEY_MAI, KEY_JUN, KEY_JUL, KEY_AUG, KEY_SEP, KEY_OKT, KEY_NOV, KEY_DEZ);
    public static final String KEY_TABLE = "table";
    public static final String KEY_CHUNK = "chunk";

    private String dbUrl;
    private String dbUser;
    private String dbPassword;
    private String timeseriesDBUrl;
    private String tsUser;
    private String tsPassword;
    private TimeSeriesClient tsClient;
    private RemoteRDBStoreClient tsRDBStoreClient;
    private RemoteRDBStoreClient rdbStoreClient;
    private RemoteStoreClient storeClient;

    private String ubemURI;
    private String ubemSolar;

    public SolarkatasterAgent() {readConfig();}

    /**
     * Processes HTTP requests with originating details.
     * @param requestParams Request parameters in a JSONObject.
     * @param request HTTP Servlet Request.
     * @return response in JSON format.
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        return processRequestParameters(requestParams);
    }

    /**
     * Processes HTTP requests.
     * @param requestParams Request parameters as a JSONObject.
     * @return response in JSON format.
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        if (validateInput(requestParams)){
            JSONArray OIDArray = rdbStoreClient.executeQuery(getMinMaxOidString(requestParams.getString(KEY_TABLE)));

            Integer max = OIDArray.getJSONObject(0).getInt("max");

            Integer start = OIDArray.getJSONObject(0).getInt("min");

            Integer step = Math.round((max - start) / requestParams.getInt(KEY_CHUNK));

            ArrayList<List> dataArrayList;
            JSONArray dataArray;

            while(start <= max) {
                dataArray = rdbStoreClient.executeQuery(getQueryString(requestParams.getString(KEY_TABLE), start, start + step));

                dataArrayList = parseDataToLists(dataArray);

                createTimeSeries(dataArrayList.get(0));

                try (Connection conn = tsRDBStoreClient.getConnection()){
                    tsClient.bulkaddTimeSeriesData(dataArrayList.get(1), conn);
                }
                catch (Exception e){
                    e.printStackTrace();
                    throw new JPSRuntimeException(e);
                }

                start = start + step + 1;
            }
        }
        return requestParams;
    }

    /**
     * Checks the incoming JSON request for validity.
     * @param requestParams JSON request parameters.
     * @return request validity
     */
    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        try {
            if (requestParams.get(KEY_TABLE).toString().isEmpty() || !(requestParams.get(KEY_CHUNK) instanceof Integer)) {
                throw new BadRequestException();
            }
            else if (requestParams.getInt(KEY_CHUNK) < 1) {
                throw new BadRequestException();
            }
        }
        catch (Exception e){
            throw new BadRequestException();
        }
        return true;
    }

    /**
     * Gets variables from config.properties
     */
    private void readConfig() {
        ResourceBundle config = ResourceBundle.getBundle("config");

        ubemURI = config.getString("uri.ontology.ubem");
        ubemSolar = ubemURI + "MonthlyAverageSolarIrradiationValue";
        dbUrl = config.getString("db.url");
        dbUser = config.getString("db.user");
        dbPassword = config.getString("db.password");
        timeseriesDBUrl = config.getString("timeseriesDB.url");
        tsUser = config.getString("ts.user");
        tsPassword = config.getString("ts.password");

        rdbStoreClient = new RemoteRDBStoreClient(dbUrl, dbUser, dbPassword);
        storeClient = new RemoteStoreClient(config.getString("timeseries.query.endpoint"), config.getString("timeseries.update.endpoint"));
        tsClient = new TimeSeriesClient(storeClient, OffsetDateTime.class);
        tsRDBStoreClient = new RemoteRDBStoreClient(timeseriesDBUrl, tsUser, tsPassword);
    }

    /**
     * Construct query as a string for Solarkataster time series parameters
     * @param tableName name of table for which to query Solarkataster data from
     * @param start starting oid
     * @param end ending oid
     * @return SQL query as a string
     */
    private String getQueryString(String tableName, Integer start, Integer end) {
        String query;

        query = "SELECT " + KEY_MOD;

        for (int i = 0; i < TIME_SERIES.size(); i++){
            query = query + ", " + TIME_SERIES.get(i);
        }

        query = query + " FROM \"" + tableName + "\" ";

        return query + "WHERE " + KEY_OID + " BETWEEN " + start + " AND " + end;
    }

    /**
     * Constructs and returns a list of doubles of the TIME_SERIES parameters value in data
     * @param data SQL query response for one building
     * @return List of Double
     */
    private List<Double> getDoubleList (JSONObject data) {
        List<Double> dataList = new ArrayList<>();

        for (int i = 0; i < TIME_SERIES.size(); i++){
            if(data.has(TIME_SERIES.get(i))){
                dataList.add(data.getDouble(TIME_SERIES.get(i)));
            }
            else{
                dataList.add(Double.NaN);
            }
        }

        return dataList;
    }

    /**
     * Initialise time series that will be be associated with dataIRI
     * @param dataIRI IRIs to which the timeseriesIRI will be associated to
     */
    private void createTimeSeries(List<List<String>> dataIRI) {
        int n = dataIRI.size();

        List<List<Class<?>>> dataClass = Collections.nCopies(n, Arrays.asList(Double.class));
        List<String> timeUnit = Collections.nCopies(n, null);
        List<TimeSeriesClient.Type> type = Collections.nCopies(n, TimeSeriesClient.Type.AVERAGE);
        List<Duration> durations = Collections.nCopies(n, Duration.ofDays(31));
        List<ChronoUnit> units = Collections.nCopies(n, ChronoUnit.MONTHS);

        try (Connection conn = tsRDBStoreClient.getConnection()){
            tsClient.bulkInitTimeSeries(dataIRI, dataClass, timeUnit, conn, type, durations, units);
        }
        catch (Exception e){
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }

    /**
     * Parse dataArray into dataIRIs and corresponding TimeSeries
     * @param dataArray JSONArray of SQL query results
     * @return ArrayList of dataIRIs and TimeSeries
     */
    private ArrayList<List> parseDataToLists(JSONArray dataArray) {
        ArrayList<List> output = new ArrayList<>();

        JSONObject temp;
        List<TimeSeries<Double>> tsList = new ArrayList<>();
        List<List<String>> dataIRI = new ArrayList<>();
        List<?> times = Collections.nCopies(TIME_SERIES.size(), null);

        for (int i = 0; i < dataArray.length(); i++){
            temp = dataArray.getJSONObject(i);
            dataIRI.add(Arrays.asList(ubemSolar + "_" + temp.getString(KEY_MOD)));
            tsList.add(new TimeSeries(times, dataIRI.get(i), Arrays.asList(getDoubleList(temp))));
        }

        output.add(dataIRI);
        output.add(tsList);

        return output;
    }

    /**
     * Construct query for smallest largest OID as a string
     * @param tableName name of table for which to query Solarkataster data from
     * @return SQL query as a string
     */
    private String getMinMaxOidString(String tableName) {
        return "SELECT MIN(" + KEY_OID + "), MAX(" + KEY_OID + ") FROM \"" + tableName + "\"";
    }
}

