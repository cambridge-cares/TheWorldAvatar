package uk.ac.cam.cares.jps.agent.openmeteoagent;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

@WebServlet(
        urlPatterns = {
                OpenMeteoAgent.URI_INSTANTIATE,
                OpenMeteoAgent.URI_DELETE
        })
public class OpenMeteoAgent extends JPSAgent {
    private static final String KEY_LAT = "latitude";
    private static final String KEY_LONG = "longitude";
    private static final String KEY_START = "start_date";
    private static final String KEY_END = "end_date";
    private static final String KEY_HOURLY = "hourly";

    private static final String API_URL = "https://archive-api.open-meteo.com/v1/archive";
    private static final String API_PARAMETER_TEMP = "temperature_2m";
    private static final String API_PARAMETER_HUMIDITY = "relativehumidity_2m";
    private static final String API_PARAMETER_DEWPOINT = "dewpoint_2m";
    private static final String API_PARAMETER_PRESSURE = "surface_pressure";
    private static final String API_PARAMETER_RAIN = "rain";
    private static final String API_PARAMETER_SNOW = "snowfall";
    private static final String API_PARAMETER_CLOUD = "cloudcover";
    private static final String API_PARAMETER_DNI = "direct_normal_irradiance";
    private static final String API_PARAMETER_DHI = "diffuse_radiation";
    private static final String API_PARAMETER_WINDSPEED = "windspeed_10m";
    private static final String API_PARAMETER_WINDDIRECTION = "winddirection_10m";

    private List<String> API_PARAMETERS = Arrays.asList(API_PARAMETER_TEMP, API_PARAMETER_HUMIDITY, API_PARAMETER_DEWPOINT, API_PARAMETER_PRESSURE, API_PARAMETER_RAIN, API_PARAMETER_SNOW, API_PARAMETER_CLOUD, API_PARAMETER_DNI, API_PARAMETER_DHI, API_PARAMETER_WINDSPEED, API_PARAMETER_WINDDIRECTION);

    private String latitude;
    private String longitude;

    public static final String URI_INSTANTIATE = "/instantiate";
    public static final String URI_DELETE = "/delete";

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        return processRequestParameters(requestParams);
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        if (validateInput(requestParams)) {
            latitude = requestParams.getString(KEY_LAT);
            longitude = requestParams.getString(KEY_LONG);
            JSONObject weatherData = getWeatherData(latitude, longitude, requestParams.getString(KEY_START), requestParams.getString(KEY_END));
        }
        return requestParams;
    }

    /**
     * Checks validity of incoming request
     * @param requestParams Request parameters as JSONObject
     * @return Validity of request
     */
    @Override
    public boolean validateInput(JSONObject requestParams) {
        boolean validate;

        try{
            // check latitude and longitude are provided, and are numbers
            validate = !requestParams.get(KEY_LAT).equals(null) && ! requestParams.get(KEY_LONG).equals(null)
                    && !(requestParams.get(KEY_LAT) instanceof String) && !(requestParams.get(KEY_LONG) instanceof String);

            if (requestParams.getString("requestUrl").contains(URI_INSTANTIATE)){
                validate = !requestParams.getString(KEY_START).isEmpty() && !requestParams.getString(KEY_END).isEmpty()
                        && validateDate(requestParams.getString(KEY_START))
                        && validateDate(requestParams.getString(KEY_END));

                SimpleDateFormat ymd = new SimpleDateFormat("yyyy-MM-dd");
                Date start = ymd.parse(requestParams.getString(KEY_START));
                Date end = ymd.parse(requestParams.getString(KEY_END));
                Date now = new Date();

                // start date cannot be later than end date, and end date cannot be later than the date at the time of the incoming request
                validate = validate && start.before(end)
                        && (end.before(now) || end.equals(now));
            }

        }
        catch (Exception e){
            throw new BadRequestException();
        }

        if (!validate){
            throw new BadRequestException();
        }

        return true;
    }

    /**
     * Validate whether the string is in the yyyy-mm-dd date format
     * @param date The date as a string
     * @return Validity of the format of date
     */
    public boolean validateDate(String date){
        try {
            DateFormat df = new SimpleDateFormat("yyyy-MM-dd");
            Date dateCheck = df.parse(date);
            return df.format(dateCheck).equals(date);
        } catch (ParseException e) {
            throw new BadRequestException();
        }
    }

    /**
     * Calls the Open-Meteo Historical Weather API to retrieve the hourly weather data from start_date to end_date with weather parameter in API_PARAMETERS for weather station located at (latitude, longitude)
     * @param latitude Geographical WGS84 coordinate of the weather station
     * @param longitude Geographical WGS84 coordinate of the weather station
     * @param startDate Start date of the time interval, in yyyy-mm-dd format
     * @param endDate End date of the time interval, in yyyy-mm-dd format
     * @return Weather data as a JSONObject
     */
    public JSONObject getWeatherData(String latitude, String longitude, String startDate, String endDate) {
        String query = KEY_LAT + "=" + latitude + "&";
        query = query + KEY_LONG + "=" + longitude + "&";
        query = query + KEY_START + "=" + startDate + "&";
        query = query + KEY_END + "=" + endDate + "&" + KEY_HOURLY + "=";

        for (String parameter: API_PARAMETERS){
            query = query + parameter + ",";
        }

        query = query.substring(0, query.length() - 1);

        try {
            URLConnection connection = new URL(API_URL + "?" + query).openConnection();

            InputStream is = connection.getInputStream();
            BufferedReader rd = new BufferedReader(new InputStreamReader(is));
            StringBuffer response = new StringBuffer();
            String line;
            while ((line = rd.readLine()) != null && !line.isEmpty()){
                response.append(line);
                response.append('\r');
            }
            rd.close();
            return new JSONObject(response.toString());
        }
        catch (IOException e){
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }


}