package uk.ac.cam.cares.jps.agent.cea.utils.datahandler;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import org.json.JSONArray;
import org.json.JSONObject;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;

public class DataParser {
    /**
     * Parses input JSONObject into a list of strings
     * @param dataJSON - request body in JSON format
     * @param key - requested data
     * @return List of data
     */
    public static List<String> getList (JSONObject dataJSON, String key) {
        JSONArray array = (JSONArray) dataJSON.get(key);
        List<String> list = new ArrayList<>();
        for (int j = 0; j < array.length(); j++) {
            list.add(array.getString(j));
        }
        return list;
    }

    /**
     * Parses input JSONObject into a list of time series data
     * @param dataJSON - request body in JSON format
     * @param key - requested data
     * @return List of data
     */
    public static List<Double> getTimeSeriesList (JSONObject dataJSON, String key, Integer index) {
        List<Double> timeSeriesList = new ArrayList<>();

        if (dataJSON.has(key)) {
            JSONArray array = (JSONArray) dataJSON.get(key);
            JSONArray timeDataArray = (JSONArray) array.get(index);

            for (int i = 0; i < timeDataArray.length(); i++) {
                timeSeriesList.add(Double.valueOf(timeDataArray.getString(i)));
            }
        }
        return timeSeriesList;
    }

    /**
     * Parses input JSONObject into a list of times
     * @param dataJSON - request body in JSON format
     * @param key - requested data
     * @return List of times
     */
    public static List<OffsetDateTime> getTimesList (JSONObject dataJSON, String key) {
        JSONArray array = (JSONArray) dataJSON.get(key);
        List<OffsetDateTime> list = new ArrayList<>();
        for (int j = 0; j < array.length(); j++) {
            OffsetDateTime odt = OffsetDateTime.parse(array.getString(j));
            list.add(odt);
        }
        return list;
    }

    /**
     * Calculates annual value by summing all data in column in time series and rounding to 2dp
     * @param timeSeries time series data
     * @param dataIri iri in time series database
     * @return annualValue as a String
     */
    public static String calculateAnnual(TimeSeries<OffsetDateTime> timeSeries, String dataIri){
        List<Double> values = timeSeries.getValuesAsDouble(dataIri);
        Double annualValue = 0.;
        for(Double value : values){
            annualValue += value;
        }
        annualValue = Math.round(annualValue*Math.pow(10,2))/Math.pow(10,2);
        return annualValue.toString();

    }
}
