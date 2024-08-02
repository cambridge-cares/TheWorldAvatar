package uk.ac.cam.cares.jps.agent.cea.utils.datahandler;

import org.json.JSONArray;
import org.json.JSONObject;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;

public class DataParser {
    /**
     * Parses input JSONObject into a list of strings
     * @param dataJSON request body in JSON format
     * @param key requested data
     * @return a list of data
     */
    public static List<Double> getList (JSONObject dataJSON, String key) {
        JSONArray array = (JSONArray) dataJSON.get(key);
        List<Double> list = new ArrayList<>();
        for (int j = 0; j < array.length(); j++) {
            list.add(array.getDouble(j));
        }
        return list;
    }

    /**
     * Parses input JSONObject into a list of time series data
     * @param dataJSON request body in JSON format
     * @param key requested data
     * @return a list of time series data
     */
    public static List<Double> getTimeSeriesList (JSONObject dataJSON, String key, Integer index) {
        List<Double> timeSeriesList = new ArrayList<>();

        if (dataJSON.has(key)) {
            JSONArray array = (JSONArray) dataJSON.get(key);
            JSONArray timeDataArray = (JSONArray) array.get(index);

            for (int i = 0; i < timeDataArray.length(); i++) {
                timeSeriesList.add(timeDataArray.getDouble(i));
            }
        }
        return timeSeriesList;
    }

    /**
     * Parses input JSONObject into a list of timestamps
     * @param dataJSON  request body in JSON format
     * @param key requested data
     * @return a list of timestamps
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
}
