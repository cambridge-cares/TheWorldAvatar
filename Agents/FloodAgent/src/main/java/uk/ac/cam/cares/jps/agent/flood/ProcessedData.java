package uk.ac.cam.cares.jps.agent.flood;

import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

public class ProcessedData {
    Map<String, List<Instant>> dataTimeMap;
    Map<String, List<Double>> dataValueMap;

    public ProcessedData() {
        dataTimeMap = new HashMap<>();
        dataValueMap = new HashMap<>();
    }

    public void addData(String dataIRI, Instant timestamp, Double value) {
        if (dataTimeMap.containsKey(dataIRI)) {
            // add timestamp to the list
            dataTimeMap.get(dataIRI).add(timestamp);
            dataValueMap.get(dataIRI).add(value);
        } else {
            // instantiate new lists and add them to the map
            List<Instant> times = new ArrayList<>();
            List<Double> values = new ArrayList<>();
            times.add(timestamp);
            values.add(value);
            dataTimeMap.put(dataIRI, times);
            dataValueMap.put(dataIRI, values);
        }
    }

    public List<Double> getValues(String dataIRI) {
        return this.dataValueMap.get(dataIRI);
    }

    public List<Instant> getTimestamps(String dataIRI) {
        return this.dataTimeMap.get(dataIRI);
    }

    public int getSize() {
        return this.dataTimeMap.size();
    }

    public Iterator<String> getDataIriIterator() {
        return dataTimeMap.keySet().iterator();
    }

    public List<String> getDataIriList() {
        return new ArrayList<>(dataTimeMap.keySet());
    }
}
