package uk.ac.cam.cares.jps.agent.cea.data;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class CEAInputData {
    public String geometry;
    public String height;
    public Map<String, Double> usage;
    public ArrayList<CEAInputData> surrounding;
    public List<OffsetDateTime> weatherTimes;
    public Map<String, List<Double>> weather;
    public List<Double> weatherMetaData;

    public CEAInputData(String geometry_value, String height_value, Map<String, Double> usage_value, ArrayList<CEAInputData> surrounding_value, List<OffsetDateTime> weatherTimes_value, Map<String, List<Double>> weather_value, List<Double> weatherMetaData_value) {
        this.geometry = geometry_value;
        this.height = height_value;
        this.usage = usage_value;
        this.surrounding = surrounding_value;
        this.weatherTimes = weatherTimes_value;
        this.weather = weather_value;
        this.weatherMetaData = weatherMetaData_value;
    }

    public String getGeometry() {
        return this.geometry;
    }

    public String getHeight() {
        return this.height;
    }

    public Map<String, Double> getUsage() {
        return this.usage;
    }

    public ArrayList<CEAInputData> getSurrounding() {
        return this.surrounding;
    }

    public List<OffsetDateTime> getWeatherTimes() {return this.weatherTimes;}

    public Map<String, List<Double>> getWeather() {return this.weather;}

    public List<Double> getWeatherMetaData() {return this.weatherMetaData;}

    public void setGeometry(String geometry_value) {
        this.geometry = geometry_value;
    }

    public  void setHeight(String height_value) {
        this.height = height_value;
    }

    public void setUsage(Map<String, Double> usage_value) {
        this.usage = usage_value;
    }

    public void setSurrounding(ArrayList<CEAInputData> surrounding_value) {
        this.surrounding = surrounding_value;
    }

    public void setWeatherTimes(List<OffsetDateTime> weatherTimes_value) {this.weatherTimes = weatherTimes_value;}

    public void setWeather(Map<String, List<Double>> weather_value) {this.weather = weather_value;}

    public void setWeatherMetaData(List<Double> weatherMetaData_value) {this.weatherMetaData = weatherMetaData_value;}
}
