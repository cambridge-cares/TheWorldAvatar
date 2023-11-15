package uk.ac.cam.cares.jps.agent.cea.data;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.Map;

public class CEAMetaData {
    public List<CEAGeometryData> surrounding;
    public List<OffsetDateTime> weatherTimes;
    public Map<String, List<Double>> weather;
    public List<Double> weatherMetaData;
    public byte[] terrain;

    public CEAMetaData(List<CEAGeometryData> surrounding_value, List<OffsetDateTime> weatherTimes_value, Map<String, List<Double>> weather_value, List<Double> weatherMetaData_value, byte[] terrain_value) {
        this.surrounding = surrounding_value;
        this.weatherTimes = weatherTimes_value;
        this.weather = weather_value;
        this.weatherMetaData = weatherMetaData_value;
        this.terrain = terrain_value;
    }

    public List<CEAGeometryData> getSurrounding() {return this.surrounding;}

    public List<OffsetDateTime> getWeatherTimes() {return this.weatherTimes;}

    public Map<String, List<Double>> getWeather() {return this.weather;}

    public List<Double> getWeatherMetaData() {return this.weatherMetaData;}

    public byte[] getTerrain() {return this.terrain;}

    public void setSurrounding(List<CEAGeometryData> surrounding_value) {this.surrounding = surrounding_value;}

    public void setWeatherTimes(List<OffsetDateTime> weatherTimes_value) {this.weatherTimes = weatherTimes_value;}

    public void setWeather(Map<String, List<Double>> weather_value) {this.weather = weather_value;}

    public void setWeatherMetaData(List<Double> weatherMetaData_value) {this.weatherMetaData = weatherMetaData_value;}

    public void setTerrain(byte[] terrain_value) {this.terrain = terrain_value;}
}
