package uk.ac.cam.cares.jps.agent.cea.data;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.Map;

public class CEAMetaData {
    private List<CEAGeometryData> surrounding;
    private List<OffsetDateTime> weatherTimes;
    private Map<String, List<Double>> weather;
    private List<Double> weatherMetaData;
    private byte[] terrain;

    public CEAMetaData(List<CEAGeometryData> surroundingValue, List<OffsetDateTime> weatherTimesValue, Map<String, List<Double>> weatherValue, List<Double> weatherMetaDataValue, byte[] terrainValue) {
        this.surrounding = surroundingValue;
        this.weatherTimes = weatherTimesValue;
        this.weather = weatherValue;
        this.weatherMetaData = weatherMetaDataValue;
        this.terrain = terrainValue;
    }

    public List<CEAGeometryData> getSurrounding() {return this.surrounding;}

    public List<OffsetDateTime> getWeatherTimes() {return this.weatherTimes;}

    public Map<String, List<Double>> getWeather() {return this.weather;}

    public List<Double> getWeatherMetaData() {return this.weatherMetaData;}

    public byte[] getTerrain() {return this.terrain;}

    public void setSurrounding(List<CEAGeometryData> surroundingValue) {this.surrounding = surroundingValue;}

    public void setWeatherTimes(List<OffsetDateTime> weatherTimesValue) {this.weatherTimes = weatherTimesValue;}

    public void setWeather(Map<String, List<Double>> weatherValue) {this.weather = weatherValue;}

    public void setWeatherMetaData(List<Double> weatherMetaDataValue) {this.weatherMetaData = weatherMetaDataValue;}

    public void setTerrain(byte[] terrainValue) {this.terrain = terrainValue;}
}
