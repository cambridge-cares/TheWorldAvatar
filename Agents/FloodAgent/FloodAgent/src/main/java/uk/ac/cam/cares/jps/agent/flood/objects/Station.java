package uk.ac.cam.cares.jps.agent.flood.objects;

import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;

public class Station {
    private String iri;
    private int visId;
    private double lat;
    private double lon;
    private String identifier;
    private String label;
    private String river;
    private String catchment;
    private String town;
    private String dateOpened;
    private Map<String, String> displayProperties;
    private List<TimeSeries<Instant>> ts_list;
    private Map<String, String> measureNameMap; // measure IRI to parameter name
    private Map<String, String> measureUnitMap; // measure IRI to unit
    private Map<String, String> measureSubTypeMap; // measure IRI to sub type name

    public Station(String iri) {
    	this.iri = iri;
    	this.identifier = "";
    	this.label = "";
    	this.river = "";
    	this.catchment = "";
    	this.town = "";
    	this.dateOpened = "";
    	this.displayProperties = new HashMap<String, String>();
    	this.ts_list = new ArrayList<>();
    	this.measureNameMap = new HashMap<>();
    	this.measureUnitMap = new HashMap<>();
    	this.measureSubTypeMap = new HashMap<>();
    }
    
    public void setLabel(String label) {
    	this.label = label;
    	this.displayProperties.put("Name", label);
    }
    
    public String getLabel() {
    	return this.label;
    }
    
    public void setRiver(String river) {
    	this.river = river;
    	this.displayProperties.put("River", river);
    }
    
    public String getRiver() {
    	return this.river;
    }
    
    public void setCatchment(String catchment) {
    	this.catchment = catchment;
    	this.displayProperties.put("Catchment", catchment);
    }
    
    public String getCatchment() {
    	return this.catchment;
    }
    
    public void setTown(String town) {
    	this.town = town;
    	this.displayProperties.put("Town", town);
    }
    
    public String getTown() {
    	return this.town;
    }
    
    public void setDateOpened(String dateOpened) {
    	this.dateOpened = dateOpened;
    	this.displayProperties.put("Date opened", dateOpened);
    }
    
    public String getDateOpened() {
    	return this.dateOpened;
    }
    
    public void setVisId(int visId) {
    	this.visId = visId;
    }
    
    public void setLat(double lat) {
    	this.lat = lat;
    	this.displayProperties.put("Latitude", String.valueOf(lat));
    }
    
    public void setLon(double lon) {
    	this.lon = lon;
    	this.displayProperties.put("Longitude", String.valueOf(lon));
    }
    
    public void setIdentifier(String identifier) {
    	this.identifier = identifier;
    	this.displayProperties.put("Identifier", identifier);
    }
    
    public String getIri() {
    	return this.iri;
    }
    
    public String getIdentifier() {
    	return this.identifier;
    }
    
    public double getLat() {
    	return this.lat;
    }
    
    public double getLon() {
    	return this.lon;
    }
    
    public int getVisId() {
    	return this.visId;
    }
    
    public Map<String, String> getDisplayProperties() {
    	return this.displayProperties;
    }
    
    public void addTimeseries(TimeSeries<Instant> ts) {
    	this.ts_list.add(ts);
    }
    
    public List<TimeSeries<Instant>> getTimeSeriesList() {
    	return this.ts_list;
    }
    
    // combine time series list into a single time series object
    public TimeSeries<Instant> getCombinedTimeSeries() {
    	// TO BE CORRECTED
    	return this.ts_list.get(0);
    }
    
    public void setMeasureName(String measureIri, String measureName) {
    	this.measureNameMap.put(measureIri, measureName);
    }
    
    public String getMeasureName(String measure) {
    	return this.measureNameMap.get(measure);
    }
    
    public void setMeasureSubTypeName(String measureIri, String subTypeName) {
    	this.measureSubTypeMap.put(measureIri, subTypeName);
    }
    
    public String getMeasureSubTypeName(String measure) {
    	return this.measureSubTypeMap.get(measure);
    }
    
    public void setMeasureUnit(String measureIri, String unit) {
    	this.measureUnitMap.put(measureIri, unit);
    }
    
    public String getMeasureUnit(String measure) {
    	return this.measureUnitMap.get(measure);
    }
}
