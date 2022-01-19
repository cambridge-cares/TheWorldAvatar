package uk.ac.cam.cares.jps.agent.flood.objects;

public class Station {
    private String iri;
    private int visId;
    private double lat;
    private double lon;
    private String name;
    
    public Station(String iri) {
    	this.iri = iri;
    }
    
    public void setVisId(int visId) {
    	this.visId = visId;
    }
    
    public void setLat(double lat) {
    	this.lat = lat;
    }
    
    public void setLon(double lon) {
    	this.lon = lon;
    }
    
    public void setName(String name) {
    	this.name = name;
    }
    
    public String getIri() {
    	return this.iri;
    }
    
    public String getName() {
    	return this.name;
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
}
