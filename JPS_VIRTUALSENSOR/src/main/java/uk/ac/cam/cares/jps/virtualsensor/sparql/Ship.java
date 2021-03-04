package uk.ac.cam.cares.jps.virtualsensor.sparql;

import org.json.JSONObject;

public class Ship {
	private double speed; // speed is in knot!
	private String type;
	private double x;
	private double y;
	private Chimney chim;
	
    public Ship (String shipIRI, boolean withChimney) {
    	if (withChimney) {
    		JSONObject queryresult = ShipSparql.queryShipProperties(shipIRI);
    		this.speed = queryresult.getDouble("ss");
    		this.type = queryresult.getString("type");
    	} else {
    		double[] coordinates = ShipSparql.queryShipCoordinates(shipIRI);
    		this.x = coordinates[0];
    		this.y = coordinates[1];
    		this.chim = new Chimney(shipIRI);
    	}
    }
    
    public double getSpeed() {
    	return this.speed;
    }
    public String getType() {
    	return this.type;
    }
    public Chimney getChimney() {
    	return this.chim;
    }
    public double getXCoord() {
    	return this.x;
    }
    public double getYCoord() {
    	return this.y;
    }
}
