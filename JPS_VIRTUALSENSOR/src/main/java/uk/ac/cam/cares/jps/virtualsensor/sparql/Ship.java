package uk.ac.cam.cares.jps.virtualsensor.sparql;

import org.json.JSONObject;

public class Ship {
	private double speed; // speed is in knot!
	private String type;
	
    public Ship (String shipIRI) {
    	JSONObject queryresult = ShipSparql.queryShipProperties(shipIRI);
    	this.speed = queryresult.getDouble("ss");
    	this.type = queryresult.getString("type");
    }
    
    public double getSpeed() {
    	return this.speed;
    }
    public String getType() {
    	return this.type;
    }
}
