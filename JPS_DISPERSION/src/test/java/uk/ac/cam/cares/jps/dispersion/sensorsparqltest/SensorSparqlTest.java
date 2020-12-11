package uk.ac.cam.cares.jps.dispersion.sensorsparqltest;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Test;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.region.Region;
import uk.ac.cam.cares.jps.base.region.Scope;
import uk.ac.cam.cares.jps.dispersion.sensorsparql.SensorSparql;

public class SensorSparqlTest extends TestCase{
	String testrepo = "http://localhost:8080/rdf4j-server/repositories/stationtest/statements";
	String blazegraphrepo = "http://localhost:8080/blazegraph/namespace/weatherstations/sparql";
	String freddierepo = "https://kg.cmclinnovations.com/rdf4j-server/repositories/stationtest";

	public void testCreateWeatherStation() {
		String station_name = "weatherstation1";
		double [] xyz_coord = {4,25,3};
		
		SensorSparql ws = new SensorSparql();
		ws.createWeatherStation(station_name, xyz_coord, blazegraphrepo);
	}
	
	public void testCreateAirQualityStation() {
		String station_name = "airqualitystation1";
		double [] xyz_coord = {104,2,9};
		
		SensorSparql ws = new SensorSparql();
		ws.createAirQualityStation(station_name, xyz_coord);
	}

	public void testQueryAirStationsWithinScope() {
		JSONObject jo = new JSONObject();
        Region.putRegion(jo, 2);
        Scope sc = new Scope(jo.getJSONObject(Region.keyRegion));
		SensorSparql ws = new SensorSparql();
		JSONArray result = ws.queryAirStationsWithinScope(sc);
	}
	
	public void testQueryAirStationCoordinatesWithIRI() {
		String station_iri = "http://www.theworldavatar.com/ontology/ontostation/OntoStation.owl#airqualitystation1";
		SensorSparql ss = new SensorSparql();
		ss.queryAirStationCoordinatesWithIRI(station_iri);
	}
}
