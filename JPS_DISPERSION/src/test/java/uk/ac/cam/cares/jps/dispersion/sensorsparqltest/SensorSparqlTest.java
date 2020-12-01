package uk.ac.cam.cares.jps.dispersion.sensorsparqltest;

import java.sql.SQLException;

import org.junit.Test;
import uk.ac.cam.cares.jps.dispersion.sensorsparql.SensorSparql;

public class SensorSparqlTest {
	String testrepo = "http://localhost:8080/rdf4j-server/repositories/stationtest/statements";
	String blazegraphrepo = "http://localhost:8080/blazegraph/namespace/weatherstations/sparql";
	String freddierepo = "https://kg.cmclinnovations.com/rdf4j-server/repositories/stationtest";
	@Test
	public void testCreateWeatherStation() throws SQLException {
		String station_name = "weatherstation1";
		double [] xyz_coord = {4,25,3};
		
		SensorSparql ws = new SensorSparql();
		ws.createWeatherStation(station_name, xyz_coord, blazegraphrepo);
	}
	
	@Test
	public void testCreateAirQualityStation() throws SQLException {
		String station_name = "airqualitystation1";
		double [] xyz_coord = {104,2,9};
		
		SensorSparql ws = new SensorSparql();
		ws.createAirQualityStation(station_name, xyz_coord, blazegraphrepo);
	}
	
	@Test
	public void testQueryCoordinates() {
		SensorSparql ws = new SensorSparql();
		ws.queryCoordinates(testrepo);
	}
	
	@Test
	public void testClearEndpoint() throws SQLException {
		SensorSparql ws = new SensorSparql();
		ws.clearEndpoint(blazegraphrepo);
	}
	
	@Test
	/**
	 * To test this, first create a graph called <http://www.theworldavatar.com/kb/weatherstations/weatherstation1>
	 * @throws SQLException
	 */
	public void testDeleteGraph() throws SQLException {
		String graph = "<http://www.theworldavatar.com/kb/weatherstations/weatherstation1>";
		SensorSparql ws = new SensorSparql();
		ws.deleteGraph(blazegraphrepo,graph);
	}
}
