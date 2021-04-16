package uk.ac.cam.cares.jps.virtualsensor.sparqltest;

import java.util.List;

import org.json.JSONArray;
import org.json.JSONObject;
import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.region.Region;
import uk.ac.cam.cares.jps.virtualsensor.objects.Scope;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.virtualsensor.objects.WeatherStation;
import uk.ac.cam.cares.jps.virtualsensor.sparql.SensorSparql;

public class SensorSparqlTest extends TestCase{
	public void testCreateWeatherStation() {
		String csvPath = AgentLocator.getPathToWorkingDir(this) + "/station_coordinates.csv";
		String csvFile=new QueryBroker().readFileLocal(csvPath);
		List<String[]> csv_array = MatrixConverter.fromCsvToArray(csvFile);
		
		for (int i = 1; i <csv_array.size(); i++) {
			double x = Double.parseDouble(csv_array.get(i)[0]);
			double y = Double.parseDouble(csv_array.get(i)[1]);
			double z = Double.parseDouble(csv_array.get(i)[2]);
			double [] xyz = {x,y,z};
			SensorSparql.createWeatherStation(i, xyz);
		}
	}
	
	public void testQueryWeatherStationProperties() {
		String stationiri = "http://www.theworldavatar.com/ontology/ontostation/OntoStation.owl#weatherstation1";
		SensorSparql.queryWeatherStationProperties(stationiri);
	}
	
	public void testUpdateWeatherStation() {
		WeatherStation ws = new WeatherStation();
		ws.setStationiri("http://www.theworldavatar.com/ontology/ontostation/OntoStation.owl#weatherstation1");
		ws.setCloudcover(50);
		ws.setHumidity(0.1);
		ws.setPrecipitation(10);
		ws.setPressure(1010);
		ws.setTemperature(30);
		ws.setWinddirection(60);
		ws.setWindspeed(15);
		ws.setTimestamp(100);
		SensorSparql.updateWeatherStation(ws);
	}
	
	public void testQueryWeatherStationTimeStamp() {
		String stationiri = "http://www.theworldavatar.com/ontology/ontostation/OntoStation.owl#weatherstation1";
		SensorSparql.queryWeatherStationTimeStamp(stationiri);
	}
	
	public void testCreateAirQualityStation() {
		String station_name = "virtualsensor1";
		double [] xyz_coord = {104,2,9};
		
		SensorSparql.createAirQualityStation(station_name, xyz_coord);
	}

	public void testQueryAirStationsWithinScope() {
		JSONObject jo = new JSONObject();
        Region.putRegion(jo, 2);
        Scope sc = new Scope(jo.getJSONObject(Region.keyRegion));
		SensorSparql.queryAirStationsWithinScope(sc);
	}
	
	public void testQueryAirStationCoordinatesWithIRI() {
		String station_iri = "http://www.theworldavatar.com/ontology/ontostation/OntoStation.owl#virtualsensor1";
		SensorSparql.queryAirStationCoordinatesWithIRI(station_iri);
	}
	
	public void testAddSensorValue() {
		String station_iri_string = "http://www.theworldavatar.com/ontology/ontostation/OntoStation.owl#virtualsensor1";
		SensorSparql.addSensorValue(station_iri_string,SensorSparql.CO2,30,false);
	}
	
	public void testQueryAirStationProperties() {
		String station_iri_string = "http://www.theworldavatar.com/ontology/ontostation/OntoStation.owl#virtualsensor1";
		JSONArray result = SensorSparql.queryAirStationProperties(station_iri_string);
		result.getJSONObject(0);
	}
	
	public void testQueryAllStations() {
		SensorSparql.queryAllAirStations();
	}
	
	public void testGetNumWeatherStation() {
		SensorSparql.GetNumWeatherStation();
	}
}
