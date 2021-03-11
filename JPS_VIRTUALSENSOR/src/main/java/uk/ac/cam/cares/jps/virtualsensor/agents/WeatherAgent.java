package uk.ac.cam.cares.jps.virtualsensor.agents;

import java.time.Instant;
import java.util.Arrays;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.region.Scope;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.virtualsensor.episode.CalculationUtils;
import uk.ac.cam.cares.jps.virtualsensor.objects.WeatherStation;
import uk.ac.cam.cares.jps.virtualsensor.sparql.SensorSparql;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This weather agent uses the openweather API and query through coordinates
 * @author Kok Foong Lee
 *
 */
@WebServlet(urlPatterns = {"/WeatherAgent"})
public class WeatherAgent extends JPSAgent {
	Logger logger = LoggerFactory.getLogger(WeatherAgent.class);
	
	@Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        JSONObject response = new JSONObject();
    	if (validateInput(requestParams)) {
	        Scope sc = new Scope(requestParams);
	        double[] centre = sc.getScopeCentre();
	        
	        JSONArray queryResult = SensorSparql.queryWeatherStationsWithinScope(sc);
	        
	        // initialise weather stations
	        WeatherStation[] weatherStations = new WeatherStation[queryResult.length()];
	        for (int i = 0; i < queryResult.length(); i++) {
	        	weatherStations[i] = new WeatherStation();
	        	weatherStations[i].setStationiri(queryResult.getJSONObject(i).getString("stationiri"));
	            weatherStations[i].setXcoord(queryResult.getJSONObject(i).getDouble("xvalue"));
	            weatherStations[i].setYcoord(queryResult.getJSONObject(i).getDouble("yvalue"));
	            double distance = CalculationUtils.distanceWGS84(weatherStations[i].getYcoord(), weatherStations[i].getYcoord(), 
	            		centre[1], centre[0], "K");
	            weatherStations[i].setDistance(distance);
	        }
	        
	        //sort weather stations according to distance to centre, sorted in ascending order
	        Arrays.sort(weatherStations);
	        
	        // update stations we are going to use
	        updateWeatherStationWithAPI(weatherStations[0]);
	        updateWeatherStationWithAPI(weatherStations[queryResult.length()-1]);
	        
	        JSONArray station = new JSONArray();
	        station.put(weatherStations[0].getStationiri());
	        station.put(weatherStations[queryResult.length()-1].getStationiri());
			response.put("stationiri", station);
    	}
        return response;
    }

	@Override
    public boolean validateInput(JSONObject requestParams) {
    	boolean valid = false;
    	try {
    		new Scope(requestParams);
    		valid = true;
    	} catch (Exception e) {
    		throw new BadRequestException();
    	}
    	return valid;
    }
	
	private JSONObject getWeatherDataFromAPI(double [] xy) {
		URIBuilder builder = new URIBuilder().setScheme("http").setHost("api.openweathermap.org")
                .setPath("/data/2.5/weather");
		builder.setParameter("lat", String.valueOf(xy[1]));
		builder.setParameter("lon", String.valueOf(xy[0]));
		builder.setParameter("units", "metric");
		builder.setParameter("appid", "329f65c3f7166977f6751cff95bfcb0a");
		
		try {
            HttpGet request = new HttpGet(builder.build());
            String apiresult = AgentCaller.executeGet(request);
            JSONObject jo = new JSONObject(apiresult);
            return jo;
        } catch (Exception e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }
	}
	
	public void updateWeatherStationWithAPI(WeatherStation ws) {
		long lastupdate = SensorSparql.queryWeatherStationTimeStamp(ws.getStationiri());
		long currenttime = Instant.now().getEpochSecond();
		if ((currenttime-lastupdate) > 3600) {
			try {
				double [] xy = {ws.getXcoord(),ws.getYcoord()};
				JSONObject apiresult = getWeatherDataFromAPI(xy);
				double precipitation = 0.0; // in mm/h
				if (apiresult.has("rain")) {
					JSONObject rain = apiresult.getJSONObject("rain");
					try {
						precipitation = rain.getDouble("1h");
					} catch (Exception e) {
						precipitation = rain.getDouble("3h")/3.0;
					}
				}
				// units are set according to mcwind file for Episode
				double pressure = apiresult.getJSONObject("main").getDouble("pressure"); // in hPa
				double cloudcover = apiresult.getJSONObject("clouds").getDouble("all")/100; // fraction
				double windspeed = apiresult.getJSONObject("wind").getDouble("speed"); // in m/s
				double winddirection = 0.0;
				if (apiresult.getJSONObject("wind").has("deg")) {
					winddirection = apiresult.getJSONObject("wind").getDouble("deg");
				}
				double humidity = apiresult.getJSONObject("main").getDouble("humidity"); // percentage
				double temperature = apiresult.getJSONObject("main").getDouble("temp");
				
				ws.setCloudcover(cloudcover);
				ws.setHumidity(humidity);
				ws.setPrecipitation(precipitation);
				ws.setPressure(pressure);
				ws.setTemperature(temperature);
				ws.setWinddirection(winddirection);
				ws.setWindspeed(windspeed);
				ws.setTimestamp(currenttime);
				
				// update weather station in the triple-store
				SensorSparql.updateWeatherStation(ws);
			} catch (JPSRuntimeException e) {
				logger.info(e.getMessage());
				logger.info("Weather API update failed, old data will be used");
			}
		}
	}
}
