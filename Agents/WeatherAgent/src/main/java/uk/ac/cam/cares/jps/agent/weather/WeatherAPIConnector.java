package uk.ac.cam.cares.jps.agent.weather;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.Map;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

class WeatherAPIConnector {
	 static final Logger LOGGER = LogManager.getLogger(WeatherAPIConnector.class);

	/**
	 * obtain current weather data given the set of coordinates (EPSG:4326)
	 * @param lat
	 * @param lon
	 * @return
	 */
	static Map<String,Double> getCurrentWeatherDataFromOpenWeather(double lat, double lon) {
		URIBuilder builder = new URIBuilder().setScheme("http").setHost("api.openweathermap.org")
                .setPath("/data/2.5/weather");
		builder.setParameter("lat", String.valueOf(lat));
		builder.setParameter("lon", String.valueOf(lon));
		builder.setParameter("units", "metric");
		builder.setParameter("appid", Config.apikey);
		
		try (CloseableHttpClient httpclient = HttpClients.createDefault()) {
            HttpGet request = new HttpGet(builder.build());
            CloseableHttpResponse response = httpclient.execute(request);
            JSONObject apiresult = new JSONObject(EntityUtils.toString(response.getEntity()));
            
            // units are set according to mcwind file for Episode
            double precipitation = 0.0; // in mm/h
			if (apiresult.has("rain")) {
				JSONObject rain = apiresult.getJSONObject("rain");
				if (rain.has("1h")) {
					precipitation = rain.getDouble("1h");
				} else {
					precipitation = rain.getDouble("3h")/3.0;
				}
			}

			// collect results into a Map			
			Map<String,Double> resultMap = new HashMap<>();
			resultMap.put(WeatherQueryClient.Rainfall, precipitation);
			resultMap.put(WeatherQueryClient.AtmosphericPressure, apiresult.getJSONObject("main").getDouble("pressure"));
			resultMap.put(WeatherQueryClient.CloudCover,apiresult.getJSONObject("clouds").getDouble("all"));
			resultMap.put(WeatherQueryClient.WindSpeed, apiresult.getJSONObject("wind").getDouble("speed"));
			if (apiresult.getJSONObject("wind").has("deg")) {
				resultMap.put(WeatherQueryClient.WindDirection, apiresult.getJSONObject("wind").getDouble("deg"));
			}
			resultMap.put(WeatherQueryClient.RelativeHumidity, apiresult.getJSONObject("main").getDouble("humidity"));
			resultMap.put(WeatherQueryClient.AirTemperature, apiresult.getJSONObject("main").getDouble("temp"));
			
            return resultMap;
        } catch (Exception e) {
        	LOGGER.error(e.getMessage());
            throw new JPSRuntimeException(e.getMessage(), e);
        }
	}

	/**
	 * obtain current weather data given the set of coordinates (EPSG:4326) and timestamp
	 * @param lat
	 * @param lon
	 * @param timestamp
	 * @return
	 */
	static Map<String,Double> getWeatherDataFromOpenWeatherWithTimestamp(double lat, double lon, String timestamp) {
		URIBuilder builder = new URIBuilder().setScheme("https").setHost("api.openweathermap.org")
		.setPath("data/3.0/onecall/timemachine");
		builder.setParameter("lat", String.valueOf(lat));
		builder.setParameter("lon", String.valueOf(lon));
		builder.setParameter("units", "metric");
		builder.setParameter("appid", Config.apikey);
		builder.setParameter("dt", timestamp);

		try (CloseableHttpClient httpclient = HttpClients.createDefault()) {
            HttpGet request = new HttpGet(builder.build());
            CloseableHttpResponse response = httpclient.execute(request);
            JSONObject apiresult = new JSONObject(EntityUtils.toString(response.getEntity()));

			JSONObject data = apiresult.getJSONArray("data").getJSONObject(0);
			// units are set according to mcwind file for Episode
            double precipitation = 0.0; // in mm/h
			if (data.has("rain")) {
				JSONObject rain = data.getJSONObject("rain");
				if (rain.has("1h")) {
					precipitation = rain.getDouble("1h");
				} else {
					precipitation = rain.getDouble("3h")/3.0;
				}
			}

			// collect results into a Map			
			Map<String,Double> resultMap = new HashMap<>();
			resultMap.put(WeatherQueryClient.Rainfall, precipitation);
			resultMap.put(WeatherQueryClient.AtmosphericPressure, data.getDouble("pressure"));
			resultMap.put(WeatherQueryClient.CloudCover,data.getDouble("clouds"));
			resultMap.put(WeatherQueryClient.WindSpeed, data.getDouble("wind_speed"));
			resultMap.put(WeatherQueryClient.WindDirection, data.getDouble("wind_deg"));
			resultMap.put(WeatherQueryClient.RelativeHumidity, data.getDouble("humidity"));
			resultMap.put(WeatherQueryClient.AirTemperature, data.getDouble("temp"));

			return resultMap;
		} catch (IOException e) {
			LOGGER.error(e.getMessage());
			LOGGER.fatal("Error in closing HTTP client");
			throw new JPSRuntimeException("Error in closing HTTP client", e);
		} catch (URISyntaxException e) {
			LOGGER.error(e.getMessage());
			LOGGER.fatal("Error in building GET request to openweather");
			throw new JPSRuntimeException("Error in building GET request to openweather", e);
		}
	}
}
