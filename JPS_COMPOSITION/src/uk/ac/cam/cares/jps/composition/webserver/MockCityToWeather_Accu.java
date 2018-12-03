package uk.ac.cam.cares.jps.composition.webserver;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONStringer;

import uk.ac.cam.cares.jps.composition.util.SendRequest;

@WebServlet("/MockCityToWeather_Accu")
public class MockCityToWeather_Accu extends HttpServlet {
	private static final long serialVersionUID = 1L;
    public String conditionTemplateUrl = "http://dataservice.accuweather.com/currentconditions/v1/%s?apikey=Vyw7uj9BZa4jTyxflgQiguTMUPXZX7Xm&details=true";   
	public String citykeyTemplateUrl = "http://dataservice.accuweather.com/locations/v1/cities/search?apikey=Vyw7uj9BZa4jTyxflgQiguTMUPXZX7Xm&q=%s";
	

    public MockCityToWeather_Accu() {
        super();
    }

	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// 1. Get city name from cityIRI
		// 2. Get key from the city name
		// 3. Get the weather 
		// 4. Process the weather result 
		
		String cityIRI = null;
		try {
			cityIRI = new JSONObject(request.getParameter("query")).getString("city");
		} catch (JSONException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		String city = CityToWeather.queryCityNameLabel(cityIRI);
		if(city == null) {
			response.getWriter().write("");
		}
		else {
			city = city.replace(" ","_");
		try {
			String keySearchUrl = String.format(citykeyTemplateUrl, city);
			String key = getKey(SendRequest.sendGet(keySearchUrl));
			if(key == null) {
				response.getWriter().write("");
			}
			else {
				String weatherUrl = String.format(conditionTemplateUrl, key);
				String weather = SendRequest.sendGet(weatherUrl);
				response.getWriter().write(processWeatherResultForAccu(weather).toString());
			}

		
		} catch (Exception e) {
			e.printStackTrace();
		}
		}
	}

	
	public String getKey(String keyResult) throws JSONException {
		JSONArray a = new JSONArray(keyResult);
		if(a.length() == 0) {
			return null;
		}
		String key = a.getJSONObject(0).getString("Key");
		return key;
	}

	public JSONObject processWeatherResultForAccu(String resultInString) throws JSONException {
		
		JSONObject o = new JSONArray(resultInString).getJSONObject(0);
		String humidity = o.getString("RelativeHumidity");
		String temperature = o.getJSONObject("Temperature").getJSONObject("Metric").getString("Value");
		
		JSONObject wind = o.getJSONObject("Wind");
		String wind_speed = wind.getJSONObject("Speed").getJSONObject("Metric").getString("Value");
		String wind_direction = wind.getJSONObject("Direction").getString("Degrees");

		String cloudCover = o.getString("CloudCover");
		String description = o.getString("WeatherText");
		
		String precipitationIntensity = o.getJSONObject("PrecipitationSummary").getJSONObject("Precipitation")
				.getJSONObject("Metric").getString("Value");
		
		String result = null;
		try {
			result = new JSONStringer().object().
					key("weatherstate").object()
						.key("hashumidity").object() //52.508287, 13.415407
							.key("hasvalue").value(humidity).endObject()
						.key("hasexteriortemperature").object()
							.key("hasvalue").value(temperature).endObject()
						.key("haswind").object()
							.key("hasspeed").value(wind_speed)
							.key("hasdirection").value(wind_direction).endObject()	
						.key("hascloudcover").object()
							.key("hascloudcovervalue").value(cloudCover).endObject()
						.key("hasweathercondition").value(description.replace(" ","_"))			
						.key("hasprecipation").object()
							.key("hasintensity").value(precipitationIntensity).endObject()
				.endObject().endObject().toString();
		} catch (JSONException e) {
			e.printStackTrace();
		} 
		
		return new JSONObject(result);
	}
	
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doGet(request, response);
	}


	

}
