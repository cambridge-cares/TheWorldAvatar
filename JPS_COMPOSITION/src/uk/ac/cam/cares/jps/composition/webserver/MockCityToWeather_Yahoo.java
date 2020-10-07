package uk.ac.cam.cares.jps.composition.webserver;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONStringer;

import uk.ac.cam.cares.jps.composition.util.SendRequest;


@WebServlet("/MockCityToWeather_Yahoo")
public class MockCityToWeather_Yahoo extends HttpServlet {
	private static final long serialVersionUID = 1L;

    
    public MockCityToWeather_Yahoo() {
        super();
    }

   
    
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		
		// As usual make a query from the IRI of the city to get its label ... 
		String cityIRI = null;
		try {
			cityIRI = new JSONObject(request.getParameter("query")).getString("city");
		} catch (JSONException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		String city = CityToWeather.queryCityNameLabel(cityIRI);
		if(city == null)
		{
			response.getWriter().write("");
		}
		else
		{
			city = city.replace(" ", "_");
		
		try {
			String url = "https://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20weather.forecast%20where%20woeid%20in%20"
					+ "(select%20woeid%20from%20geo.places(1)%20where%20text%3D%22" + city +"%2C%20ak%22)%20and%20u='c'"
							+ "&format=json&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys";
			String result = SendRequest.sendGet(url);
			response.getWriter().write(processWeatherResultForYahoo(result).toString());
		} catch (Exception e) {
			e.printStackTrace();
		}
		}
	}
	
	public JSONObject processWeatherResultForYahoo(String resultInString) throws JSONException {
		
		JSONObject resultInJson = new JSONObject(resultInString);
		JSONObject condition = resultInJson.getJSONObject("query").getJSONObject("results").getJSONObject("channel");
		JSONObject atmosphere = null;
		if(condition.has("atmosphere")) {
			 atmosphere = condition.getJSONObject("atmosphere");
		}
		else {
			 atmosphere = null;
		}
		String humidity = atmosphere.getString("humidity");
		String temperature = condition.getJSONObject("item").getJSONObject("condition").getString("temp");
		JSONObject wind = condition.getJSONObject("wind");
		String wind_speed = wind.getString("speed");
		String wind_direction = wind.getString("direction");
		String cloudCover = null;
		String description = condition.getJSONObject("item").getJSONObject("condition").getString("text");
		String precipitationIntensity = null;
		
		
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
