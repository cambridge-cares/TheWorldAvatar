package uk.ac.cam.cares.jps.ship;

import java.io.IOException;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

@WebServlet("/GetHKUWeatherData")
public class HKUWeatherRetriever extends JPSHttpServlet {
	private static final long serialVersionUID = 1L;
	private Logger logger = LoggerFactory.getLogger(HKUWeatherRetriever.class);
     
	protected void doGetJPS(HttpServletRequest req, HttpServletResponse res) throws IOException {
		
		String value = req.getParameter("query");
		JSONObject input = new JSONObject(value);
		
		
		
//		JSONObject resultoftaking = new JSONObject();
//		resultoftaking.put("weatherdata", requestlatestdata());
		
		res.getWriter().write(requestlatestdata()); 
		System.out.println("return the result from weather agent");		
	}
	
	public void readWritedata() { //later changed to postgresql??
		System.out.println("it goes to readwrite data");	
		String dataPath = BucketHelper.getLocalDataPathWithoutThreadContext();
		
		String fullPath = dataPath + "/HKU_WeatherData";
		String result= "result for requesting the data";
		
		QueryBroker broker = new QueryBroker();
		System.out.println("location for csv= "+fullPath);
		broker.put(fullPath + "/arbitrary.csv", result);
		
		//then put it to data set for the metadata
	}
	
	
	public String requestlatestdata() {
		// it will see from the postgresql which is the latest and give the latest weather data recorded
		//right now just try to call the current weather data first
	System.out.println("go to weather agent");
		JSONObject input = new JSONObject();
		input.put("city", "http://dbpedia.org/resource/Hong_Kong");
		String result = AgentCaller.executeGetWithJsonParameter("/JPS_COMPOSITION/CityToWeather",input.toString());
		
	
		return result;
	}
}
