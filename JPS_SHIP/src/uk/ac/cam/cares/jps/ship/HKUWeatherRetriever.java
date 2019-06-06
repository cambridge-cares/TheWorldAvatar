package uk.ac.cam.cares.jps.ship;

import java.io.IOException;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.query.QueryBroker;

@WebServlet("/GetHKUWeatherData")
public class HKUWeatherRetriever extends HttpServlet {
	private static final long serialVersionUID = 1L;
	private Logger logger = LoggerFactory.getLogger(HKUWeatherRetriever.class);
     
	protected void doGet(HttpServletRequest req, HttpServletResponse res) throws IOException {
		
		String value = req.getParameter("query");
		JSONObject input = new JSONObject(value);
		
		
		
		JSONObject resultoftaking = new JSONObject();
		resultoftaking.put("weatherdata", requestlatestdata());
		res.getWriter().write(resultoftaking.toString()); 
	}
	
	public static void readWritedata() {
		//continously read data and put
		String dataPath = QueryBroker.getLocalDataPath();
		String fullPath = dataPath + "/HKU_WeatherData";
		String result= "result for requesting the data";
		QueryBroker broker = new QueryBroker();
		broker.put(fullPath + "/arbitrary.csv", result);
		
		//then put it to data set for the metadata
	}
	
	
	public String requestlatestdata() {
		// it will see from the directory which is the latest and give the latest weather data recorded
	
		String result="0";
	
		return result;
	}
}
