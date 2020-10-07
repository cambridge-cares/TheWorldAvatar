package uk.ac.cam.cares.jps.ship;

import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

@WebServlet(urlPatterns = {"/GetHKUWeatherData","/GetHKUWeatherLatestData"})
public class HKUWeatherRetriever extends JPSHttpServlet { //the time result still behind 16 hr??? 
	private static final long serialVersionUID = 1L;
	private Logger logger = LoggerFactory.getLogger(HKUWeatherRetriever.class);
	public static final String LATEST_PATH = "/GetHKUWeatherLatestData";
	public static final String OLDER_PATH = "/GetHKUWeatherData";
	
	@Override
	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		JSONObject responseParams = requestParams;
		 String path = request.getServletPath();
		 if(LATEST_PATH.equals(path)) {
			 try {
				String result= queryFromHKUServer(getTimeInXsdTimeStampFormat(System.currentTimeMillis()-600*1000),getTimeInXsdTimeStampFormat(System.currentTimeMillis()),"/getweatherhistory");
				logger.info("time now= "+getTimeInXsdTimeStampFormat(System.currentTimeMillis()));
				List<String[]> readingFromCSV = MatrixConverter.fromCsvToArray(result);
				int size=readingFromCSV.size();
				JSONArray weatherreading= new JSONArray();
				for(int x=1;x<size;x++) { //header is not count
					//4,5,6,7,8
					
					if(!Arrays.asList(readingFromCSV.get(x)).contains("N/A")) {
						JSONObject weather= new JSONObject();
						weather.put("stnname",readingFromCSV.get(x)[0]);
						weather.put("x",readingFromCSV.get(x)[1]);
						weather.put("y",readingFromCSV.get(x)[2]);
						weather.put("z",readingFromCSV.get(x)[3]);
						weather.put("OutsideAirTemperature",readingFromCSV.get(x)[4]);
						weather.put("OutsideAirPressure",readingFromCSV.get(x)[5]);
						weather.put("OutsideAirRelativeHumidity",readingFromCSV.get(x)[6]);
						weather.put("OutsideWindSpeed",readingFromCSV.get(x)[7]);
						weather.put("OutsideWindDirection",readingFromCSV.get(x)[8]);
						weather.put("timestamp",readingFromCSV.get(x)[9]);
						weatherreading.put(weather);
					}
				}
				responseParams.put("HKweather", weatherreading);
			 } catch (JSONException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (UnsupportedEncodingException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
 
		 }else if(OLDER_PATH.equals(path)) {
			 String result;
			try {
				result = requestlatestdata();
				responseParams=new JSONObject(result);
				System.out.println("return the result from weather agent");	
			} catch (JSONException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (UnsupportedEncodingException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
			 
		 }
		return responseParams;
	}
     
//	protected void doGetJPS(HttpServletRequest req, HttpServletResponse res) throws IOException {
//		
//		String value = req.getParameter("query");
//		JSONObject input = new JSONObject(value);
//		
//		res.getWriter().write(requestlatestdata()); 
//		System.out.println("return the result from weather agent");		
//	}
	
	public void readWritedata() throws JSONException, UnsupportedEncodingException { //later changed to postgresql??
		System.out.println("it goes to readwrite data");	
		String dataPath = BucketHelper.getLocalDataPathWithoutThreadContext();
		
		String fullPath = dataPath + "/HKU_WeatherData";
		
		//System.currentTimeMillis()-3600*1000 = 1 hour before current time
		String result= queryFromHKUServer(getTimeInXsdTimeStampFormat(System.currentTimeMillis()-3600*1000),getTimeInXsdTimeStampFormat(System.currentTimeMillis()),"/getweatherhistory");

		QueryBroker broker = new QueryBroker();
		System.out.println("location for csv= "+fullPath);
		broker.putLocal(fullPath + "/1hrweatherhistory.csv", result);
		
		//then put it to data set for the metadata
	}
	
	
	public String requestlatestdata() throws JSONException, UnsupportedEncodingException {
		// it will see from the postgresql which is the latest and give the latest weather data recorded
		//right now just try to call the current weather data first
	System.out.println("go to weather agent");
		JSONObject input = new JSONObject();
		input.put("city", "http://dbpedia.org/resource/Hong_Kong");
		String result = AgentCaller.executeGetWithJsonParameter("/JPS_COMPOSITION/CityToWeather",input.toString());
		
		/*
		 * String result=queryFromHKUServer(getTimeInXsdTimeStampFormat(System.
		 * currentTimeMillis()-600*1000),getTimeInXsdTimeStampFormat(System.
		 * currentTimeMillis()),"/getweatherhistory"); List<String[]> listmap =
		 * MatrixConverter.fromCsvToArray(result); double sumoftemp=0; double
		 * sumofspeed=0; int count=0; for (int x=1;x<listmap.size();x++) {
		 * if(!listmap.get(x)[4].contentEquals("N/A")&&!listmap.get(x)[7].contentEquals(
		 * "N/A")&&!listmap.get(x)[8].contentEquals("N/A")) { count++;
		 * //listmapfiltered.add(listmap.get(x));
		 * sumoftemp=sumoftemp+Double.valueOf(listmap.get(x)[4]);
		 * sumofspeed=sumofspeed+Double.valueOf(listmap.get(x)[7]); } } double
		 * averagetemp=sumoftemp/count; double averagespeed=sumofspeed/count*1000/3600;
		 * //convert from km/h to m/s System.out.println("average temp= "+averagetemp);
		 * System.out.println("average speed= "+averagespeed);
		 * 
		 * JSONObject weatherstate = new
		 * JSONObject(result).getJSONObject("weatherstate"); String humchosen=""; String
		 * tempchosen=""; String speedchosen=""; String dirchosen="";
		 * weatherstate.getJSONObject("hashumidity").put("hasvalue",humchosen);
		 * weatherstate.getJSONObject("hasexteriortemperature").put("hasvalue",
		 * averagetemp);
		 * weatherstate.getJSONObject("haswind").put("hasspeed",averagespeed);
		 * weatherstate.getJSONObject("haswind").put("hasdirection",dirchosen);
		 * result=weatherstate.toString();
		 */

		
		return result;
	}
	
	public static String getTimeInXsdTimeStampFormat(long millis) { 
		Date date = new Date(millis);
	    SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss"); 
		format.setTimeZone(TimeZone.getTimeZone("GMT+0:00"));
	    String timestamp = format.format(date);
	    return timestamp.replace(" ", "T");
	}
	
	public static String executeGet(URIBuilder builder) {
		try {
			URI uri = builder.build();
			HttpGet request = new HttpGet(uri);
			request.setHeader(HttpHeaders.ACCEPT, "application/json");
			HttpResponse httpResponse = HttpClientBuilder.create().build().execute(request);
			if (httpResponse.getStatusLine().getStatusCode() != 200) {
				throw new JPSRuntimeException("HTTP response with error = " + httpResponse.getStatusLine());
			}
			return EntityUtils.toString(httpResponse.getEntity());
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		} 
	}
	
	public static String queryFromHKUServer(String from, String to,String path) throws JSONException, UnsupportedEncodingException {

		String myHost = "147.8.183.135" ;
		int myPort = 10086;
		String myPath = path;
				
		URIBuilder builder;
		builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath(myPath)
				.setParameter("from", from/*"2019-06-16T20:00:00"*/)
				.setParameter("to", to/*"2019-06-16T21:00:00"*/);
		System.out.println("builder= "+builder);
		String result = executeGet(builder);
		return result;
		
	}
}
