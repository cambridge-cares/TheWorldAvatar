package uk.ac.cam.cares.jps.des;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.ConnectException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.TimeZone;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.apache.commons.lang.RandomStringUtils;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.query.Query;
import org.apache.jena.sparql.core.Var;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.base.util.MiscUtil;
import uk.ac.cam.cares.jps.des.n.DESAgentNew;
@WebServlet(urlPatterns = {"/GetForecastData" })
public class ForecastAgent extends JPSAgent{
	private static final long serialVersionUID = 1L;
	private static String SolCastURL= "https://api.solcast.com.au/weather_sites/0ff4-0cb4-c270-5389/forecasts?format=json&api_key=IxJaiBo4-jICEIZSFPuRYVvJ2OqiFBqN";
	private static String AccuWeatherURL = "http://dataservice.accuweather.com/forecasts/v1/hourly/12hour/300565?apikey=%20%09NP6DUl1mQkBlOAn7CE5j3MGPAAR9xbpg&details=true&metric=true";
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
	    requestParams = processRequestParameters(requestParams, null);
	    if (!validateInput(requestParams)) {
    		throw new BadRequestException("DESAgent:  Input parameters not found.\n");
    	}
    	String iriofnetwork = requestParams.getString("electricalnetwork");
        String iriofdistrict = requestParams.getString("district");
        String irioftempF=requestParams.getString("temperatureforecast");
        String iriofirrF=requestParams.getString("irradiationforecast");
	    return requestParams;
	}

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
    	if (!validateInput(requestParams)) {
    		throw new BadRequestException("ForecastAgent:  Input parameters not found.\n");
    	}
    	try {
			forecastNextDay();
		
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
    	return requestParams;
    }
	
	
	/** Using Java to read API call from AccuWeather. Taken from 
	 * https://dzone.com/articles/how-to-implement-get-and-post-request-through-simp
	 * change this api if we ever manage to get a free 24 hourly prediction of temperature and windspeed
	 * @param args
	 * @throws Exception
	 */
	public static ArrayList<ArrayList<String>> AccuRequest() throws IOException {
		JSONArray arr = new JSONArray(AgentCaller.getRequestBody(AccuWeatherURL));
	    DecimalFormat doubSF = new DecimalFormat("##.#");
	    ArrayList<ArrayList<String>> arraarray = new ArrayList<ArrayList<String>>();
        for (int i = 0; i< arr.length(); i++) {
        	JSONObject object = arr.getJSONObject(i);
        	JSONObject temp= (JSONObject) object.get("Temperature");
        	double temper = temp.getFloat("Value");
        	String tempera = doubSF.format(temper);
        	
        	JSONObject wind = (JSONObject) object.get("Wind");
        	JSONObject winds = (JSONObject) wind.get("Speed");
        	double windsp = winds.getFloat("Value");
        	String windspe = doubSF.format(windsp/3.6);//because windspeed is in km/h and not m/s
        	ArrayList<String> lstInit = new ArrayList<>();
        	lstInit.add(tempera);
        	lstInit.add(windspe);
        	arraarray.add(lstInit);
        }        
        return arraarray;
	}
	/** feed in solcast api, and since it's in 30 minute invervals, the step iteration
	 * for the for loop is two
	 * @return
	 * @throws IOException
	 */
	public static ArrayList<ArrayList<String>> SolCastRequest() throws IOException{
		JSONObject jo =  new JSONObject(AgentCaller.getRequestBody(SolCastURL));
		JSONArray arr = jo.getJSONArray("forecasts");
	    DecimalFormat doubSF = new DecimalFormat("##.#");
		ArrayList<ArrayList<String>> arraarray =  new ArrayList<ArrayList<String>>();
        for (int i = 0; i< 48; i+= 2) {
        	JSONObject object = arr.getJSONObject(i);
        	int temp= object.getInt("air_temp");
        	String tempera = doubSF.format(temp);
        	int ghi = object.getInt("ghi");
        	String irrad = doubSF.format(ghi);//because windspeed is in km/h and not m/s
        	ArrayList<String> lstInit = new ArrayList<>();
        	lstInit.add(tempera);
        	lstInit.add(irrad);
        	arraarray.add(lstInit);
        }   
        return arraarray;     
	}
	/** Increments the date. 
	 * 
	 * @param date
	 * @return
	 */
	 public static String addOneDay(LocalDate date) {
		    return date.plusDays(1).format(DateTimeFormatter.ofPattern("yyyy-MM-dd"));
		  }
	 /** creates a list of times to be saved to OWL file in OWL format
	  * 
	  * @return
	  * @throws Exception
	  */
	public static ArrayList<String[]> createTimer() {
		Date date = new Date();
		Calendar calendar = GregorianCalendar.getInstance();
		calendar.setTime(date);
		ArrayList<String[]> sj1 = new ArrayList<String[]>();
		LocalDate today = LocalDate.now();
		int n = LocalTime.now().getHour();
		String formattedDate = today.format(DateTimeFormatter.ofPattern("yyyy-MM-dd"));
		for (int i = 0; i < 24; i++){
			if (n == 24){
		        n = 0;
		        formattedDate = addOneDay(today);
		      }
		      String[] a = {String.format("%02d", Integer.valueOf(n)) + ":00:00", formattedDate};
		      sj1.add(a);
		      n++; 
		      

		    }
		return sj1;
	}
	/** Calls API and writes to OWL file the temperature, irradiation and windspeed. 
	 * 
	 * @throws Exception
	 */
	public void forecastNextDay()throws Exception{
		ArrayList<ArrayList<String>> accuArray, solArray ; 
		try {
			accuArray = AccuRequest(); //{temp, windspeed}
			solArray = SolCastRequest(); //{temp, irrad}
		
			ArrayList<String[]> readingFromCSV = new ArrayList<String[]>(); 
			//mash the two together
			ArrayList<String[]> datetime = createTimer();
			for (int i = 0; i<accuArray.size(); i++) {
				ArrayList<String> ji =  solArray.get(i);
				ji.set(0, accuArray.get(i).get(0));//set temperature
				ji.add(accuArray.get(i).get(1));//set windspeed
				ji.add(datetime.get(i)[0]);
				ji.add(datetime.get(i)[1]);
				String[] stringArray = ji.toArray(new String[0]);
				readingFromCSV.add(stringArray);
			}
			for (int i = 12; i < solArray.size(); i++) { //no windspeed, place in zero.
				ArrayList<String> ji =  solArray.get(i); 
				ji.add("0.0");
				ji.add(datetime.get(i)[0]);
				ji.add(datetime.get(i)[1]);
				String[] stringArray = ji.toArray(new String[0]);
				readingFromCSV.add(stringArray);
			}
			WeatherTimeStampKB converter = new WeatherTimeStampKB();
			converter.startConversionForecast(readingFromCSV,"temperature", 0);
			converter.startConversionForecast(readingFromCSV,"irradiation", 1);
			converter.startConversionForecast(readingFromCSV,"windspeed", 2);
		}catch (IOException e) {
			//if either accuArray and solArray doesn't work, copy sample file
			String path= AgentLocator.getCurrentJpsAppDirectory(this) + "\\resources\\WeatherForecast";

			List<String[]> readingFromCSV = readResult(path);
			Date date = new Date();   // given date
			Calendar calendar = GregorianCalendar.getInstance(); // creates a new calendar instance
			calendar.setTime(date);   // assigns calendar to given date 
			int h = calendar.get(Calendar.HOUR_OF_DAY); // gets hour in 24h format
			Collections.rotate(readingFromCSV,(24-h)); //rotate by number of hours
			WeatherTimeStampKB converter = new WeatherTimeStampKB();
			converter.startConversionForecast(readingFromCSV,"temperature", 0);
			converter.startConversionForecast(readingFromCSV,"irradiation", 1);
				        
			e.printStackTrace();
		}
	
	}
	
	/** reads data from Solcast.com.au (need API key) and stores in ArrayList<String[]>
	 * Length of 24 hours, as we don't want a thirty minute update. 
	 * @return ArrayList[temperature, irradiation, timeInXSD]
	 * @throws IOException
	 */
	public static ArrayList<String[]> callSolarAPI() throws IOException{
		JSONObject jo =  new JSONObject(AgentCaller.getRequestBody(SolCastURL));
		JSONArray arr = jo.getJSONArray("forecasts");
	    DecimalFormat doubSF = new DecimalFormat("##.#");
	    ArrayList<String[]> ans = new ArrayList<String[]>();	    
		for (int i = 0; i< 48; i+= 2) {
        	JSONObject object = arr.getJSONObject(i);
        	int temp= object.getInt("air_temp");
        	String tempera = doubSF.format(temp);
        	int ghi = object.getInt("ghi");
        	String irrad = doubSF.format(ghi);//because windspeed is in km/h and not m/s
        	String periodEnd = object.getString("period_end").split(".")[0]+"Z";
        	String timeInXSD = MiscUtil.convertToTimeZoneXSD(periodEnd);
        	String[] lstInit = {tempera, irrad, timeInXSD};
        	ans.add(lstInit);
        }   
        return ans;     
	}
	/** reads data from AccuWeather for Singapore (need API key) and stores in ArrayList<String[]>
	 * Length of 12 hours; 24 hours is paid plan. 
	 * 
	 * @return ArrayList[Temperature, wind speed value, dateTime]
	 * @throws IOException
	 */
	public ArrayList<String[]> callAccuAPI() {

		JSONArray arr = new JSONArray(AgentCaller.getRequestBody(AccuWeatherURL));
		DecimalFormat doubSF = new DecimalFormat("##.#");
		ArrayList<String[]> accuArray = new ArrayList<String[]>();
		for (int i = 0; i< arr.length(); i++) {
			JSONObject object = arr.getJSONObject(i);
			JSONObject temp= object.getJSONObject("Temperature");
			double temper = temp.getFloat("Value");
			String temperature = doubSF.format(temper);
			
			String dateInXSD= object.getString("DateTime");
			String[] args = {temperature, dateInXSD};
			accuArray.add(args);
		}        
		return accuArray;
	}
	public void nextForecastDay(String iriTemperature) {
		//First download AccuAPI and push data from temperature for first 12 hours
		ArrayList<String[]> accuArray = callAccuAPI();
		WhereBuilder whereB = new WhereBuilder().addPrefix("j2", "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#")
    			.addPrefix("j4", "http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#")
    			.addPrefix("j5","http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#")
    			.addPrefix("j6", "http://www.w3.org/2006/time#").addWhere("?entity", "j4:observes", "?prop")
    			.addWhere("?prop", "j2:hasValue", "?vprop")
    			.addWhere("?vprop", "j6:hasTime", "?proptime")
    			.addWhere("?proptime", "j6:inXSDDateTime", "?proptimeval");
   
		SelectBuilder sensorTemp = new SelectBuilder()
    			.addPrefix("j5","http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#")
    			.addVar("?vprop").addVar("?proptime")
    			.addWhere("?entity","a", "j5:T-Sensor").addWhere(whereB).addOrderBy("?proptimeval").setLimit(12);
    	Query q= sensorTemp.build(); 
    	String sensorInfo = q.toString();
    	String convertedIRI = DESAgentNew.tempIRItoFile(iriTemperature);
    	
    	JSONObject requestParams = new JSONObject().put(JPSConstants.QUERY_SPARQL_QUERY, sensorInfo)
				.put(JPSConstants.TARGETIRI, convertedIRI );
		String resultf = AgentCaller.executeGetWithJsonParameter("jps/kb", requestParams.toString());
		String[] keysf = {"vprop","proptime"};
		List<String[]>  resultListfromquery = JenaResultSetFormatter.convertToListofStringArraysWithKeys(resultf, keysf);
    	
		updateOWLFileWithResultList(resultListfromquery,accuArray, convertedIRI); 
		
		
	}
	/** SubMethod for readWriteToOWL for each type of sensor
	 * @param sensorIRI
	 * @param sparqlQuery
	 */
	private static void updateOWLFileWithResultList(List<String[]>  resultListfromquery,
			ArrayList<String[]> accuArray,  String sensorIRI) {
		
		UpdateBuilder builder = new UpdateBuilder();
		JSONObject requestParams = new JSONObject();
		int sizeOfUpdate = resultListfromquery.size();
		String p = "<http://www.w3.org/2006/time#inXSDDateTime>";
		String d = "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>";
		for (int i = 0; i < sizeOfUpdate; i ++ ) {//We stopped at element 46
			Var v = Var.alloc(RandomStringUtils.random(5, true, false));
			Var m = Var.alloc(RandomStringUtils.random(4, true, false)); //random string generate to prevent collusion
			builder.addDelete("<"+resultListfromquery.get(i)[0]+">" ,d, v)
					.addInsert("<"+resultListfromquery.get(i)[0]+">" ,d,  accuArray.get(i)[0])
					.addWhere("<"+resultListfromquery.get(i)[0]+">" ,d,v)					
					.addDelete("<"+resultListfromquery.get(i)[1]+">" ,p, m)
					.addInsert("<"+resultListfromquery.get(i)[1]+">" ,p, accuArray.get(i)[1])
					.addWhere("<"+resultListfromquery.get(i)[1]+">" ,p,m);
			if (i %6 == 0) {
				requestParams = new JSONObject().put(JPSConstants.QUERY_SPARQL_UPDATE, builder.build().toString())
						.put(JPSConstants.TARGETIRI ,sensorIRI);
				AgentCaller.executeGetWithJsonParameter("jps/kb", requestParams.toString());
				builder = new UpdateBuilder();
				
			}
		}
		
		//finally
		requestParams = new JSONObject().put(JPSConstants.QUERY_SPARQL_UPDATE, builder.build().toString())
				.put(JPSConstants.TARGETIRI ,sensorIRI);
		AgentCaller.executeGetWithJsonParameter("jps/kb", requestParams.toString());
	}
	
	/** reads the result from the csv file produced and returns as List<String[]>
	 * 
	 * @param baseUrl String
	 * @param filename name of the file. 
	 * @return
	 * @throws IOException
	 */
	public List<String[]> readResult(String fil) throws IOException {

        String csv = new QueryBroker().readFileLocal(fil);
        List<String[]> simulationResult = MatrixConverter.fromCsvToArray(csv);
		
		return simulationResult;
	}
	
	
}
