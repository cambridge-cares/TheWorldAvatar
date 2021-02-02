package uk.ac.cam.cares.jps.des;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.ConnectException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.text.DecimalFormat;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
@WebServlet(urlPatterns = {"/GetForecastData" })
public class ForecastAgent extends JPSAgent{
	private static final long serialVersionUID = 1L;
	private static String SolCastURL= "https://api.solcast.com.au/weather_sites/0ff4-0cb4-c270-5389/forecasts?format=json&api_key=IxJaiBo4-jICEIZSFPuRYVvJ2OqiFBqN";
	private static String AccuWeatherURL = "http://dataservice.accuweather.com/forecasts/v1/hourly/12hour/300565?apikey=%20%09NP6DUl1mQkBlOAn7CE5j3MGPAAR9xbpg&details=true&metric=true";
	@Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
		validateInput(requestParams);
		try {
			forecastNextDay();
		
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
    	return requestParams;
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
    	validateInput(requestParams);
    	try {
			forecastNextDay();
		
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
    	return requestParams;
    }
	/** No validation of input here as requestParams is not needed. 
	 * 
	 */
    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (requestParams.isEmpty()) {
            throw new BadRequestException();
        }
        return true;
    }
	/** use this method to 'GET' the results. 
	 * 	the current AgentCaller.getRequestBody doesn't work because
	 * 	it doesn't check for if HTTPUrlConnection is ok
	 * @param url String of API
	 * @return
	 * @throws IOException
	 */
	public static String GETReq(String url) throws IOException{
	    URL urlForGetRequest = new URL(url);
	    HttpURLConnection conection = (HttpURLConnection) urlForGetRequest.openConnection();
	    conection.setRequestMethod("GET");
	    String readLine = null;
	    int responseCode = conection.getResponseCode();
	    if (responseCode == HttpURLConnection.HTTP_OK) {
	    	 BufferedReader in = new BufferedReader(new InputStreamReader(conection.getInputStream()));
	    	 StringBuffer response = new StringBuffer();
	    	 while ((readLine = in.readLine()) != null) {
	    		 response.append(readLine);
	    	 	}
	    	 in.close();

		 return response.toString();
	    }else {
	    	throw new ConnectException("Request to "+ url + "failed; try again later. ");
	    	
	    }
	}
	
	/** Using Java to read API call from AccuWeather. Taken from 
	 * https://dzone.com/articles/how-to-implement-get-and-post-request-through-simp
	 * change this api if we ever manage to get a free 24 hourly prediction of temperature and windspeed
	 * @param args
	 * @throws Exception
	 */
	public static ArrayList<ArrayList<String>> AccuRequest() throws IOException {
		JSONArray arr = new JSONArray(GETReq(AccuWeatherURL));
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
		JSONObject jo =  new JSONObject(GETReq(SolCastURL));
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
	 static public String addOneDay(LocalDate date) {
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
	public void forecastNextDay() throws Exception{
		ArrayList<ArrayList<String>> accuArray, solArray ; 
		try {
			accuArray = AccuRequest();
			solArray = SolCastRequest(); 
		
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
