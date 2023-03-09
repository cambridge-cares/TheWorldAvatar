package uk.ac.cam.cares.jps.ship;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.Arrays;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

@WebServlet("/GetHKUPollutionData")
public class HKUPollutionRetriever extends JPSHttpServlet  {//the expected time range should be in GMT time

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@Override
	protected void doGetJPS(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		
		
		String value = request.getParameter("query");
		JSONObject input = new JSONObject(value);
		
		
		
		JSONObject resultoftaking = new JSONObject();
		resultoftaking.put("pollutiondata", requestlatestdatapol());
		response.getWriter().write(resultoftaking.toString()); 
		
	}
	
	public String requestlatestdatapol() throws JSONException, UnsupportedEncodingException {
		// it will see from the postgresql which is the latest and give the latest pollution data recorded
	
		String result=HKUWeatherRetriever.queryFromHKUServer(HKUWeatherRetriever.getTimeInXsdTimeStampFormat(System.currentTimeMillis()-3600*1000),HKUWeatherRetriever.getTimeInXsdTimeStampFormat(System.currentTimeMillis()),"/getairhistory");
		//List<String[]> listmap = MatrixConverter.fromCsvToArray(result);
		
		return result;
	}
	
	//BELOW MUST BE IN EPSG WGS:84 coordinate, remember all are in ug/m3!!!!
	public String requestlatestbackgroundpol(String stringregion) throws JSONException, UnsupportedEncodingException {
		// it will see from the postgresql which is the latest and give the latest pollution data recorded
		JSONObject region= new JSONObject(stringregion);
		String upy=""+region.getJSONObject("region").getJSONObject("uppercorner").get("uppery");
		String upx=""+region.getJSONObject("region").getJSONObject("uppercorner").get("upperx");
		String lowy=""+region.getJSONObject("region").getJSONObject("lowercorner").get("lowery");
		String lowx=""+region.getJSONObject("region").getJSONObject("lowercorner").get("lowerx");
		
		String result=HKUWeatherRetriever.queryFromHKUServer(HKUWeatherRetriever.getTimeInXsdTimeStampFormat(System.currentTimeMillis()-3600*1000),HKUWeatherRetriever.getTimeInXsdTimeStampFormat(System.currentTimeMillis()),"/getairhistory");
		List<String[]> listmap = MatrixConverter.fromCsvToArray(result);
		double sumofpm25=0;
		double sumofno2=0;
		double sumofo3=0;
		double sumofpm10=0;
		double sumofso2=0;
		
		int count=0;
		for (int x=1;x<listmap.size();x++) {
			if(Double.valueOf(listmap.get(x)[1])<Double.valueOf(lowy)||Double.valueOf(listmap.get(x)[1])>Double.valueOf(upy)||Double.valueOf(listmap.get(x)[2])<Double.valueOf(lowx)||Double.valueOf(listmap.get(x)[2])>Double.valueOf(upx)) {
				if(!Arrays.stream(listmap.get(x)).anyMatch("N/A"::equals)) {
					count++;
					//listmapfiltered.add(listmap.get(x));
					sumofpm25=sumofpm25+Double.valueOf(listmap.get(x)[4]);
					sumofno2=sumofno2+Double.valueOf(listmap.get(x)[5]);
					sumofo3=sumofo3+Double.valueOf(listmap.get(x)[6]);
					sumofpm10=sumofpm10+Double.valueOf(listmap.get(x)[7]);
					sumofso2=sumofso2+Double.valueOf(listmap.get(x)[8]);
					System.out.println("selected name= "+listmap.get(x)[0]);
				}
			}
		}
		
		double averagepm25=sumofpm25/count;
		double averageno2=sumofno2/count;
		double averagepm10=sumofpm10/count;
		double averageso2=sumofso2/count;
		double averageo3=sumofo3/count;
		
		System.out.println("average pm2.5= "+averagepm25);
		System.out.println("average no2= "+averageno2);
		System.out.println("average pm10= "+averagepm10);
		System.out.println("average so2= "+averageso2);
		System.out.println("average o3= "+averageo3);
		
		
		return result;
	}
	
	public void readWritedata() throws JSONException, UnsupportedEncodingException { //later changed to postgresql??
		System.out.println("it goes to readwrite data");	
		String dataPath = BucketHelper.getLocalDataPathWithoutThreadContext();
		
		String fullPath = dataPath + "/HKU_AirPollutionData";
		
		//System.currentTimeMillis()-3600*1000 = 1 hour before current time
		String result= HKUWeatherRetriever.queryFromHKUServer(HKUWeatherRetriever.getTimeInXsdTimeStampFormat(System.currentTimeMillis()-3600*1000),HKUWeatherRetriever.getTimeInXsdTimeStampFormat(System.currentTimeMillis()),"/getairhistory");

		QueryBroker broker = new QueryBroker();
		System.out.println("location for csv= "+fullPath);
		broker.putLocal(fullPath + "/1hrairpollutionhistory.csv", result);
		
		//then put it to data set for the metadata
	}
	

}
