package uk.ac.cam.cares.jps.ship.test;
import java.io.UnsupportedEncodingException;
import java.util.Arrays;
import java.util.List;

import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONStringer;
import org.json.JSONWriter;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.util.CRSTransformer;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.ship.HKUPollutionRetriever;
import uk.ac.cam.cares.jps.ship.HKUWeatherRetriever;
public class TestCoordinatorCollection extends TestCase{


	
	public void testcallingAgent() {
		JSONObject jo = new JSONObject();
		
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_SHIP/CollectorCoordination",jo.toString());
		System.out.println("it is executed");
		System.out.println("result= "+resultStart);
		
	}
	
	public void testcallhkuweatheragent() {
		JSONObject jo = new JSONObject();
		String result = AgentCaller.executeGetWithJsonParameter("JPS_SHIP/GetHKUWeatherData",jo.toString());
		System.out.println("result= "+result);
	}
	
	public void testgetcurrentweather() throws JSONException, UnsupportedEncodingException {
		System.out.println("from= "+HKUWeatherRetriever.getTimeInXsdTimeStampFormat(System.currentTimeMillis() - 600 * 1000));
//		String result = HKUWeatherRetriever.queryFromHKUServer(
//				HKUWeatherRetriever.getTimeInXsdTimeStampFormat(System.currentTimeMillis() - 600 * 1000),
//				HKUWeatherRetriever.getTimeInXsdTimeStampFormat(System.currentTimeMillis()),"/getweatherhistory");
		String result = HKUWeatherRetriever.queryFromHKUServer(
				"2020-06-08T11:00:10",
				"2020-06-08T12:00:10","/getweatherhistory");
		
		System.out.println(result);
		
		List<String[]> listmap = MatrixConverter.fromCsvToArray(result);
		System.out.println(listmap.get(3)[0]);
		
		double sumoftemp=0;
		double sumofspeed=0;
		int count=0;
		for (int x=1;x<listmap.size();x++) {
			if(!listmap.get(x)[4].contentEquals("N/A")&&!listmap.get(x)[7].contentEquals("N/A")&&!listmap.get(x)[8].contentEquals("N/A")) {
				count++;
				//listmapfiltered.add(listmap.get(x));
				sumoftemp=sumoftemp+Double.valueOf(listmap.get(x)[4]);
				sumofspeed=sumofspeed+Double.valueOf(listmap.get(x)[7]);
			}
		}
		double averagetemp=sumoftemp/count;
		double averagespeed=sumofspeed/count*1000/3600; //convert from km/h to m/s
		System.out.println("average temp= "+averagetemp);
		System.out.println("average speed= "+averagespeed);
			
	}
	
	public void testgetCurrentBkgConcentration() throws JSONException, UnsupportedEncodingException { //the time is wrong formt
		   double xmin = 821182.43;
		   double xmax = 846182.43;
		   double ymin = 806448.76;
		   double ymax = 831448.76;
		   double[] pmin = CRSTransformer.transform("EPSG:2326", "EPSG:4326", new double[] {xmin, ymin});
		   double[] pmax = CRSTransformer.transform("EPSG:2326", "EPSG:4326", new double[] {xmax, ymax});
		   xmin = pmin[0]; ymin = pmin[1]; xmax = pmax[0]; ymax = pmax[1];
		   
		JSONWriter jsonInput = new JSONStringer().object().
				key("region").object()
				.key("lowercorner").object() //52.508287, 13.415407
					.key("lowerx").value(xmin) // 699182 / 13.415407 // 13728088
					.key("lowery").value(ymin).endObject() // 532537 / 52.508287 // 2281341
				.key("uppercorner").object() //52.511112, 13.424336
					.key("upperx").value(xmax) // 699983 / 13.424336 // 13736486
					.key("uppery").value(ymax).endObject() // 533338 / 52.511112 // 2286829
				.key("srsname").value("EPSG:4326") // EPSG:4326
			.endObject()
				.key("agent").value("http://www.theworldavatar.com/kb/agents/Service__ComposedADMS.owl#Service")
				.key("reactionmechanism").value("http://www.theworldavatar.com/kb/ontokin/Reduced_PRF_ERC_particle.owl#ReactionMechanism_184144363244001")
				.key("city").value("http://dbpedia.org/resource/Singapore")
				.endObject(); 
		
		System.out.println("xmin= "+CRSTransformer.transform("EPSG:4326", "EPSG:3857", new double[] {xmin, ymin})[0]);
		System.out.println("ymin= "+CRSTransformer.transform("EPSG:4326", "EPSG:3857", new double[] {xmin, ymin})[1]);
		System.out.println("xmax= "+CRSTransformer.transform("EPSG:4326", "EPSG:3857", new double[] {xmax, ymax})[0]);
		System.out.println("ymax= "+CRSTransformer.transform("EPSG:4326", "EPSG:3857", new double[] {xmax, ymax})[1]);
		
		
		HKUPollutionRetriever a= new HKUPollutionRetriever();
		String resultwithcall= a.requestlatestbackgroundpol(jsonInput.toString());
		System.out.println("bkgresult= "+resultwithcall);
		
	}
	
	public void testgetspecifiedbkgconc() throws JSONException, UnsupportedEncodingException {
		   double xmin = 821182.43;
		   double xmax = 846182.43;
		   double ymin = 806448.76;
		   double ymax = 831448.76;
		   double[] pmin = CRSTransformer.transform("EPSG:2326", "EPSG:4326", new double[] {xmin, ymin});
		   double[] pmax = CRSTransformer.transform("EPSG:2326", "EPSG:4326", new double[] {xmax, ymax});
		   xmin = pmin[0]; ymin = pmin[1]; xmax = pmax[0]; ymax = pmax[1];
		   
		String result=HKUWeatherRetriever.queryFromHKUServer("2019-07-09T02:00:00", "2019-07-09T03:00:00", "/getairhistory");
		System.out.println(result );
		List<String[]> listmap = MatrixConverter.fromCsvToArray(result);
		double sumofpm25=0;
		double sumofno2=0;
		double sumofo3=0;
		double sumofpm10=0;
		double sumofso2=0;
		
		double sum2ofpm25=0;
		double sum2ofno2=0;
		double sum2ofo3=0;
		double sum2ofpm10=0;
		double sum2ofso2=0;
		
		int count=0;
		for (int x=1;x<listmap.size();x++) {
			if(Double.valueOf(listmap.get(x)[1])<Double.valueOf(ymin)||Double.valueOf(listmap.get(x)[1])>Double.valueOf(ymax)||Double.valueOf(listmap.get(x)[2])<Double.valueOf(xmin)||Double.valueOf(listmap.get(x)[2])>Double.valueOf(xmax)) {
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
		
		int count2=0;
		for (int x=1;x<listmap.size();x++) {
			if(listmap.get(x)[0].contains("centralwestern")||listmap.get(x)[0].contains("central")||listmap.get(x)[0].contains("causewaybay")||listmap.get(x)[0].contains("eastern")||listmap.get(x)[0].contains("kuntong")||listmap.get(x)[0].contains("mongkok")||listmap.get(x)[0].contains("shanshuipo")) {
				if(!Arrays.stream(listmap.get(x)).anyMatch("N/A"::equals)) {
					count2++;
					//listmapfiltered.add(listmap.get(x));
					sum2ofpm25=sum2ofpm25+Double.valueOf(listmap.get(x)[4]);
					sum2ofno2=sum2ofno2+Double.valueOf(listmap.get(x)[5]);
					sum2ofo3=sum2ofo3+Double.valueOf(listmap.get(x)[6]);
					sum2ofpm10=sum2ofpm10+Double.valueOf(listmap.get(x)[7]);
					sum2ofso2=sum2ofso2+Double.valueOf(listmap.get(x)[8]);
					//System.out.println("selected name= "+listmap.get(x)[0]);
				}
			}
		}
		
		double averagepm25=sumofpm25/count;
		double averageno2=sumofno2/count;
		double averagepm10=sumofpm10/count;
		double averageso2=sumofso2/count;
		double averageo3=sumofo3/count;
		
		double average2pm25=sum2ofpm25/count2;
		double average2no2=sum2ofno2/count2;
		double average2pm10=sum2ofpm10/count2;
		double average2so2=sum2ofso2/count2;
		double average2o3=sum2ofo3/count2;
		
		System.out.println("average pm2.5= "+averagepm25);
		System.out.println("average no2= "+averageno2);
		System.out.println("average pm10= "+averagepm10);
		System.out.println("average so2= "+averageso2);
		System.out.println("average o3= "+averageo3);
		
		System.out.println("averageinscope pm2.5= "+average2pm25);
		System.out.println("averageinscope no2= "+average2no2);
		System.out.println("averageinscope pm10= "+average2pm10);
		System.out.println("averageinscope so2= "+average2so2);
		System.out.println("averageinscope o3= "+average2o3);
	}
	 
	public void testGetLatestHKWeatherData() {
		JSONObject jo = new JSONObject();
		String result = AgentCaller.executeGetWithJsonParameter("JPS_SHIP/GetHKUWeatherLatestData",jo.toString());
		System.out.println("result= "+result);
	}
}
