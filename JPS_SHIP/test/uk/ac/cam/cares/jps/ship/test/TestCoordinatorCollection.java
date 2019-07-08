package uk.ac.cam.cares.jps.ship.test;
import java.io.UnsupportedEncodingException;
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
		String result = HKUWeatherRetriever.queryFromHKUServer(
				HKUWeatherRetriever.getTimeInXsdTimeStampFormat(System.currentTimeMillis() - 600 * 1000),
				HKUWeatherRetriever.getTimeInXsdTimeStampFormat(System.currentTimeMillis()),"/getweatherhistory");
		
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
		
		System.out.println(CRSTransformer.transform("EPSG:4326", "EPSG:3857", new double[] {xmin, ymin})[0]);
		System.out.println(CRSTransformer.transform("EPSG:4326", "EPSG:3857", new double[] {xmin, ymin})[1]);
		System.out.println(CRSTransformer.transform("EPSG:4326", "EPSG:3857", new double[] {xmax, ymax})[0]);
		System.out.println(CRSTransformer.transform("EPSG:4326", "EPSG:3857", new double[] {xmax, ymax})[1]);
		
		
		HKUPollutionRetriever a= new HKUPollutionRetriever();
		String resultwithcall= a.requestlatestbackgroundpol(jsonInput.toString());
		System.out.println("bkgresult= "+resultwithcall);
		
	}
	 
}
