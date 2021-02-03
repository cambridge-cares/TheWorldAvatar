package uk.ac.cam.cares.jps.des;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.query.Query;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.des.n.DESAgentNew;

@WebServlet(urlPatterns = {"/GetIrradiationandWeatherData" })
public class WeatherIrradiationRetriever extends JPSAgent{
	private static final long serialVersionUID = 1L;
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
	    requestParams = processRequestParameters(requestParams, null);
	    return requestParams;
	}
	@Override 
	public JSONObject processRequestParameters(JSONObject requestParams,HttpServletRequest request) {

		validateInput(requestParams);
		String baseUrl = requestParams.optString("baseUrl", QueryBroker.getLocalDataPath()+"/JPS_DES"); //create unique uuid
        
		try {
	    	String iritempsensor=requestParams.getString("temperaturesensor");
	    	String iriirradiationsensor=requestParams.getString("irradiationsensor");
	    	String irispeedsensor=requestParams.getString("windspeedsensor");
	    	readWritedatatoOWL(baseUrl,iritempsensor,iriirradiationsensor,irispeedsensor);

			return requestParams;
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return requestParams;
 		
	}
	@Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (requestParams.isEmpty()) {
            throw new BadRequestException();
        }
        try {
	        String iriofwindF = requestParams.getString("windspeedsensor");
	        boolean w = InputValidator.checkIfValidIRI(iriofwindF);
	        
	        String irioftempF=requestParams.getString("temperaturesensor");
	
	        boolean e = InputValidator.checkIfValidIRI(irioftempF);
	        String iriofirrF=requestParams.getString("irradiationsensor");
	        boolean r = InputValidator.checkIfValidIRI(iriofirrF);
	        // Till now, there is no system independent to check if a file path is valid or not. 
	        
	        return w&e&r;
        } catch (JSONException ex) {
        	ex.printStackTrace();
        	throw new JSONException("Sensor not present in getString");
        }

    }
	public static void readWritedatatoOWL(String folder,String iritempsensor,String iriirradiationsensor,String irispeedsensor) throws Exception  { 		
		//TODO: I can't figure out how to get this to run without having to copy over a file to create
		//the location within this folder. If I leave the line below commented
		// the folder isn't created, and I get an error. 
		new DESAgentNew().copyFromPython(folder, "ocrv1.py");
		String res =  new DESAgentNew().runPythonScript("ocrv1.py",folder);

		DateTimeFormatter dtf = DateTimeFormatter.ofPattern("yyyy/MM/dd HH:mm:ss");
		LocalDateTime now = LocalDateTime.now();
		String com=dtf.format(now);
		String date=com.split("/")[2].split(" ")[0];
		   
		String jsonres=new QueryBroker().readFileLocal(folder+"/data.json");
		JSONObject current= new JSONObject(jsonres);
		String year=current.optString("year",com.split("/")[0]);
		String datemonth=current.optString("date",date)+"-"+current.optString("month",com.split("/")[1]);
		String time=current.optString("time",com.split("/")[2].split(" ")[1]);
		String speed=current.optString("windspeed","0");
		String temperature=current.optString("temperature","26");
		String irradiance=current.optString("irradiance","0");
		
		WeatherTimeStampKB converter = new WeatherTimeStampKB();		
		//query the data from the existing owl file
		
		 
    	WhereBuilder whereB = new WhereBuilder().addPrefix("j2", "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#")
    			.addPrefix("j4", "http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#")
    			.addPrefix("j5","http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#")
    			.addPrefix("j6", "http://www.w3.org/2006/time#").addWhere("?entity", "j4:observes", "?prop")
    			.addWhere("?prop", "j2:hasValue", "?vprop").addWhere("?vprop", "j2:numericalValue", "?propval")
    			.addWhere("?vprop", "j6:hasTime", "?proptime").addWhere("?proptime", "j6:inXSDDateTime", "?proptimeval");
   
    	
    	SelectBuilder sensorTemp = new SelectBuilder()
    			.addPrefix("j5","http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#")
    			.addVar("?entity").addVar("?propval")
    			.addVar("?proptimeval").addWhere("?entity","a", "j5:T-Sensor").addWhere(whereB).addOrderBy("?proptimeval");
    	Query q= sensorTemp.build(); 
    	String sensorInfo = q.toString();
    	SelectBuilder sensorIrrad = new SelectBuilder()
    			.addPrefix("j5","http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#")
    			.addVar("?entity").addVar("?propval")
    			.addVar("?proptimeval").addWhere("?entity","a", "j5:Q-Sensor").addWhere(whereB).addOrderBy("?proptimeval");
    	
    	q= sensorIrrad.build(); 
    	String sensorInfo2 = q.toString();
    	SelectBuilder sensorWind = new SelectBuilder()
    			.addPrefix("j5","http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#")
    			.addVar("?entity").addVar("?propval")
    			.addVar("?proptimeval").addWhere("?entity","a", "j5:F-Sensor").addWhere(whereB).addOrderBy("?proptimeval");
    	
    	q= sensorWind.build(); 
    	String sensorInfo3 = q.toString();
		
		String result = new QueryBroker().queryFile(iritempsensor, sensorInfo);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultListfromquerytemp = JenaResultSetFormatter.convertToListofStringArrays(result, keys);

		String result2 = new QueryBroker().queryFile(iriirradiationsensor, sensorInfo2);
		String[] keys2 = JenaResultSetFormatter.getKeys(result2);
		List<String[]> resultListfromqueryirr = JenaResultSetFormatter.convertToListofStringArrays(result2, keys2);
		
		String result3 = new QueryBroker().queryFile(irispeedsensor, sensorInfo3);
		String[] keys3 = JenaResultSetFormatter.getKeys(result3);
		List<String[]> resultListfromqueryspeed = JenaResultSetFormatter.convertToListofStringArrays(result3, keys3);
		
		
		
		List<String[]> readingFromCSV = new ArrayList<String[]>();
		for (int d=0;d<resultListfromqueryirr.size();d++) {
			String timewholecsv=resultListfromquerytemp.get(d)[2];
			String datemonthcsv=timewholecsv.split("-")[2].split("T")[0]+"-"+timewholecsv.split("-")[1];			
			String timecsv=timewholecsv.split("-")[2].split("T")[1].split("\\+")[0];
			String[]e= {timewholecsv.split("-")[0],datemonthcsv,timecsv,"100",resultListfromquerytemp.get(d)[1],"74.9",resultListfromqueryspeed.get(d)[1],"115.7",resultListfromqueryirr.get(d)[1],"0"};
			readingFromCSV.add(e);
		}
		
		
		
		//update the array value list
		readingFromCSV.remove(0); //if with header,later need to be changed TODO KEVIN
		String[]newline= {year,datemonth,time,"100",temperature,"74.9",speed,"115.7",irradiance,"0"};
		System.out.println("datemonth="+datemonth);
		readingFromCSV.add(newline);
		List<String[]> actualWeather = new ArrayList<String[]>();
		actualWeather.add(newline);
		
		//update the owl file
		//String baseURL2 = AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/";
		String irifortemp=converter.startConversion(readingFromCSV,"temperature","001","SG");
		System.out.println(irifortemp+" is updated");
		String iriforirradiation=converter.startConversion(readingFromCSV,"irradiation","001","SG");
		System.out.println(iriforirradiation+" is updated");
		String iriforwind=converter.startConversion(readingFromCSV,"windspeed","001","SG");
		System.out.println(iriforwind+" is updated");
		
	}
	


}
