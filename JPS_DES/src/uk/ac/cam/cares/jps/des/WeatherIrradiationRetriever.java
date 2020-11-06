package uk.ac.cam.cares.jps.des;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

@WebServlet(urlPatterns = {"/GetIrradiationandWeatherData" })
public class WeatherIrradiationRetriever extends JPSHttpServlet {
	private static final long serialVersionUID = 1L;
	private Logger logger = LoggerFactory.getLogger(WeatherIrradiationRetriever.class);
	@Override 
	protected void doGetJPS(HttpServletRequest request, HttpServletResponse res) {
		JSONObject jo = AgentCaller.readJsonParameter(request);

		String baseUrl = jo.optString("baseUrl",  QueryBroker.getLocalDataPath()+"/JPS_DES");
		
		JSONObject result=new JSONObject();
		try {
	    	String iritempsensor=jo.optString("temperaturesensor", "http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureSensor-001.owl#SGTemperatureSensor-001");
	    	String iriirradiationsensor=jo.optString("irradiationsensor","http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001");
	    	String irispeedsensor=jo.optString("windspeedsensor","http://www.theworldavatar.com/kb/sgp/singapore/SGWindSpeedSensor-001.owl#SGWindSpeedSensor-001");
		 System.out.println("tempsensor= "+iritempsensor);
	    	result=readWritedatatoOWL(baseUrl,iritempsensor,iriirradiationsensor,irispeedsensor);
		
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		AgentCaller.printToResponse(result, res);
 
		logger.info("return the result from weather agent");		
	}
	
	public JSONObject readWritedatatoOWL(String folder,String iritempsensor,String iriirradiationsensor,String irispeedsensor) throws Exception  { 		
		new DistributedEnergySystem().copyFromPython(folder, "runpyocr.bat");
		new DistributedEnergySystem().copyFromPython(folder,"ocrv1.py");
		String startbatCommand =folder+"/runpyocr.bat";
		System.out.println(startbatCommand);
		try {
			String resultpy= new DistributedEnergySystem().executeSingleCommand(folder,startbatCommand);
		} catch (InterruptedException e1) {
			// TODO Auto-generated catch block
			logger.error(e1.getMessage()+"python is not running interrupted");
			//later need default file data.json to substitute the loss
			new DistributedEnergySystem().copyFromPython(folder,"data.json");
		} catch (Exception ex) {
			logger.error(ex.getMessage()+"python is not running");
			new DistributedEnergySystem().copyFromPython(folder,"data.json");
		}
		logger.info("OCR finished");

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
		
		String sensorinfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> " + "SELECT ?entity ?propval ?proptimeval "
				+ "WHERE { ?entity a j5:T-Sensor ." + "  ?entity j4:observes ?prop ." + " ?prop   j2:hasValue ?vprop ."
				+ " ?vprop   j2:numericalValue ?propval ." + " ?vprop   j6:hasTime ?proptime ."
				+ " ?proptime   j6:inXSDDateTime ?proptimeval ." + "}" + "ORDER BY ASC(?proptimeval)";

		String result = new QueryBroker().queryFile(iritempsensor, sensorinfo);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultListfromquerytemp = JenaResultSetFormatter.convertToListofStringArrays(result, keys);

		String sensorinfo2 = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> " + "SELECT ?entity ?propval ?proptimeval "
				+ "WHERE { ?entity a j5:Q-Sensor ." + "  ?entity j4:observes ?prop ." + " ?prop   j2:hasValue ?vprop ."
				+ " ?vprop   j2:numericalValue ?propval ." + " ?vprop   j6:hasTime ?proptime ."
				+ " ?proptime   j6:inXSDDateTime ?proptimeval ." + "}" + "ORDER BY ASC(?proptimeval)";

		String result2 = new QueryBroker().queryFile(iriirradiationsensor, sensorinfo2);
		String[] keys2 = JenaResultSetFormatter.getKeys(result2);
		List<String[]> resultListfromqueryirr = JenaResultSetFormatter.convertToListofStringArrays(result2, keys2);
		
		String sensorinfo3 = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> " + "SELECT ?entity ?propval ?proptimeval "
				+ "WHERE { ?entity a j5:F-Sensor ." + "  ?entity j4:observes ?prop ." + " ?prop   j2:hasValue ?vprop ."
				+ " ?vprop   j2:numericalValue ?propval ." + " ?vprop   j6:hasTime ?proptime ."
				+ " ?proptime   j6:inXSDDateTime ?proptimeval ." + "}" + "ORDER BY ASC(?proptimeval)";

		String result3 = new QueryBroker().queryFile(irispeedsensor, sensorinfo3);
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
		new QueryBroker().putLocal(folder + "/WeatherActual.csv", MatrixConverter.fromArraytoCsv(actualWeather));
		
		//update the owl file
		//String baseURL2 = AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/";
		String irifortemp=converter.startConversion(readingFromCSV,"temperature","001","SG");
		System.out.println(irifortemp+" is updated");
		String iriforirradiation=converter.startConversion(readingFromCSV,"irradiation","001","SG");
		System.out.println(iriforirradiation+" is updated");
		String iriforwind=converter.startConversion(readingFromCSV,"windspeed","001","SG");
		System.out.println(iriforwind+" is updated");
		JSONObject resultweather = new JSONObject();
		//resultweather.put("folder",folder );
		resultweather.put("temperaturesensor",irifortemp );
		resultweather.put("irradiationsensor",iriforirradiation );
		resultweather.put("windspeedsensor",iriforwind );
		
		
		return resultweather;
	}
	


}
