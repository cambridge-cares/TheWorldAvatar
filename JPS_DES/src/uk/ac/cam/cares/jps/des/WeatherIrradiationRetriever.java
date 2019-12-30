package uk.ac.cam.cares.jps.des;

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
	protected void doGetJPS(HttpServletRequest req, HttpServletResponse res) {
		
//		String value = req.getParameter("query");
//		JSONObject input = new JSONObject(value);
		System.out.println("HELLO");
		JSONObject joforDES = AgentCaller.readJsonParameter(req);
		String folder;
		if (joforDES.has("folder")){
				folder = joforDES.getString("folder");
		}else {
			String dataPath = QueryBroker.getLocalDataPath();
			String baseUrl = dataPath + "/JPS_DES";
			folder = baseUrl;
		}
//		String iriirradiationsensor="http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001";
//		String iritempsensor="http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureSensor-001.owl#SGTemperatureSensor-001";
//		String irispeedsensor="http://www.theworldavatar.com/kb/sgp/singapore/SGWindSpeedSensor-001.owl#SGWindSpeedSensor-001";
		String iriirradiationsensor=joforDES.getString("irradiationsensor");
		String iritempsensor=joforDES.getString("tempsensor");
		String irispeedsensor=joforDES.getString("speedsensor");
		System.out.println("jpscontet needed?0");
		try {
			System.out.println("Check here then?");
			String jo = readWritedatatoOWL(folder,iritempsensor,iriirradiationsensor,irispeedsensor);
			AgentCaller.printToResponse(jo, res);
			
			
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
//		JSONObject resultoftaking = new JSONObject();
//		resultoftaking.put("weatherdata", requestlatestdata());
		
 
		logger.info("return the result from weather agent");		
	}
	
	public String readWritedatatoOWL(String folder,String iritempsensor,String iriirradiationsensor,String irispeedsensor) throws Exception { 		
		new DistributedEnergySystem().copyFromPython(folder, "runpyocr.bat");
		new DistributedEnergySystem().copyFromPython(folder,"ocrv1.py");
		
		String startbatCommand =folder+"/runpyocr.bat";
		System.out.println(startbatCommand);
		String resultpy= new DistributedEnergySystem().executeSingleCommand(folder,startbatCommand);
		logger.info("OCR finished");
		
//		String jsonres=new QueryBroker().readFileLocal(AgentLocator.getCurrentJpsAppDirectory(this) +"/python/data.json");
		String jsonres=new QueryBroker().readFileLocal(folder+"/data.json");
		JSONObject current= new JSONObject(jsonres);
		String year=current.getString("year");
		String datemonth=current.getString("date")+"-"+current.getString("month");
		String time=current.getString("time");
		String speed=current.getString("windspeed");
		String temperature=current.getString("temperature");
		String irradiance=current.getString("irradiance");
		
		WeatherTimeStampKB converter = new WeatherTimeStampKB();
		
		//modification of array
//		String csv = new QueryBroker().readFileLocal(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/Weather.csv");
//		List<String[]> readingFromCSV = MatrixConverter.fromCsvToArray(csv);
		// later replaced by query from the owl file of the sensor
		
		
		
		String sensorinfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> " + "SELECT ?entity ?propval ?proptimeval "
				+ "WHERE { ?entity a j5:T-Sensor ." + "  ?entity j4:observes ?prop ." + " ?prop   j2:hasValue ?vprop ."
				+ " ?vprop   j2:numericalValue ?propval ." + " ?vprop   j6:hasTime ?proptime ."
				+ " ?proptime   j6:inXSDDateTimeStamp ?proptimeval ." + "}" + "ORDER BY ASC(?proptimeval)";

		String result = new QueryBroker().queryFile(iritempsensor, sensorinfo);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultListfromquerytemp = JenaResultSetFormatter.convertToListofStringArrays(result, keys);

		String sensorinfo2 = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> " + "SELECT ?entity ?propval ?proptimeval "
				+ "WHERE { ?entity a j5:Q-Sensor ." + "  ?entity j4:observes ?prop ." + " ?prop   j2:hasValue ?vprop ."
				+ " ?vprop   j2:numericalValue ?propval ." + " ?vprop   j6:hasTime ?proptime ."
				+ " ?proptime   j6:inXSDDateTimeStamp ?proptimeval ." + "}" + "ORDER BY ASC(?proptimeval)";

		String result2 = new QueryBroker().queryFile(iriirradiationsensor, sensorinfo2);
		String[] keys2 = JenaResultSetFormatter.getKeys(result2);
		List<String[]> resultListfromqueryirr = JenaResultSetFormatter.convertToListofStringArrays(result2, keys2);
		
		String sensorinfo3 = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> " + "SELECT ?entity ?propval ?proptimeval "
				+ "WHERE { ?entity a j5:F-Sensor ." + "  ?entity j4:observes ?prop ." + " ?prop   j2:hasValue ?vprop ."
				+ " ?vprop   j2:numericalValue ?propval ." + " ?vprop   j6:hasTime ?proptime ."
				+ " ?proptime   j6:inXSDDateTimeStamp ?proptimeval ." + "}" + "ORDER BY ASC(?proptimeval)";

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
		
		
		
		
		readingFromCSV.remove(0); //if with header,later need to be changed TODO KEVIN
		String[]newline= {year,datemonth,time,"100",temperature,"74.9",speed,"115.7",irradiance,"0"};
		readingFromCSV.add(newline);
		 new QueryBroker().putLocal(folder + "/Weather.csv", MatrixConverter.fromArraytoCsv(readingFromCSV));
		
		
		//String baseURL2 = AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/";
		String irifortemp=converter.startConversion(readingFromCSV,"temperature");
		System.out.println(irifortemp+" is updated");
		String iriforirradiation=converter.startConversion(readingFromCSV,"irradiation");
		System.out.println(iriforirradiation+" is updated");
		String iriforwind=converter.startConversion(readingFromCSV,"windpseed");
		System.out.println(iriforwind+" is updated");
		JSONObject jo = new JSONObject();
		//knowing that it would be the last updated value from the owl file, we'll see this being updated every hour once this is run
		jo.put("temperature", temperature);
		jo.put("irrad",irradiance);
		return jo.toString();
	}
	


}
