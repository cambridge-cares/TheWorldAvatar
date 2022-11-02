package uk.ac.cam.cares.jps.ship;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Properties;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.BadRequestException;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.util.CommandHelper;

@WebServlet("/SLMAgent")
public class SpeedLoadMapWrapper extends JPSAgent {
	private static final String slmDir = "\\python\\ADMS-speed-load-map";
	private static final String slmScript = "ADMS-Map-SpeedTorque-NOxSoot.py";
	private static final String jpsShipProperties = "\\WEB-INF\\classes\\resources\\jpsship.properties";
	private static final String pypathUnix = "bin/python";
	private static final String pypathWindows = "\\Scripts\\python.exe";
	private static final String slmPyvenv = "speed.load.map.venv.dir";
	
	private String getSurogateValues(String inputs) {
		//@todo [AC] - detect if, python virtual environment exists in the slmDir and create it first, if necessary
		String slmWorkingDir =  AgentLocator.getCurrentJpsAppDirectory(this) + slmDir;
		String jpsShipPropertiesDir = AgentLocator.getCurrentJpsAppDirectory(this) + jpsShipProperties;
		ArrayList<String> args = new ArrayList<String>();
		Path venvPath;
		InputStream input = null;
		Properties jpsProperties = new Properties();
		try {
			if (CommandHelper.isWindows()) {
				input = new FileInputStream(jpsShipPropertiesDir);
			} else {
				input = new FileInputStream(jpsShipPropertiesDir.replace("\\", "/"));
			}
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
		try {
			jpsProperties.load(input);
		} catch (IOException e) {
			e.printStackTrace();
		}
		if (CommandHelper.isWindows()) {
			venvPath = Paths.get(jpsProperties.getProperty(slmPyvenv), pypathWindows);
		} else {
			slmWorkingDir = AgentLocator.getCurrentJpsAppDirectory(this) +  slmDir.replace("\\", "/");
			venvPath = Paths.get(jpsProperties.getProperty(slmPyvenv), pypathUnix);
		}
		args.add(venvPath.toString());
		args.add(slmScript);
		args.add(inputs);
		return CommandHelper.executeCommands(slmWorkingDir, args);
	}
	
	/*protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		
		JSONObject jo = AgentCaller.readJsonParameter(request);
		JSONObject in= new JSONObject();

	/*	double valuecalc=jo.getDouble("speed")*2500/58.1;
		if(valuecalc>2500) {
			valuecalc=2500;
		}
		String type=jo.getString("type").toLowerCase();
		JSONObject speedob= new JSONObject();		
		speedob.put("value", valuecalc); //600-2500
		speedob.put("unit", "RPM");
		JSONObject torob= new JSONObject();
		torob.put("value", 250); //50-550 range
		torob.put("unit", "Nm");
		in.put("speed", speedob);
		in.put("torque", torob);

		JSONObject json = crankUpRealShipModel(type, getSurogateValues(in.toString().replace("\"", "'")));
		
		AgentCaller.writeJsonParameter(response, json);
		
	}
*/
	/*
	 * http://betterboat.com/average-boat-speed/ assume fastest medium boat
	 * max speed= 25knot max rpm= 2500 rpm torque=constant=250Nm then 1knot=100 rpm rpm=
	 * https://www.marineinsight.com/shipping-news/worlds-fastest-ship-built-tasmania-christened-argentinas-president/->fastest=58.1 knot
	 * knot*2500/58.1 roughly 1 ship 33 kg/h 1 boat= 1.1338650741577147e-05*3600 = 0.041
	 * kg/h NO2 (comparison of NO2
	 * https://pdfs.semanticscholar.org/1bd2/52f2ae1ede131d0ef84ee21c84a73fb6b374.pdf)
	 * 1 boat mass flux=0.0192143028723584 kg/s

	 */
	@Override
	public JSONObject processRequestParameters(JSONObject requestParam){
		if(validateInput(requestParam)) {
			JSONObject in = new JSONObject();
			double valuecalc=requestParam.getDouble("speed")*2500/58.1;
			if(valuecalc>2500) {
				valuecalc=2500;
			}
			String type=requestParam.getString("type").toLowerCase();
			JSONObject speedob= new JSONObject();
			speedob.put("value", valuecalc); //600-2500
			speedob.put("unit", "RPM");
			JSONObject torob= new JSONObject();
			torob.put("value", 250); //50-550 range
			torob.put("unit", "Nm");
			in.put("speed", speedob);
			in.put("torque", torob);

			JSONObject json = crankUpRealShipModel(type, getSurogateValues(in.toString().replace("\"", "'")));
			requestParam=json;
		}
		return requestParam;
	}

	@Override
	public boolean validateInput(JSONObject requestParams) throws BadRequestException {
		boolean validate=true;
		if (requestParams.isEmpty()) {
			throw new BadRequestException("RequestParam is empty.");
		}else if(!checkSpeed(requestParams)){
			throw new BadRequestException("In the requestParam object either the key:speed is not present or it is null or it is empty.");
		}else if(!checkType(requestParams)){
			throw new BadRequestException("In the requestParam object either the key:type is not present or it is null or it is empty.");
		}
		return validate;
	}

	private boolean checkSpeed(JSONObject requestParams){
		//check three cases: 1) Key is not present 2) Key entry is Null 3) Key entry is empty.
		boolean validate= true;
		if( !requestParams.has("speed") || requestParams.isNull("speed") ){
			validate=false;
		}
		if (validate) {
			String speed=requestParams.get("speed").toString();
			if(speed.isEmpty())
				validate=false;
		}
		return validate;
	}

	private boolean checkType(JSONObject requestParams){
		//check three cases: 1) Key is not present 2) Key entry is Null 3) Key entry is empty.
		boolean validate= true;
		if( !requestParams.has("type") || requestParams.isNull("type") ){
			validate=false;
		}
		if (validate) {
			String type=requestParams.getString("type").toLowerCase();
			if(type.isEmpty())
				validate=false;
		}
		return validate;
	}

	private JSONObject crankUpRealShipModel(String type, String newjsonfile) {
		JSONObject json = new JSONObject(newjsonfile);
		// these scaling factors are purely to make the results fall within the reasonable range
		for(int gas=0;gas<json.getJSONArray("pollutants").length();gas++) {
			JSONObject pollutantmass=json.getJSONArray("pollutants").getJSONObject(gas);
			Double oldvaluemixmass= pollutantmass.getDouble("value");
			pollutantmass= considerShipTypeForMass(type,oldvaluemixmass,pollutantmass);
		}
		for(int part=0;part<json.getJSONArray("particle").length();part++) {
			JSONObject particlemass=json.getJSONArray("particle").getJSONObject(part).getJSONObject("emission_rate");
			Double oldvaluemixmass= particlemass.getDouble("value");
			JSONObject particleD=json.getJSONArray("particle").getJSONObject(part).getJSONObject("diameter");
			Double oldvaluemixD= particleD.getDouble("value");
			particleD.put("value",(double)Math.round(oldvaluemixD * 1000d) / 1000d);
			particlemass=considerShipTypeForMass(type,oldvaluemixmass,particlemass);
		}
		JSONObject mixturemass=json.getJSONObject("mixture").getJSONObject("massflux");
		Double oldvaluemixmass= mixturemass.getDouble("value");
		mixturemass= considerShipTypeForMass(type,oldvaluemixmass,mixturemass);
		return json;
	}

	private JSONObject considerShipTypeForMass(String type, double oldvaluemixmass, JSONObject mass){
		if(type.contains("cargo")) {
			mass.put("value",oldvaluemixmass*322);
		}
		else if(type.contains("tanker")) {
			mass.put("value",oldvaluemixmass*430);
		}
		else if(type.contains("container")) {
			mass.put("value",oldvaluemixmass*580);
		}
		else if(type.contains("passenger")) {
			mass.put("value",oldvaluemixmass*697);
		}
		else  {
			mass.put("value",oldvaluemixmass*300);
		}
		return mass;
	}
}
