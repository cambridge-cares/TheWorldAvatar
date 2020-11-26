package uk.ac.cam.cares.jps.ship;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Scanner;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.util.CommandHelper;

@WebServlet("/SLMAgent")
public class SpeedLoadMapWrapper extends HttpServlet {
	private static final String slmDir = "\\python\\ADMS-speed-load-map";
	private static final String slmPython = "\\env\\Scripts\\python.exe";
	private static final String slmScript = "ADMS-Map-SpeedTorque-NOxSoot.py";
	
	private String getSurogateValues(String inputs) {
		//@todo [AC] - detect if, python virtual environment exists in the slmDir and create it first, if necessary
		String smlWorkingDir =  AgentLocator.getCurrentJpsAppDirectory(this) + slmDir;
		String pythonExec = smlWorkingDir + slmPython;
		ArrayList<String> args = new ArrayList<String>();

		if (CommandHelper.isWindows()) {
			args.add(pythonExec);
	        args.add(slmScript);
			args.add(inputs);
		} else {
			smlWorkingDir = AgentLocator.getCurrentJpsAppDirectory(this) +  slmDir.replace("\\", "/");
			args.add(KeyValueMap.getInstance().get(IKeys.SPEED_LOAD_MAP_VENV_DIR));
			args.add(slmScript);
			args.add(inputs);
		}

		return CommandHelper.executeCommands(smlWorkingDir, args);
	}
	
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		
		JSONObject jo = AgentCaller.readJsonParameter(request);
		JSONObject in= new JSONObject();
		double valuecalc=jo.getDouble("speed")*2500/58.1;
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

	private JSONObject crankUpRealShipModel(String type, String newjsonfile) {
		JSONObject json = new JSONObject(newjsonfile);
		
		
		for(int gas=0;gas<json.getJSONArray("pollutants").length();gas++) {
			JSONObject pollutantmass=json.getJSONArray("pollutants").getJSONObject(gas);
			Double oldvaluemixmass= pollutantmass.getDouble("value");
			if(type.contains("cargo")) {
				pollutantmass.put("value",oldvaluemixmass*322);	
			}
			else if(type.contains("tanker")) {
				pollutantmass.put("value",oldvaluemixmass*430);	
				
			}
			else if(type.contains("container")) {
				pollutantmass.put("value",oldvaluemixmass*580);	
			}
			else if(type.contains("passenger")) {
				pollutantmass.put("value",oldvaluemixmass*697);	
			}
			else  {
				pollutantmass.put("value",oldvaluemixmass*300);	
			}
		}
		
		
		
		for(int part=0;part<json.getJSONArray("particle").length();part++) {
			JSONObject particlemass=json.getJSONArray("particle").getJSONObject(part).getJSONObject("emission_rate");
			Double oldvaluemixmass= particlemass.getDouble("value");
			JSONObject particleD=json.getJSONArray("particle").getJSONObject(part).getJSONObject("diameter");
			Double oldvaluemixD= particleD.getDouble("value");
			particleD.put("value",(double)Math.round(oldvaluemixD * 1000d) / 1000d);
			if(type.contains("cargo")) {
				
				particlemass.put("value",oldvaluemixmass*322);
			}
			else if(type.contains("tanker")) {
				particlemass.put("value",oldvaluemixmass*430);	
				
			}
			else if(type.contains("container")) {
				particlemass.put("value",oldvaluemixmass*580);	
			}
			else if(type.contains("passenger")) {
				particlemass.put("value",oldvaluemixmass*697);	
			}
			else  {
				particlemass.put("value",oldvaluemixmass*300);	
			}
		}
		
		
		
		JSONObject mixturemass=json.getJSONObject("mixture").getJSONObject("massflux");
		Double oldvaluemixmass= mixturemass.getDouble("value");
		if(type.contains("cargo")) {
			mixturemass.put("value",oldvaluemixmass*322);	
		}
		else if(type.contains("tanker")) {
			mixturemass.put("value",oldvaluemixmass*430);	
			
		}
		else if(type.contains("container")) {
			mixturemass.put("value",oldvaluemixmass*580);	
		}
		else if(type.contains("passenger")) {
			mixturemass.put("value",oldvaluemixmass*697);	
		}
		else  {
			mixturemass.put("value",oldvaluemixmass*300);	
		}
		return json;
	}
	
	
}
