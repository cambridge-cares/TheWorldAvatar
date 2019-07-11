package uk.ac.cam.cares.jps.ship;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Scanner;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.util.CommandHelper;

@WebServlet("/SLMAgent")
public class SpeedLoadMapWrapper extends HttpServlet {
	
	private void startspeedloadmap(String batchFolderlocation) {
		//system.out.println("starting the binary converter");
		String startSRMCommand = "C:/JPS_DATA/workingdir/JPS/SRM/ADMS-speed-load-map/SpeedLoadMap.bat ";
		CommandHelper.executeSingleCommand(batchFolderlocation, startSRMCommand);
	}
	
	//if(!source.contains("none")) {
	
	/*
	 * http://betterboat.com/average-boat-speed/ assume fastest medium boat max
	 * speed= 25knot max rpm= 2500 rpm torque=constant=250Nm then 1knot=100 rpm rpm=
	 * knot*100 roughly 1 ship 33 kg/h 1 boat= 1.1338650741577147e-05*3600 = 0.041
	 * kg/h NO2 (comparison of NO2
	 * https://pdfs.semanticscholar.org/1bd2/52f2ae1ede131d0ef84ee21c84a73fb6b374.pdf) 
	 * 1 boat mass flux=0.0192143028723584 kg/s 

	 */
	
	
	//double valuecalc=100*shipspeed;
	
	/*JSONObject in= new JSONObject();
	
}*/
	
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		
		JSONObject jo = AgentCaller.readJsonParameter(request);
		JSONObject in= new JSONObject();
		double valuecalc=jo.getDouble("speed")*100;
		String type=jo.getString("type");
		JSONObject speedob= new JSONObject();		
		speedob.put("value", valuecalc);
		speedob.put("unit", "RPM");
		JSONObject torob= new JSONObject();
		torob.put("value", 250);
		torob.put("unit", "Nm");
		in.put("speed", speedob);
		in.put("torque", torob);
		  try (FileWriter file = new FileWriter(AgentLocator.getPathToJpsWorkingDir() + "/JPS/SRM/ADMS-speed-load-map/in.json")) {
			  
	            file.write(in.toString());
	            file.flush();
	 
	        } catch (IOException e) {
	            e.printStackTrace();
	        }
		
		startspeedloadmap(AgentLocator.getPathToJpsWorkingDir() + "/JPS/SRM/ADMS-speed-load-map");
		String jsonFiledir = AgentLocator.getPathToJpsWorkingDir() + "/JPS/SRM/ADMS-speed-load-map/out.json";
	   
		
		File file = new File(jsonFiledir);
	    StringBuilder fileContents = new StringBuilder((int)file.length());        

	    try (Scanner scanner = new Scanner(file)) {
	        while(scanner.hasNextLine()) {
	            fileContents.append(scanner.nextLine() + System.lineSeparator());
	        }
	    }
	    
	    String newjsonfile = fileContents.toString();
	    
		JSONObject json = crankUpRealShipModel(type, newjsonfile);
		
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
		}
		
		
		
		for(int part=0;part<json.getJSONArray("particle").length();part++) {
			JSONObject particlemass=json.getJSONArray("particle").getJSONObject(part).getJSONObject("emission_rate");
			Double oldvaluemixmass= particlemass.getDouble("value");
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
		return json;
	}
	
	
}
