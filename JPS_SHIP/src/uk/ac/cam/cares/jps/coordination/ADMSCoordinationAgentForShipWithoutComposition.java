package uk.ac.cam.cares.jps.coordination;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.util.PythonHelper;

@WebServlet("/ADMSCoordinationAgentForShipWithoutComposition")
public class ADMSCoordinationAgentForShipWithoutComposition extends HttpServlet {

	private static final long serialVersionUID = -2264681360832342804L;
	Logger logger = LoggerFactory.getLogger(ADMSCoordinationAgentForShipWithoutComposition.class);

	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
	
		String jsonInput = AgentCaller.readJsonParameter(request).toString();		
		JSONObject result = executeWithoutComposition(jsonInput);
		AgentCaller.writeJsonParameter(response, result);
	}
	
	private void updateShipCoordinates() throws IOException {
		File file = new File("C:\\JPS_DATA\\workingdir\\JPS\\SHIP\\counter.txt");        
        BufferedReader reader = null;
        FileWriter writer = null;
        int countIn = 0;
        int countOut = 0;
        
        try 
        {
            reader = new BufferedReader(new FileReader(file));
            
            //Reading all the lines of input text file into oldContent
            
            String line = reader.readLine();
            logger.info("line: " + line);
            
            countIn = Integer.parseInt(line);
            logger.info(String.valueOf(countIn));
            
            //Rewriting the input text file with newContent
            
            writer = new FileWriter(file);
            
    		if (countIn < 4 && countIn >= 0) {
    			countOut = countIn + 1;
    		} else {
    			countOut = 0;
    		}
    		logger.info("countOut: " + String.valueOf(countOut));
            writer.write(String.valueOf(countOut));
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }
        finally
        {
            reader.close();                
            writer.close();
            String result = PythonHelper.callPython("caresjpsship/writeThenReadShipCoordinates.py", String.valueOf(countIn), this);
            logger.info("DONE UPDATING SHIP COORDINATES with count " + String.valueOf(countIn));
        }
	}
	
	public JSONObject executeWithoutComposition(String jsonInput) throws IOException {
		
		try {
			
			JSONObject jo = new JSONObject(jsonInput);
			
			
			//updateShipCoordinates();
			
			//String jsonArrayOfShipIRI =execute("/JPS_SHIP/GetShipListFromRegion",jsonInput);	

			String jsonArrayOfShipIRI =execute("/JPS_POSTGRESQL/getEntitiesWithinRegion",jsonInput);

			String url = KeyValueManager.get(IKeys.URL_POSITIONQUERY);
			url += "/getEntitiesWithinRegion";
			AgentCaller.executeGetWithURLAndJSON(url, jsonInput);			
				
			JSONObject jsonShipIRIs = new JSONObject(jsonArrayOfShipIRI);
			JSONArray shipIRIs = jsonShipIRIs.getJSONArray("shipIRIs");
			jo.put("ship", shipIRIs);
//			logger.info("shipIRIs FROM COORDINA
			
			JSONObject jsonReactionShip = new JSONObject();
			String reactionMechanism = jo.getString("reactionmechanism");
			jsonReactionShip.put("reactionmechanism", reactionMechanism);
						
//			for (int i = 0; i < shipIRIs.length(); i++) {
			for (int i = 0; i < 1; i++) {
				String shipIRI = shipIRIs.getString(i);
				jsonReactionShip.put("ship", shipIRI);
				
				//String wasteResult = AgentCaller.executeGet("/JPS_SHIP/ShipAgent", "query", jsonReactionShip.toString());
				String wasteResult = execute("/JPS_SHIP/ShipAgent", jsonReactionShip.toString());
				String waste = new JSONObject(wasteResult).getString("waste");
				jo.put("waste", waste);
			}
			// TODO: SC
			// Iterate over list of ship iris and perform query of each ship.
			
						
			String regionToCityResult = execute("/JPS/RegionToCity", jsonInput);
			String city = new JSONObject(regionToCityResult).getString("city");
			jo.put("city", city);
//			logger.info("city FROM COORDINATION AGENT: " + city);
			
			String result = execute("/JPS/GetBuildingListFromRegion", jo.toString());
			JSONArray building = new JSONObject(result).getJSONArray("building");
			jo.put("building", building);
//			logger.info("building FROM COORDINATION AGENT: " + building.toString());
			
			result = execute("/JPS_COMPOSITION/CityToWeather", regionToCityResult);
			JSONObject weatherstate = new JSONObject(result).getJSONObject("weatherstate");
			jo.put("weatherstate", weatherstate);
//			logger.info("weatherstate FROM COORDINATION AGENT: " + weatherstate.toString());
			
			result = execute("/JPS/ADMSAgent", jo.toString());
			String folder = new JSONObject(result).getString("folder");
			jo.put("folder", folder);
			
			return jo;
			
		} catch (JSONException e) {
			logger.error(e.getMessage(), e);
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}
	
	protected String execute(String path, String jsonInput) {

		logger.info("execute for path=" + path + ", json=" + jsonInput);
		String result = AgentCaller.executeGet(path, "query", jsonInput);
		logger.info("execution result=" + result);
		return result;
	}
}
