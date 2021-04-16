package uk.ac.cam.cares.jps.virtualsensor.agents;

import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.region.Region;
import uk.ac.cam.cares.jps.virtualsensor.objects.Point;
import uk.ac.cam.cares.jps.virtualsensor.sparql.DispSimSparql;
import uk.ac.cam.cares.jps.virtualsensor.sparql.SensorSparql;

@WebServlet("/SensorUpdaterAgent")
public class SensorUpdaterAgent extends JPSAgent{

	private static final long serialVersionUID = 1L;

	/**
	 * This agent accepts the IRI of an air quality station, then updates it
	 * Currently hardcoded to check whether the station is within the HK or Sg scopes
	 * Need to tag simulations semantically
	 */
	@Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        JSONObject responseParams = new JSONObject();
        if (validateInput(requestParams)) {
        	String stationiri = requestParams.getString(SensorSparql.keyAirStationIRI);
        	
        	String sim_iri = DispSimSparql.GetSimForSensor(stationiri);
        	
        	long latestTimeStamp;
        	
        	try {
        		latestTimeStamp = SensorSparql.GetLatestTimeStamp(stationiri);
        	} catch (Exception e) { // no data recorded yet
        		latestTimeStamp = 0;
        	}
        	
        	// this will give a list of outputs that have not been added to this sensor (after latestTimeStamp)
        	JSONArray queryResult = DispSimSparql.GetOutputPathAndTime(sim_iri, latestTimeStamp);
        	
        	if (queryResult.length() > 0) {
        		// obtain coordinates of this station
            	Point p = SensorSparql.queryAirStationCoordinatesWithIRI(stationiri);
            	String simCRS = DispSimSparql.GetSimCRS(sim_iri);
            	p.transform(simCRS);
        		
            	// go through each output and update sensor
        		for (int i=0; i<queryResult.length(); i++) {
		        	String outputPath = queryResult.getJSONObject(i).getString("outputPath");
		        	long timestamp = queryResult.getJSONObject(i).getLong("timestamp");
		        	URI path_uri = null;
		        	try {
						path_uri = new URI(outputPath);
					} catch (URISyntaxException e) {
						throw new JPSRuntimeException(e);
					}

		        	File f = new File(path_uri);
		        	String concFilePath = Paths.get(f.getAbsolutePath(),EpisodeAgent.FILE_NAME_3D_MAIN_CONC_DATA).toString();
		        	// convert sensor coordinates to the local simulation coordinates 
		        	// should be queried from the triple-store
		        	
		        	JSONObject request = new JSONObject();
		        	// now prepare to call interpolation agent
		        	request.put("filepath", concFilePath);
		        	request.put("x", p.getX());
		        	request.put("y", p.getY());
		
		        	String interp_result = AgentCaller.executeGetWithJsonParameter("JPS_VIRTUALSENSOR/InterpolationPyAgent", request.toString());
		        	JSONObject interp_result_jo = new JSONObject(interp_result);
		
		        	double concHC=0;
		        	double concNOx=0;
		        	// Sum individual HCs, we define HCs as molecules with at least 1 H and 1 C atoms
		        	List<String> HCs = Arrays.asList("C2H6","C2H4","nC4H10","HCHO","CH3CHO","CH3COC2H5","C3H6","oXylene","isoprene");
		        	// NOx is the sum of NO and NO2
		        	List<String> NOx = Arrays.asList("NO","NO2");
		        	// others
		        	List<String> other_pol = Arrays.asList("PM2.5","PM10","O3","SO2","CO2","CO");
		        	Iterator<String> it = interp_result_jo.keys();
		        	while (it.hasNext()) {
		        		String component = it.next();
		        		double conc = interp_result_jo.getJSONArray(component).getDouble(0); 
		        		
		        		String dataname;
		        		if (HCs.contains(component)) {
		        			concHC += conc; 
		        		} else if (NOx.contains(component)) {
		        			dataname = "Outside" + component + "Concentration";
		        			SensorSparql.addSensorValue(stationiri, dataname, conc, timestamp);
		        			concNOx += conc;
		        		} else if (other_pol.contains(component)) {
		        			// values other than HCs and NOx can be added directly
		        		    dataname = "Outside" + component + "Concentration";
		        			SensorSparql.addSensorValue(stationiri, dataname, conc, timestamp);
		        		}
		        	}
		        	SensorSparql.addSensorValue(stationiri, SensorSparql.HC, concHC, timestamp);
		        	SensorSparql.addSensorValue(stationiri, SensorSparql.NOx, concNOx, timestamp);
        		}
        	}
        }
        return responseParams;
    }
	
	@Override
	public boolean validateInput(JSONObject requestParams) {
    	boolean valid = false;
    	try {
    		// check if air quality station IRI is valid
    		new URL(requestParams.getString(Region.keyAirStationIRI)).toURI();
    		valid = true;
    	} catch (Exception e) {
    		throw new BadRequestException(e);
    	}
    	return valid;
    }
}
