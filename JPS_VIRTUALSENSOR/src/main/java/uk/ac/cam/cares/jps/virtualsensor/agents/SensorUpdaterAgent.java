package uk.ac.cam.cares.jps.virtualsensor.agents;

import java.net.URL;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.region.Region;
import uk.ac.cam.cares.jps.virtualsensor.objects.Scope;
import uk.ac.cam.cares.jps.base.util.CRSTransformer;
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
        	String stationiri = requestParams.getString(Region.keyAirStationIRI);
        	// obtain coordinates of this station
        	double [] xy = SensorSparql.queryAirStationCoordinatesWithIRI(stationiri);
        	
        	// check whether this station is located within the Singapore or HK scope
        	// coordinates of the stations are obtained from clicking on the map - EPSG:4326
        	// these should be replaced with queries
        	JSONObject sgJo = new JSONObject();
        	Region.putRegion(sgJo, 2);
        	Scope sgScope = new Scope(sgJo.getJSONObject(Region.keyRegion));
        	sgScope.transform(CRSTransformer.EPSG_4326);
        	
        	JSONObject hkJo = new JSONObject();
        	Region.putRegion(hkJo, 4);
        	Scope hkScope = new Scope(hkJo.getJSONObject(Region.keyRegion));
        	hkScope.transform(CRSTransformer.EPSG_4326);

        	JSONObject plyJo = new JSONObject();
        	Region.putRegion(plyJo, 5);
        	Scope plyScope = new Scope(plyJo.getJSONObject(Region.keyRegion));
        	plyScope.transform(CRSTransformer.EPSG_4326);
        	
        	JSONObject request = new JSONObject();
        	if (sgScope.isWithinScope(xy)) {
        		request.put(Region.keyCity, Region.SINGAPORE_IRI);
        	} else if (hkScope.isWithinScope(xy)) {
        		request.put(Region.keyCity, Region.HONG_KONG_IRI);
        	} else if (plyScope.isWithinScope(xy)) {
        		request.put(Region.keyCity,  Region.PLYMOUTH_IRI);
        	} else {
        		// do nothing if it's not in any simulated scopes
        		return responseParams;
        	}
        	
        	String resultlocation = AgentCaller.executeGetWithJsonParameter("JPS_VIRTUALSENSOR/episode/results/latest", request.toString());
        	// convert sensor coordinates to the local simulation coordinates 
        	// should be queried from the triple-store
        	double[] xy_local = CRSTransformer.transform(CRSTransformer.EPSG_4326, 
        			Region.getTargetCRSName("episode", request.getString(Region.keyCity)), xy);
        	request.remove(Region.keyCity);
        	
        	// now prepare to call interpolation agent
        	request.put("filepath", resultlocation);
        	request.put("x", xy_local[0]);
        	request.put("y", xy_local[1]);

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
        			SensorSparql.addSensorValue(stationiri, dataname, conc, true);
        			concNOx += conc;
        		} else if (other_pol.contains(component)) {
        			// values other than HCs and NOx can be added directly
        		    dataname = "Outside" + component + "Concentration";
        			SensorSparql.addSensorValue(stationiri, dataname, conc, true);
        		}
        	}
        	SensorSparql.addSensorValue(stationiri, SensorSparql.HC, concHC, true);
        	SensorSparql.addSensorValue(stationiri, SensorSparql.NOx, concNOx, true);
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
