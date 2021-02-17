package uk.ac.cam.cares.jps.powsys.retrofit;

import java.util.ArrayList;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.base.util.MiscUtil;

@WebServlet("/EnergyStrorageRetrofit")
public class BatteryRetrofit extends JPSAgent {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private GeneralRetrofitAgent gRA;
	
	public BatteryRetrofit(){
		gRA = new GeneralRetrofitAgent();
	}
    @Override
    public void setLogger() {
        logger = LoggerFactory.getLogger(BatteryRetrofit.class);
    }
    Logger logger = LoggerFactory.getLogger(BatteryRetrofit.class);
	public void retrofitEnergyStorage(String electricalNetwork, List<String> BatteryList) {
		List<GeneratorInfo> batteries = new ArrayList<GeneratorInfo>();
		for (String currentGen : BatteryList) {
			String batIri = currentGen;
			GeneratorInfo info = new GeneratorInfo();
			info.generatorIri = batIri;	
			batteries.add(info);
		}

		
		gRA.addGeneratorsToElectricalNetwork(electricalNetwork, batteries);
		
		logger.info("finished retrofitting energy storage");
	}
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		requestParams = processRequestParameters(requestParams, null);
		return requestParams;
	}
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		
		if (!validateInput(requestParams)) {
			throw new JSONException("BatteryRetrofitAgent Input parameters invalid");
		}
		String electricalNetwork = requestParams.getString("electricalnetwork");
		JSONArray ja = requestParams.getJSONArray("batterylist");
		List<String> BatteryList = MiscUtil.toList(ja);
		retrofitEnergyStorage(electricalNetwork, BatteryList);
		// TODO Auto-generated method stub
		return requestParams;
	}
	@Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (requestParams.isEmpty()) {
            throw new BadRequestException();
        }
        try {
	        String ENIRI = requestParams.getString("electricalnetwork");
	        boolean w = InputValidator.checkIfValidIRI(ENIRI);	        
	        JSONArray ja =requestParams.getJSONArray("batterylist");
			List<String> batteries = MiscUtil.toList(ja);
			if (ja.length()!= 0) {
				for (int i = 0; i< batteries.size(); i++) {
					if (batteries.get(i)!= null) {
						boolean t = InputValidator.checkIfValidIRI(batteries.get(i));
						if (t == false) {
							return false;
						}
					}
				}
			}else {
				return false;
			}
	        return w;
        } catch (JSONException ex) {
        	ex.printStackTrace();
        }
        return false;
    }
	
}
