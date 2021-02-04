package uk.ac.cam.cares.jps.ess.coordination;

import java.io.IOException;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.base.util.MiscUtil;



@WebServlet(urlPatterns = { "/startsimulationCoordinationESS" })
public class CoordinationESSAgent extends JPSAgent {
	
	private static final long serialVersionUID = 1L;
	@Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(CoordinationESSAgent.class);
    }
    @Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		requestParams = processRequestParameters(requestParams, null);
	    return requestParams;
    }
    @Override
   	public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		try {
			return startSimulation(requestParams);
		} catch (IOException e) {
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
	        String ENIRI = requestParams.getString("electricalnetwork");
	        boolean w = InputValidator.checkIfValidIRI(ENIRI);	        
	        JSONArray ja =requestParams.getJSONArray("RenewableEnergyGenerator");
			List<String> RenewableGenerators = MiscUtil.toList(ja);
			if (ja.length()!= 0) {
				for (int i = 0; i< RenewableGenerators.size(); i++) {
					if (RenewableGenerators.get(i)!= null) {
						boolean t = InputValidator.checkIfValidIRI(RenewableGenerators.get(i));
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
	public JSONObject startSimulation(JSONObject jo) throws IOException {
		
		//retrofit the generator of solar
		AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/RenewableGenRetrofit", jo.toString());
		
		//run the opf
		String result = AgentCaller.executeGetWithJsonParameter("JPS_ESS/ESSAgent", jo.toString());
		JSONObject res1=new JSONObject(result);
		jo.put("storage",res1.getString("storage"));
	
		
		String result2 = AgentCaller.executeGetWithJsonParameter("JPS_ESS/OptimizationAgent", jo.toString());
		JSONObject res2=new JSONObject(result2);
		
		String optimizationresult=res2.getString("optimization");
		//jo.put("optimization",optimizationresult);
		
		logger.info("starting the method selected"); //in this case OPF
		
		String resultStart = AgentCaller.executeGetWithJsonParameter(optimizationresult, jo.toString());
		
		logger.info("optimatization end result= "+resultStart);
						
		jo.put("batterylist",new JSONObject(resultStart).getJSONArray("batterylist"));
		
		String finresult=AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/EnergyStrorageRetrofit", jo.toString());
	
		logger.info("started creating battery");
		JSONObject finres= new JSONObject(finresult); 
		
		return finres;
		

		
	}
	

	
	
	
	

}
