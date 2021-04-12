package uk.ac.cam.cares.jps.powsys.coordination;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.sparql.Paths;
import uk.ac.cam.cares.jps.base.query.sparql.Prefixes;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.InputValidator;

@WebServlet(urlPatterns = { "/startsimulation", "/processresult", "/processresultwithpf", "/processresultwithopf" })
public class CoordinationAgent extends JPSAgent implements Prefixes, Paths {

	private static final long serialVersionUID = 6859324316966357379L;
    @Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(CoordinationAgent.class);
    }
    Logger logger = LoggerFactory.getLogger(CoordinationAgent.class);
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        requestParams = processRequestParameters(requestParams, null);
        return requestParams;
    }
    @Override
	public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
    	if (!validateInput(requestParams)) {
			throw new JSONException ("CoordinationAgent: Input parameters not found.\n");
		}
		String path = requestParams.getString("path");

		if (path.contains("/startsimulation") ){
			
			startSimulation(requestParams);
			
		} else if (path.contains("/processresult") ){
			
			AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/retrofit", requestParams.toString());
			
		} else if (path.contains("/processresultwithpf")) { //unused
			
			String pathForENAgent = "JPS_POWSYS/ENAgent/startsimulationPF";
			AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/retrofit", requestParams.toString());
			AgentCaller.executeGetWithJsonParameter(pathForENAgent, requestParams.toString());
			
		} else if (path.contains("/processresultwithopf")) { //unused
			
			String pathForENAgent = "JPS_POWSYS/ENAgent/startsimulationOPF";
			AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/retrofit", requestParams.toString());
			AgentCaller.executeGetWithJsonParameter(pathForENAgent, requestParams.toString());
			
		}
		return requestParams;
		
	}
    @Override
    /** validates input by checking if path and electricalnetwork parameters are present
     * 
     */
      public boolean validateInput(JSONObject requestParams) throws BadRequestException {
          if (requestParams.isEmpty()) {
              throw new BadRequestException();
          }
          try {
          String iriofnetwork = requestParams.getString("electricalnetwork");
          
          return InputValidator.checkIfValidIRI(iriofnetwork) ;
          } catch (JSONException ex) {
            return false;
          }
      }
	public void startSimulation(JSONObject jo) {
		
		logger.info("starting optimization for carbon tax");
		
		String result = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/optimizeforcarbontax", jo.toString());
		
		logger.info("carbon tax optimization finished with result = " + result);
	
		JSONObject jo2= new JSONObject(result);
		
		jo.put("substitutionalgenerators",jo2.getJSONArray("substitutionalgenerators"));
		
		result = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/NuclearAgent/startsimulation", jo.toString());
	
		logger.info("started npp optimization asynchronously");
		//logger.info("started npp optimization synchronously");
//		JSONObject jo3= new JSONObject(result);
//		jo.put("plants",jo3.getJSONArray("plants"));
//		
//		result = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/retrofit", jo.toString());
		
	}
}
