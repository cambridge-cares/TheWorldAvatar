package uk.ac.cam.cares.jps.semakaupv;

import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.util.InputValidator;

@WebServlet(urlPatterns = { "/SemakauVisualization"})
public class SemakauVisualization extends JPSAgent {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
	    requestParams = processRequestParameters(requestParams, null);
	    return requestParams;
	}
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams,HttpServletRequest request){
		if (!validateInput(requestParams)) {
			throw new JSONException("SemakauVisualizationAgent: Input parameters not found.\n");
		}
		String irradiationsensorIRI=requestParams.getString("irradiationsensor");
		String pvgeneratorIRI=requestParams.optString("pvgenerator","http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/PV-002.owl#PV-002");
		String busIRI=requestParams.optString("ebus","http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/EBus-006.owl#EBus-006");
		JSONObject responseParams =  graphDataPoints(irradiationsensorIRI, pvgeneratorIRI, busIRI);
		System.gc();
		return responseParams;
	}
	@Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (requestParams.isEmpty()) {
            throw new BadRequestException();
        } 
        try {
            String pvgeneratorIRI = requestParams.getString("pvgenerator");
            boolean q = InputValidator.checkIfValidIRI(pvgeneratorIRI);
            String iriofirrF=requestParams.getString("irradiationsensor");
	        boolean r = InputValidator.checkIfValidIRI(iriofirrF);
            String busIRI=requestParams.getString("ebus");
	        boolean w = InputValidator.checkIfValidIRI(busIRI);
            return q&r&w;
        } catch (JSONException ex) {        	
        	return false;
        } 
    }
	/** extracts all graph data from IRIs and dumps in JSON Object
	 * 
	 * @param irradiationsensorIRI
	 * @param pvgeneratorIRI
	 * @param busIRI
	 * @return
	 */
	public JSONObject graphDataPoints(String irradiationsensorIRI, String pvgeneratorIRI, String busIRI) {
		SelectBuilder pvGeneratorSB = SemakauPV.preparePVGeneratorWithPropTime().addVar("?activepowervalue").addVar("?reactivepowervalue");
		List<String[]> resultListfromquerygen = SemakauPV.queryResult(pvgeneratorIRI, pvGeneratorSB.buildString());
		SelectBuilder busSB = SemakauPV.prepareBusWithPropTime().addVar("?VoltMagvalue").addVar("?VoltAnglevalue");
		
		List<String[]>  resultListfromquerybus  = SemakauPV.queryResult(busIRI,busSB.buildString());
		List<String[]>  resultListfromqueryirrad  = SemakauPV.queryResult(irradiationsensorIRI, SemakauPV.getSolarData());
		JSONObject jo = new JSONObject();
	    JSONArray propValLst = new JSONArray();
	    JSONArray proptimeLst = new JSONArray();
	    JSONArray VoltMagval = new JSONArray();
	    JSONArray VoltAngleval = new JSONArray();
	    JSONArray activepowerval= new JSONArray();
	    JSONArray reactivepowerval = new JSONArray();
		for (int i = 0; i< resultListfromquerygen.size(); i++ ) {
			propValLst.put(resultListfromqueryirrad.get(i)[1]);
			String propTime = resultListfromqueryirrad.get(i)[2];
			propTime = propTime.split("T")[1];
			proptimeLst.put(propTime.split("\\+")[0]);
			activepowerval.put(resultListfromquerygen.get(i)[3]);
			reactivepowerval.put(resultListfromquerygen.get(i)[4]);
			
			VoltMagval.put(resultListfromquerybus.get(i)[4]);
			VoltAngleval.put(resultListfromquerybus.get(i)[5]);
		}
		jo.put("propVal", propValLst).put("proptime", proptimeLst).put("activePower", activepowerval)
		.put("reactivePower", reactivepowerval).put("VoltMag", VoltMagval).put("VoltAng", VoltAngleval);
		return jo;
	}
}
