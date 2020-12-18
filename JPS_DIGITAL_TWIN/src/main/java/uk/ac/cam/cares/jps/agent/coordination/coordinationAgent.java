package uk.ac.cam.cares.jps.agent.coordination;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.agent.gPROMS.gPROMSAgent;
import uk.ac.cam.cares.jps.agent.matlab.JPSMatlabAgent;
import uk.ac.cam.cares.jps.base.agent.*;
import uk.ac.cam.cares.jps.base.annotate.MetaDataQuery;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;

@WebServlet("/CoordinationAgent")
public class coordinationAgent extends JPSAgent  {

	public static final String CHEMICAL_SYSTEM_IRI = "http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/debutaniser_section.owl";
	public static final String ELECTRICAL_SYSTEM_IRI = "www.theworldavatar.com/ontology/ontopowsys/electrical_system.owl";
	
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		
	//Execution of gPROMS
	requestParams.put("Chemical_IRI", CHEMICAL_SYSTEM_IRI);
	String gPROMSStart = AgentCaller
		        .executeGetWithJsonParameter("ElChemoAgent/job/request", requestParams.toString());
	
	//Execution of SPIN for chemical
	//requestParams.put("gPROMS_URI", gPROMSAgent.GPROMS_AGENT_URL);
	String spinStartc = AgentCaller
	        .executeGetWithJsonParameter("ElChemoAgent/SpinChemical", requestParams.toString());
	
	if (spinStartc!=null){
		System.exit(0);
	}
	
	else {	 
	//Taking  Matlab agent inputs
	requestParams.put("Electrical_IRI",ELECTRICAL_SYSTEM_IRI);	
	
    String csvFilePath = null;
    List<String> lst = null;
    String resultFromRDF4J =
        MetaDataQuery.queryResources(null, null, null, gPROMSAgent.GPROMS_AGENT_URL, null, null, null, lst);
    String[] keys = JenaResultSetFormatter.getKeys(resultFromRDF4J);
    List<String[]> listmap =
        JenaResultSetFormatter.convertToListofStringArrays(resultFromRDF4J, keys);
    requestParams.put("inputfile",csvFilePath);
    
    //Executing Matlab
	String matlabStart = AgentCaller
	        .executeGetWithJsonParameter("ElChemoAgent/JPSMatlabAgent/startSimulation", requestParams.toString());
	
	//Execution of Spin for electrical
    String matFilePath = null;
    List<String> lstm = null;
    String resultFromRDF4Jm =
        MetaDataQuery.queryResources(null, null, null, JPSMatlabAgent.MATLAB_AGENT_URL, null, null, null, lst);
    String[] keysm = JenaResultSetFormatter.getKeys(resultFromRDF4J);
    List<String[]> listmapm =
        JenaResultSetFormatter.convertToListofStringArrays(resultFromRDF4J, keys);
    requestParams.put("matoutfile",matFilePath);
	requestParams.put("Matlab_IRI", JPSMatlabAgent.MATLAB_AGENT_URL);
	String spinStarte = AgentCaller
	        .executeGetWithJsonParameter("ElChemoAgent/SpinElectrical", requestParams.toString());
	if (spinStarte!=null){
		System.exit(0);
	}
	}
	return requestParams;
	}
}
