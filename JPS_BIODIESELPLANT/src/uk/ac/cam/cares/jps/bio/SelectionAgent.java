package uk.ac.cam.cares.jps.bio;

import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;



@WebServlet("/DoModelSelection")
public class SelectionAgent extends JPSHttpServlet {
	
    @Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(SelectionAgent.class);
    }
    Logger logger = LoggerFactory.getLogger(SelectionAgent.class);
    
    
    @Override
   	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
    	String topnode="http://www.theworldavatar.com/kb/TheWorld.owl#TheWorld";
		
    	String compIRI=requestParams.getString("componentIRI");
		   //String compIRI="http://www.jparksimulator.com/kb/sgp/jurongisland/biodieselplant3/R-301.owl#R-301";
    	String componentlistInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontoeip/ecoindustrialpark/EcoIndustrialPark.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?plant "
				+ "WHERE {"
				+ "?place j2:hasSubsystem ?plant ."
				+ "?place  a  j1:Eco-industrialPark ."
				+ "}";

		   
		   JSONObject response = extractedJSONOutput(topnode, componentlistInfo, compIRI);
		   String pathiri=response.getString("path");
		   AgentCaller.executeGetWithJsonParameter(pathiri, response.toString());
    	return response;	
    }


	public JSONObject extractedJSONOutput(String topnode, String componentlistInfo, String compIRI) {

       	String plantInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontoeip/ecoindustrialpark/EcoIndustrialPark.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?plant ?path "
				+ "WHERE {"
				+ "?plant a  j2:CompositeSystem ."
				+ "?plant j2:isModeledBy ?model ."
				+ "?model j2:hasURLPath ?path ."
				+ "?plant   j2:hasSubsystem <"+compIRI+"> ." 
				+ "}";   
		OntModel model=readModelGreedy(topnode);
		   ResultSet resultSet = JenaHelper.query(model, componentlistInfo);
			String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
	        String[] keysplant = JenaResultSetFormatter.getKeys(result);
	        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keysplant);
	        System.out.println("what is resultList size= "+resultList.size());
	        String selectedplant=null;
	        String selectedpath=null;
	        for(int d=0;d<resultList.size();d++) {
				String result2 = new QueryBroker().queryFile(resultList.get(d)[0], plantInfo);
				String[] keysplant2 = JenaResultSetFormatter.getKeys(result2);
				List<String[]> resultList2 = JenaResultSetFormatter.convertToListofStringArrays(result2, keysplant2);
		        if(resultList2.size()>0) {
		        	selectedplant=resultList2.get(0)[0];
		        	selectedpath=resultList2.get(0)[1];
		        System.out.println("what is the plant= "+resultList2.get(0)[0]);
		        System.out.println("what is the path= "+resultList2.get(0)[1]);
		        }
	        }
	        JSONObject response= new JSONObject();
	        response.put("PLANTIRI",selectedplant);
	        response.put("path",selectedpath);
		return response;
	}
    
    
	public static OntModel readModelGreedy(String iriofEIP) {
		String plantlistInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontoeip/ecoindustrialpark/EcoIndustrialPark.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?component "
				+ "WHERE {?entity  a  j2:CompositeSystem  ." + "?entity   j2:hasSubsystem ?component ." + "}";

		QueryBroker broker = new QueryBroker();
		return broker.readModelGreedy(iriofEIP, plantlistInfo);
	}
}
