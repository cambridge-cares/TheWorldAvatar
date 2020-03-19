package uk.ac.cam.cares.jps.bio;

import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
    	OntModel model=readModelGreedy(topnode);
    	String plantlistInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontoeip/ecoindustrialpark/EcoIndustrialPark.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?plant "
				+ "WHERE {"
				+ "?world j2:hasSubsystem ?place ."
				+ "?place j2:hasSubsystem ?plant ."
				+ "?plant  a  j2:CompositeSystem ."
				//+ "?plant j2:isModeledBy ?model ."
				+ "?plant   j2:hasSubsystem <"+compIRI+"> ." 
				+ "}";
    	
    	ResultSet resultSet = JenaHelper.query(model, plantlistInfo);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
        String[] keysplant = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keysplant);
    	
    	
    	return requestParams;	
    }
    
    
	public static OntModel readModelGreedy(String iriofEIP) {
		String plantlistInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontoeip/ecoindustrialpark/EcoIndustrialPark.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?component "
				+ "WHERE {?entity  a  j1:Eco-industrialPark  ." + "?entity   j2:hasSubsystem ?component ." + "}";

		QueryBroker broker = new QueryBroker();
		return broker.readModelGreedy(iriofEIP, plantlistInfo);
	}
}
