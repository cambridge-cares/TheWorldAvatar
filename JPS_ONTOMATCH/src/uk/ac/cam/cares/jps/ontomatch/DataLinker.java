package uk.ac.cam.cares.jps.ontomatch;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServlet;

import org.apache.commons.io.FileUtils;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

/**
 * Agent that read alignment result file, then write linkage to kb
 * @author zsc
 *
 */
@WebServlet(urlPatterns = { "/dataLinker" })
public class DataLinker extends JPSHttpServlet {
    
    
	/**
	 * 
	 */
	private static final long serialVersionUID = -7950910519843708707L;

	@Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(DataLinker.class);
    }
	
	Logger logger = LoggerFactory.getLogger(DataLinker.class);
	
	@Override
	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		System.out.println("Datalinker agent");

		JSONObject jo = requestParams;
		// read alignment
		String afileIRI = "";
		try {
			//afileIRI = jo.getString("alignmentIRI");
		} catch (JSONException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		//TODO: add this to config
	    String threshold = "0.6";
	    String stubIRI = "http://localhost:3000/a.owl";
		System.out.println("reading alignment from  " + stubIRI);
		//get a list of matched IRIs where 
		//add to destination
		List<String[]> instances2Equal = provideAlignedInstanceList(stubIRI, threshold);
		addEqual(instances2Equal);
		JSONObject result = new JSONObject();
		JSONArray resArr = new JSONArray();
		try {
			result.put("success", 1);
			System.out.print("read parameter result: ");
			System.out.println(result.toString());
		} catch (JSONException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return result;
		//logger.info("optimization result = " + result);
	}


	

	
	//TODO: merge this function into ALignment Helper
	public List<String[]> provideAlignedInstanceList(String iriOfAlignmentFile,String threshold) {
		String queryStr = "PREFIX alignment: <http://knowledgeweb.semanticweb.org/heterogeneity/alignment#> "
				+ "SELECT ?entity1 ?entity2 " 
				//+ "WHERE {?a ?p ?o."

				+ "WHERE {?cell alignment:entity1 ?entity1."
				+ "?cell  alignment:entity2 ?entity2 ."
				+"?cell alignment:measure ?measure."

				+ "FILTER (?measure >= "+threshold +" ) " //filtering gen 001 as it is slackbus
				+ "}";
		System.out.println(queryStr);
		List<String[]> resultListfromquery = null;
		try {
			OntModel model = JenaHelper.createModel(iriOfAlignmentFile);
			ResultSet resultSet = JenaHelper.query(model, queryStr);
			String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
			String[] keys = JenaResultSetFormatter.getKeys(result);
			resultListfromquery = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
	        System.out.println("reading alignment:");
			System.out.println(resultListfromquery.toString());
			
		}
		catch(Exception e) {
            StringWriter sw = new StringWriter();
            e.printStackTrace(new PrintWriter(sw));
            String exceptionAsString = sw.toString();
			logger.error(exceptionAsString);
		}
		return resultListfromquery;

	}
		

	/**
	 * sparql update to powerplant databases
	 * @param alignedInstances
	 */
	public void addEqual(List<String[]> alignedInstances) {
        QueryBroker broker = new QueryBroker();
        //read file, then rewrite whole file and save
        for (String[] paras :alignedInstances) {
        	String iri1 = paras[0];
        	String iri2 = paras[1];
            String updateStr = "PREFIX owl: <http://www.w3.org/2002/07/owl#> "+//
                    "INSERT DATA\r\n" + 
                    		"{<"+iri1+"> owl:sameAs <"+iri2+">.\r\n" + 
                    		"}";        	
           System.out.println(iri1);
           try {
               broker.updateFile(iri1, updateStr);

           }catch(Exception e) {
               StringWriter sw = new StringWriter();
               e.printStackTrace(new PrintWriter(sw));
               String exceptionAsString = sw.toString();
   			System.out.println(exceptionAsString);
           }
        }
	}

}
