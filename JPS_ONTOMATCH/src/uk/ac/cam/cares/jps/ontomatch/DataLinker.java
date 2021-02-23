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
 * Agent that read alignment result file, then write linkage to KG
 * Input from KG: alignment Ontology of one matching process
 * Output to KG: owl:sameAs triples
 * @author shaocong zhang
 * @version 1.0
 * @since 2020-09-08
 */
@WebServlet(urlPatterns = { "/dataLinker" })
public class DataLinker extends JPSHttpServlet {
    
    

	private static final long serialVersionUID = -7950910519843708707L;


	@Override
	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		logger.info("Datalinker agent");

		JSONObject jo = requestParams;
		String afileIRI = "";
		double threshold = 0.0;
		try {
			afileIRI = jo.getString("alignmentIRI");
			threshold = jo.getDouble("threshold");

		} catch (JSONException e1) {
			e1.printStackTrace();
		}

		logger.info("reading alignment from  " + afileIRI);
		//get a list of matched IRIs 
		List<String[]> instances2Equal = provideAlignedInstanceList(afileIRI, threshold);
		//add owl:sameAs triple to KG
		addEqual(instances2Equal);
		JSONObject result = new JSONObject();
		try {
			result.put("success", 1);
		} catch (JSONException e) {
			e.printStackTrace();
		}
		return result;
	}


	

   /***
    * function that reads the alignment list from an alignment ontology file and return	it as a list of string array
    * @param iriOfAlignmentFile
    * @param threshold threshold of measure to filter aligned entities that get returned by query
    * @return alignment result as list of string array <{entity1, entity2, measure}...>
    */
	public List<String[]> provideAlignedInstanceList(String iriOfAlignmentFile,double threshold) {
		String queryStr = "PREFIX alignment: <http://knowledgeweb.semanticweb.org/heterogeneity/alignment#> "
				+ "SELECT ?entity1 ?entity2 " 
				//+ "WHERE {?a ?p ?o."

				+ "WHERE {?cell alignment:entity1 ?entity1."
				+ "?cell  alignment:entity2 ?entity2 ."
				+"?cell alignment:measure ?measure."

				+ "FILTER (?measure >= "+Double.toString(threshold) +" ) " //filtering gen 001 as it is slackbus
				+ "}";
		System.out.println(queryStr);
		List<String[]> resultListfromquery = null;
		try {
			OntModel model = JenaHelper.createModel(iriOfAlignmentFile);
			ResultSet resultSet = JenaHelper.query(model, queryStr);
			String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
			String[] keys = JenaResultSetFormatter.getKeys(result);
			resultListfromquery = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
			
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
	 * sparql update owl:sameAs triple to KG
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
           try {
               broker.updateFile(iri1, updateStr);

           }catch(Exception e) {
               StringWriter sw = new StringWriter();
               e.printStackTrace(new PrintWriter(sw));
               String exceptionAsString = sw.toString();
   			logger.error(exceptionAsString);
           }
        }
	}

}
