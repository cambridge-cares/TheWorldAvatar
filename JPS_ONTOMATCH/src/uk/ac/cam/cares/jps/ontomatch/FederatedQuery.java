package uk.ac.cam.cares.jps.ontomatch;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

@WebServlet(urlPatterns = { "/federatedQuery" })
public class FederatedQuery extends JPSHttpServlet{

	private static final long serialVersionUID = -2354646810093235777L;
    private String serviceIRI = null;
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
		String entityIRI = null;
		System.out.println(jo.toString());
		try {
			entityIRI = jo.getString("entityIRI");
			System.out.println("QUERY FOR: "+entityIRI);
		} catch (JSONException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		//stub for testing
	    String stubIRI = "http://www.theworldavatar.com/kb/powerplants/Altbach_Coal_Power_Plant_Germany.owl#Altbach_Coal_Power_Plant_Germany";
		JSONArray resArr = performFederatedQuery(stubIRI);
		JSONObject result = new JSONObject();
		int tripleNumber = resArr.length();
		try {
			result.put("content", resArr);
			result.put("number", tripleNumber);
			System.out.print("read parameter result: ");
			System.out.println(result.toString());
		} catch (JSONException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return result;

		//TODO: return must e json array
		//logger.info("optimization result = " + result);
	}


	/**only neeeded for local testing**/
    public String convertIRI(String iri, boolean local2IRI) {
    	if (local2IRI == true) {
    	return iri.replace("localhost:3000", "www.theworldavatar.com");
    	}else{
    		return iri.replace("www.theworldavatar.com","localhost:3000");
    	}}

	public JSONArray performFederatedQuery(String iri) {
		String queryStrRemote = "PREFIX owl: <http://www.w3.org/2002/07/owl#> "
				+ "SELECT distinct ?p ?o " 
				+ "WHERE {<"+iri+"> owl:sameAs ?dbiri."
				+ " SERVICE <http://dbpedia.org/sparql> "  
				+" {?dbiri ?p ?o.}"
				+"}";
		System.out.println(queryStrRemote);
		
		String queryStrLocal = "SELECT distinct ?p ?o "+
				"WHERE {<"+iri+"> ?p ?o."+
				"}";

		JSONArray resArr = new JSONArray();
		try {
			OntModel model = JenaHelper.createModel(convertIRI(iri,false));
			ResultSet resultSetRemote = JenaHelper.query(model, queryStrRemote);
			ResultSet resultSetLocal = JenaHelper.query(model, queryStrLocal);

			String resultR = JenaResultSetFormatter.convertToJSONW3CStandard(resultSetRemote);
			System.out.println(resultR);
			String resultL = JenaResultSetFormatter.convertToJSONW3CStandard(resultSetLocal);
			System.out.println(resultL);
			String[] keys = JenaResultSetFormatter.getKeys(resultR);
			List<String[]> localListfromquery = JenaResultSetFormatter.convertToListofStringArrays(resultL, keys);
			List<String[]> remoteListfromquery = JenaResultSetFormatter.convertToListofStringArrays(resultR, keys);
			List<String[]> combined = new ArrayList<String[]>(localListfromquery);
			combined.addAll(remoteListfromquery);
			System.out.println("reading attributes:");
			for(String[] paras:combined) {
				JSONObject resObj = new JSONObject();
				for(int idx = 0; idx<keys.length; idx++) {
					resObj.put(keys[idx], paras[idx]);
				}
				resArr.put(resObj);
				System.out.println(resObj);
			}
		}
		catch(Exception e) {
            StringWriter sw = new StringWriter();
            e.printStackTrace(new PrintWriter(sw));
            String exceptionAsString = sw.toString();
			logger.error(exceptionAsString);
		}
		return resArr;

	}
		

	
}
