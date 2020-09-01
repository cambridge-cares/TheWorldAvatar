package uk.ac.cam.cares.jps.ontomatch;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.AsyncPythonHelper;

/***
 * 
Agent that performs element level matching, four types can be chosen:
string, word, domain, value
Functionally it is a servlet wrapper around the python programs that actually does the job 
Input: IRI of ontology
Output: IRI to alignmentFile
 * @author shaocong
 */


@WebServlet(urlPatterns = { "/elementMatcher" })
public class ElementMatcher extends JPSHttpServlet{
	/**
	 * 
	 */
	private static final long serialVersionUID = -7032945484999523116L;

	/**Type of element matchers**/
	public enum MATCHERTYPE {
	    DOMAIN, STRING,WORD, VALUE,I_STRING,I_WORD
	}
	/**Types of matching type, only related to function names in py script, no algorithm difference**/
	public enum MATCHING_TYPE {
	    INDIVIDUAL, TERM
	}
	private final String MATCH_TERM_FUNCTION_NAME = "matchSerial";
	private final String MATCH_INDIVIDUAL_FUNCTION_NAME = "matchIndividuals";

	@Override
	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		System.out.println("element matcher agent");
		JSONObject jo = requestParams;
		String savePath = null, matchMethod = null, targetOnto = null, sourceOnto = null,
				modelPath = null, dictPath = null;
        MATCHERTYPE type = null;
        MATCHING_TYPE matchingType = null;
		String tgtPkl, srcPkl, pyMatchFunction = null;

		
		//query for metadata: pkl address

	   
		/***read params****/
		try {
			savePath = jo.getString("alignmentFileAddress");
			targetOnto = jo.getString("targetOntoIRI");
			sourceOnto = jo.getString("sourceOntoIRI");
			type = MATCHERTYPE.valueOf(jo.getString("matcherType"));
			matchingType = MATCHING_TYPE.valueOf(jo.getString("matchingType"));
			/**special case: domain matcher needs extra params**/
			if(type == MATCHERTYPE.DOMAIN) {
				modelPath = jo.getString("modelAddress");
				dictPath = jo.getString("dictAddress");
			}
		
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		JSONObject resultObj = new JSONObject();
		/***call python, then save to kb(rdf)*****/
		try {
			/**get matching function name in py*/
			pyMatchFunction = getPyFunctionName(matchingType);
			/**query pkl address***/
			tgtPkl = queryLexicalPklAddressfromMetaData(targetOnto);
			srcPkl = queryLexicalPklAddressfromMetaData(sourceOnto);	
			
		    /**get matcher script addr****/	
			String[] results = null;
			String matcherLocation = getMatcherScriptName(type);//get script name by matcher type and call
			/**construct params list according to matcher type, then call pythonHelper***/
			if(type == MATCHERTYPE.DOMAIN) {//special case: domain matcher needs extra params
				String[] paras = {savePath, srcPkl,tgtPkl, pyMatchFunction, modelPath, dictPath};
				//			String[] paras = {stubSavePath, SrcPkl,TgtPkl, MATCH_METHOD, stubModelPath, stubDictPath};
			results = AsyncPythonHelper.callPython(matcherLocation,paras,LexicalProcessor.class);
			} else {
				String[] paras = {savePath, srcPkl,tgtPkl, pyMatchFunction};
				//String[] paras = {stubSavePath, SrcPkl,TgtPkl, MATCH_METHOD};
			results = AsyncPythonHelper.callPython(matcherLocation,paras,LexicalProcessor.class);
			}
			System.out.println("result: ");
			System.out.println(results[0].toString());
			System.out.println("err: ");
			System.out.println(results[1].toString());
			if(results[0].contains("success")){
				resultObj.put("success", true);
			} else {
				resultObj.put("error", results[1]);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return resultObj;
	}
	/***
	 *
	 * Query address of process pkl file for the ontology, which is stored in metadata
	 * @param IRI
	 * @return
	 */
	private String queryLexicalPklAddressfromMetaData(String IRI) {
		String addr = null;
		String queryStr = "PREFIX dcterms: <http://purl.org/dc/terms/> " 
				+"SELECT ?addr  " 
				+ "WHERE {"
				+ "?addr  dcterms:subject <" +IRI+">."
				+ "}";
		String resultStr = KnowledgeBaseClient.query(MetaDataAnnotator.getMetadataSetUrl(), null, queryStr);
	   try {
		   System.out.println(resultStr);
		JSONObject jo = new JSONObject(resultStr);
	      addr = jo.getJSONObject("results").getJSONArray("bindings").getJSONObject(0).getJSONObject("addr").getString("value");
	   }catch(Exception e) {
		   System.out.println(e.toString());
	   }
		System.out.println("get query result from metaDataStore: "+addr);
		return addr;	
	}

	/**
	 * get script path specific to matcher type
	 * @param matcherType
	 * @return
	 * @throws Exception
	 */
	private String getMatcherScriptName(MATCHERTYPE matcherType) throws Exception {
		switch(matcherType) {
		case STRING:
			return "stringMatcherCaller.py";
		case WORD:
			return "BOWMatcherCaller.py";
		case DOMAIN:
			return "domainMatcherCaller.py";	
		case VALUE:
			return "ValueMatcherCaller.py";	
		case I_STRING:
			return "InstanceStringMatcherCaller.py";	
		case I_WORD:
			return "InstanceBOWMatcherCaller.py";	
		default:
			throw new Exception("Matcher Type not Supported");
		} 	
	}
	
	private String getPyFunctionName(MATCHING_TYPE mt) throws Exception {
		switch(mt) {
		case TERM:
			return MATCH_TERM_FUNCTION_NAME;
		case INDIVIDUAL:
			return MATCH_INDIVIDUAL_FUNCTION_NAME;
		default:
			throw new Exception("Matching Type not Found");	
		}
	}
	
	//for testing
	protected void testGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doGet(request, response);
	}
}
