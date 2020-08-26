package uk.ac.cam.cares.jps.ontomatch;

import java.util.ArrayList;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

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

	//Type of matchers
	public enum MATCHERTYPE {
	    DOMAIN, STRING,WORD, VALUE
	}
	private String MATCH_METHOD = "matchSerial";//parameters to py script specifying match method, this

	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		System.out.println("element matcher agent");
		JSONObject jo = requestParams;
		String savePath = null, matchMethod = null, targetOnto = null, sourceOnto = null,modelPath = null, dictPath = null;
        MATCHERTYPE type = null;
		//for testing, comment out later
		//MATCHERTYPE type = MATCHERTYPE.DOMAIN;
		//String stubSavePath = "file:///D:/workwork/testFiles/alignments/aStr3.owl";
		//String stubTgt = "D://workwork//testFiles//ontologies/PowerPlant.owl";
		//String stubSrc = "D://workwork//testFiles//ontologies/dbpedia_2014.owl";
		//String stubModelPath = "D:/workwork/ontoMatchData/simMatch/model/modellevel2/model30t5p5a.gensim";
		//String stubDictPath = "D:/workwork/ontoMatchData/simMatch/model/modellevel2/dictionarylevel2.gensim";
		
		//query for metadata: pkl address
		String TgtPkl = queryLexicalPklAddressfromMetaData(targetOnto);
		String SrcPkl = queryLexicalPklAddressfromMetaData(sourceOnto);		
	
		/***read params****/
		try {
			savePath = jo.getString("alignmentFileAddress");
			targetOnto = jo.getString("targetOntoIRI");
			sourceOnto = jo.getString("sourceOntoIRI");
			type = MATCHERTYPE.valueOf(jo.getString("matcherType"));
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
			String[] results = null;
			String matcherLocation = getMatcherScriptName(type);//get script name by matcher type and call
			if(type == MATCHERTYPE.DOMAIN) {//special case: domain matcher needs extra params
				String[] paras = {savePath, sourceOnto,targetOnto, MATCH_METHOD, modelPath, dictPath};
				//			String[] paras = {stubSavePath, SrcPkl,TgtPkl, MATCH_METHOD, stubModelPath, stubDictPath};
			results = AsyncPythonHelper.callPython(matcherLocation,paras,LexicalProcessor.class);
			} else {
				String[] paras = {savePath, sourceOnto,targetOnto, MATCH_METHOD};
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
			return "DomainMatcherCaller.py";	
		default:
			throw new Exception("Matcher Type not Supported");
		} 
		
		
	}
}
