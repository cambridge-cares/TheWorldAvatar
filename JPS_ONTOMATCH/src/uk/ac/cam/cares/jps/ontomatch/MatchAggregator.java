package uk.ac.cam.cares.jps.ontomatch;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
/***
 * 

Agent that processes all alignment files produced by single metric matching to a combination result
Include functions to choose: weighted sum, filtering ,cardinality filtering, class type penalizing
Must start with weighted sum and end with filtering
Input: IRI of alignments
Output: IRI of new alignmnent
 * @author shaocong
 *
 */
import uk.ac.cam.cares.jps.base.util.AsyncPythonHelper;


public class MatchAggregator extends JPSHttpServlet{
	private List<List> matchScoreLists = new ArrayList<List>();
	private List<Map> finalScoreList = new ArrayList<Map>();
	private List<Integer> weight= new ArrayList<Integer>(); 
	private float threshold;
	private List<String> choices = new ArrayList<String>();
	private String classAlignmentIRI;
	int pFactor;
	int sameClassThreshold;

	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		System.out.println("Ontology processor agent");
		JSONObject jo = requestParams;
		try {
			//String saveAddress =  KeyValueMap.getInstance().get("targetOntology.pklfile");
		//get weights
		JSONArray jweight = jo.getJSONArray("weight");
		for (int i=0; i<jweight.length(); i++) {
			weight.add((Integer)jweight.get(i) );
		}
		//get alignment IRIs
		JSONArray jalignments = jo.getJSONArray("alignments");
	    List<String> alignmentIRIs = new ArrayList<String>(); 
		for (int i=0; i<jalignments.length(); i++) {
			String aIRI = (String)jalignments.get(i);
			getAlignmentList(aIRI);
		}
		JSONArray functionChoice = jo.getJSONArray("choices");
		for(int i = 0; i < functionChoice.length(); i++){
			choices.add(functionChoice.getString(i));
		}
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		JSONObject resultObj = new JSONObject();
		
		//reading choice params 
		try {
			classAlignmentIRI = jo.getString("classAlign");
			pFactor = jo.getInt("pFactor");
			sameClassThreshold = jo.getInt("sameClassThreshold");
		}catch(JSONException e) {
			//do nothing as these are optional params
		};
		try {
			handleChoice();
			//TODO:this should render a new alignment file,add this function to AlignmentHelper
			resultObj.put("success", true);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return resultObj;
	}
	
	    public void handleChoice() throws Exception {
	    	//TODO: check if params is null
	    	weighting();
	    	if(choices.contains("cardinality")){
	    		one2oneCardinalityFiltering();
	    	}
	    	if(choices.contains("penalizing")){
	    		penalizing();
	    	}
	    	filtering(threshold);
	    }
	
		public void getAlignmentList(String iriOfAlignmentFile) {
			String queryStr = "PREFIX alignment: <http://knowledgeweb.semanticweb.org/heterogeneity/alignment#> "
					+ "SELECT ?entity1 ?entity2 " 
					+ "WHERE {?cell alignment:entity1 ?entity1."
					+ "?cell  alignment:entity2 ?entity2 ."
					+"?cell alignment:measure ?measure."
					//+ "FILTER (?measure >= "+threshold +" ) " //filtering gen 001 as it is slackbus
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
			 matchScoreLists.add(resultListfromquery);
		}
		
//TODO: make alignment into a class is probably easier
	private void weighting() {		
		int matcherNum = matchScoreLists.size();
		for(int idxMatcher = 0 ; idxMatcher<matcherNum;idxMatcher++) {
			List aScoreList = matchScoreLists.get(idxMatcher);
			int myWeight  = weight.get(idxMatcher);
			int elementNum = matchScoreLists.get(0).size();
			for(int idxElement = 0; idxElement < elementNum; idxElement++) {
				Map myscore = (Map)aScoreList.get(idxElement);
				if(idxElement >= finalScoreList.size()){//index not exists, initiates
					 Map<String, Object> acell = new HashMap<>();
					 acell.put("entity1", myscore.get("entity1"));
					 acell.put("entity2", myscore.get("entity2"));
					 acell.put("measure", (int)myscore.get("measure")*myWeight);
					 finalScoreList.add(acell);
					}else{// index exists, update measure
                        Map mcell = finalScoreList.get(idxElement);                       
						 mcell.put("measure",(int)mcell.get("measure")+(int)myscore.get("measure")*myWeight);
					}	

			 }			
		}
	}
	
	
	private void filtering(float threshold) {//remove cellmaps with measure<threshold
	//loop thru cells to filter out based on measurement 
		int elementNum = finalScoreList.size();
		for(int idxElement = 0; idxElement < elementNum; idxElement++) {
            Map mcell = finalScoreList.get(idxElement);             
            if((int)mcell.get("measure") < threshold) {
            mcell.remove(mcell);
            }
		}		
	}
	
	private void one2oneCardinalityFiltering() throws IOException {
		//need to call the python for now
		//input: map rendered as json
		JSONArray scoreListNow = AlignmentHelper.scoreList2Json(finalScoreList);
		String[] paras = {scoreListNow.toString()};
		String[] results = AsyncPythonHelper.callPython("matchers/onetooneCardi.py",paras,MatchAggregator.class);	
		JSONArray scoreListNew = new JSONArray(results[0]);
		finalScoreList =  AlignmentHelper.Json2ScoreList(scoreListNew);
	}
	
	private void penalizing() throws Exception {
		//need to call the python for now
		JSONArray scoreListNow = AlignmentHelper.scoreList2Json(finalScoreList);
		JSONArray classAlignment = AlignmentHelper.readAlignmentFileAsJSONArray(classAlignmentIRI, "0");
		String[] paras = {scoreListNow.toString(), classAlignment.toString() };
		String[] results = AsyncPythonHelper.callPython("matchers/Penalizer.py",paras,OntologyProcessor.class);
		//TODO: what is the return type?
		//TODO: parse the return back to list
		JSONArray scoreListNew = new JSONArray(results[0]);
		finalScoreList =  AlignmentHelper.Json2ScoreList(scoreListNew);
	}
	


}

