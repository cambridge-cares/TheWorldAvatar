package uk.ac.cam.cares.jps.ontomatch;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.annotation.WebServlet;
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


/***
 * Aggregates element level alignment result(tmp alignment owl files).
 * Contains several function to be composed by choice.
 * Input: alignment owl IRIs.
 * Output: result alignment owl IRI.
 */
@WebServlet(urlPatterns = { "/matchAggregator" })

public class MatchAggregator extends JPSHttpServlet{
	/**
	 * 
	 */
	private static final long serialVersionUID = -1142445270131640156L;
	private String srcOnto, tgtOnto;
	private String thisAlignmentIRI;
	private List<List> matchScoreLists = new ArrayList<List>();
	private List<Map> finalScoreList = new ArrayList<Map>();
	private List<Double> weights= new ArrayList<Double>(); 
	private double threshold;
	private List<AGGREGATE_CHOICE> choices = new ArrayList<AGGREGATE_CHOICE>();
	private String classAlignmentIRI;
	private double pFactor,sameClassThreshold;
	

    public enum AGGREGATE_CHOICE {
        PENALIZING, CARDINALITY
	}
	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		System.out.println("Ontology processor agent");
		//TODO: for testing, comment out later
		threshold = 0.6;
		weights.add(0.4);weights.add(0.4);weights.add(0.2);		
		String onto1 = "D://workwork//testFiles//ontologies/dbpedia_2014.owl",onto2 = "D://workwork//testFiles//ontologies/PowerPlant.owl";
		String[] stubAlignments = {"file:///D:/workwork/testFiles/alignments/aStr.owl","file:///D:/workwork/testFiles/alignments/aStr2.owl","file:///D:/workwork/testFiles/alignments/aStr3.owl"}; 
		for (int i=0; i<stubAlignments.length; i++) {
			String aIRI = stubAlignments[i];
			getAlignmentList(aIRI);
		}
		String addr = "file:///D:/workwork/testFiles/alignments/final.owl";
		JSONObject jo = requestParams;
		/***
		try {
			threshold = jo.getFloat("threshold");
			srcOnto = jo.getString("srcOnto");
			tgtOnto = jo.getString("tgtOnto");
			thisAlignmentIRI = jo.getString("addr");

			//get weights
			JSONArray jweight = jo.getJSONArray("weights");
		for (int i=0; i<jweight.length(); i++) {
			weight.add(jweight.getInt(i) );
		}

		JSONArray jalignments = jo.getJSONArray("alignments");
	    List<String> alignmentIRIs = new ArrayList<String>(); 
		for (int i=0; i<jalignments.length(); i++) {
			String aIRI = jalignments.getString(i);
			getAlignmentList(aIRI);
		}
		JSONArray functionChoice = jo.getJSONArray("choices");
		for(int i = 0; i < functionChoice.length(); i++){
			choices.add(AGGREGATE_CHOICE.valueOf(functionChoice.getString(i)));
		}
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		**/
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
			AlignmentIOHelper.writeAlignment2File(finalScoreList, onto1, onto2, addr);
			//TODO:this should render a new alignment file,add this function to AlignmentHelper
			resultObj.put("success", true);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return resultObj;
	}
	
	    public void handleChoice() throws Exception {
	    	weighting();
	    	if(choices.contains(AGGREGATE_CHOICE.CARDINALITY)){
	    		one2oneCardinalityFiltering();
	    	}
	    	if(choices.contains(AGGREGATE_CHOICE.PENALIZING)){
	    		penalizing();
	    	}
	    	filtering(threshold);
	    }
	
		public void getAlignmentList(String iriOfAlignmentFile) {
			String queryStr = "PREFIX alignment: <http://knowledgeweb.semanticweb.org/heterogeneity/alignment#> "
					+ "SELECT ?entity1 ?entity2 ?measure " 
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
		
	private void weighting() {		
		int matcherNum = matchScoreLists.size();
		for(int idxMatcher = 0 ; idxMatcher<matcherNum;idxMatcher++) {
			List aScoreList = matchScoreLists.get(idxMatcher);
			double myWeight  = weights.get(idxMatcher);
			int elementNum = matchScoreLists.get(0).size();
			for(int idxElement = 0; idxElement < elementNum; idxElement++) {
				String[] myscore = (String[]) aScoreList.get(idxElement);
				if(idxElement >= finalScoreList.size()){//index not exists, initiates
					 Map<String, Object> acell = new HashMap<>();
					 acell.put("entity1", myscore[0]);
					 acell.put("entity2", myscore[1]);
					 acell.put("measure", Double.parseDouble(myscore[2])*myWeight);
					 finalScoreList.add(acell);
					}else{// index exists, update measure
                        Map mcell = finalScoreList.get(idxElement);                       
						 mcell.put("measure",(double)mcell.get("measure")+Double.parseDouble(myscore[2])*myWeight);
					}	

			 }			
		}
	}
	
	
	private void filtering(double threshold2) {//remove cellmaps with measure<threshold
	//loop thru cells to filter out based on measurement 
		int elementNum = finalScoreList.size();
		for(int idxElement = 0; idxElement < elementNum; idxElement++) {
            Map mcell = finalScoreList.get(idxElement);             
            if((double)mcell.get("measure") - threshold2 <0) {
            mcell.remove(mcell);
            }
		}		
	}
	
	private void one2oneCardinalityFiltering() throws IOException {
		//need to call the python for now
		//input: map rendered as json
		JSONArray scoreListNow = AlignmentIOHelper.scoreList2Json(finalScoreList);
		String[] paras = {scoreListNow.toString()};
		String[] results = AsyncPythonHelper.callPython("matchers/onetooneCardi.py",paras,MatchAggregator.class);	
		JSONArray scoreListNew = new JSONArray(results[0]);
		finalScoreList =  AlignmentIOHelper.Json2ScoreList(scoreListNew);
	}
	
	private void penalizing() throws Exception {
		//need to call the python for now
		JSONArray scoreListNow = AlignmentIOHelper.scoreList2Json(finalScoreList);
		JSONArray classAlignment = AlignmentIOHelper.readAlignmentFileAsJSONArray(classAlignmentIRI, 0.0);
		String[] paras = {scoreListNow.toString(), classAlignment.toString() };
		String[] results = AsyncPythonHelper.callPython("matchers/Penalizer.py",paras,LexicalProcessor.class);
		//TODO: what is the return type?
		//TODO: parse the return back to list
		JSONArray scoreListNew = new JSONArray(results[0]);
		finalScoreList =  AlignmentIOHelper.Json2ScoreList(scoreListNew);
	}
	
}

