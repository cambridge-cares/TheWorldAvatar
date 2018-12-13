package uk.ac.cam.cares.jps.srm.test;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.ResultSetFactory;
import org.apache.jena.query.ResultSetRewindable;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.json.JSONException;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

public class TestSRMAgent extends TestCase {
	
	private String getContextPathForJPSSRMAgent() {
		return "/JPS/SRMAgent";
	}
	
	public void testCallAgent () throws JSONException {
		JSONObject dataSet = new JSONObject();
		try {
			dataSet.put("reactionmechanism", "https://como.cheng.cam.ac.uk/kb/Toluene.owl#ReactionMechanism_4631074216281807") ;
			dataSet.put("engine", "http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#DieselEngine-001") ;
		}
		catch (JSONException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		
		String json = dataSet.toString();
		String resultjson = AgentCaller.executeGet(getContextPathForJPSSRMAgent(), "query", json);
		System.out.println ("resultjson= "+resultjson);
		
		String srmjsonfile = new JSONObject(resultjson).getString("file");
		System.out.println("corrected json file= "+srmjsonfile);
	}
	
	public static synchronized ResultSet queryFromOWLFile(String sparql, OntModel model) {
		Query query = QueryFactory.create(sparql);
		QueryExecution queryExec = QueryExecutionFactory.create(query, model);
		ResultSet rs = queryExec.execSelect();   
		//reset the cursor, so that the ResultSet can be repeatedly used
		ResultSetRewindable results = ResultSetFactory.copyResults(rs);    
		//ResultSetFormatter.out(System.out, results, query); ?? don't know why this is needed to be commented
		return results;
	}
	
	public void testqueryOWLFIle () {
		OntModel jenaOwlModel = null;
		
		String engineInfo = "PREFIX eng:<http://www.theworldavatar.com/ontology/ontoengine/OntoEngine.owl#> " 
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/chemical_process_system.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/material/material.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "SELECT ?OpMode "
				//+ "WHERE {?entity  a  eng:CompressionIgnitionEngine  ." 
				+ "WHERE {?entity   eng:numberOfCylinder ?No_cyl ."
				+ "?entity  a ?OpMode ."
				+"FILTER regex(STRBEFORE(STR(?OpMode),\"#\"), \"http://www.theworldavatar.com/ontology/ontoengine/OntoEngine.owl\") ."
				+ "}";
		
		
		
		jenaOwlModel = ModelFactory.createOntologyModel();	
		jenaOwlModel.read("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#DieselEngine-001", null);
		
	ResultSet rs_engine = TestSRMAgent.queryFromOWLFile(engineInfo,jenaOwlModel); 
	String valueiri=null;
	String valuetype=null;
	for (; rs_engine.hasNext();) {			
		QuerySolution qs_p = rs_engine.nextSolution();

		
			Resource cpiri = qs_p.getResource("OpMode"); // extract the name of the source
			valueiri = cpiri.toString();
			System.out.println("query = "+valueiri);
			if(valueiri.contains("http://www.theworldavatar.com/ontology/ontoengine/OntoEngine.owl#")&&!valueiri.contains("#Engine"))
			{
				valuetype=("CI");
				System.out.println("query result1= "+valuetype);
			}

	
			


		}
	}
	
}
