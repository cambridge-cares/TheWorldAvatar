package uk.ac.cam.cares.jps.discovery.knowledgebase;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.query.Query;
import com.hp.hpl.jena.query.QueryExecution;
import com.hp.hpl.jena.query.QueryExecutionFactory;
import com.hp.hpl.jena.query.QueryFactory;
import com.hp.hpl.jena.query.ResultSet;
import com.hp.hpl.jena.query.ResultSetFactory;
import com.hp.hpl.jena.query.ResultSetFormatter;
import com.hp.hpl.jena.query.ResultSetRewindable;

import uk.ac.cam.cares.jps.base.config.AgentLocator;

public class AgentKnowledgeBase {
	
	public static final String ONTOAGENT_BASE_IRI = "http://www.theworldavatar.com/OntoAgent";
	public static final String ONTOAGENT_ONTOLOGY_IRI = ONTOAGENT_BASE_IRI + "/OntoAgent.owl";
	
	private static AgentKnowledgeBase instance = null;

	private Logger logger = LoggerFactory.getLogger(AgentKnowledgeBase.class);
	private OntModel knowledgebase = null;
	
	private AgentKnowledgeBase () {
	}
	
	public static synchronized AgentKnowledgeBase getInstance() {
		if (instance == null) {
			return getInstance(null);
		}
		return instance;
	}
	
	public static synchronized AgentKnowledgeBase getInstance(String dirForAgentKnowledgesBase) {
		if (instance == null) {
			instance = new AgentKnowledgeBase();
			instance.init(dirForAgentKnowledgesBase);
			instance.logger.info("AgentKnowledgeBase was created");
		}
		return instance;
	}
	
	public static String getFileForAgentOntology() {
		return uk.ac.cam.cares.jps.base.config.AgentLocator.getPathToJpsDataOntologyDir() + "/OntoAgent/OntoAgent.owl";
	}
	
	public static String getDirForAgentKnowledgesBase() {
		return AgentLocator.getPathToJpsDataKnowledgeDir() + "/OntoAgent";
	}
	
	protected String getDirForAgentKnowledgesBaseInternally() {
		return getDirForAgentKnowledgesBase();
	}
	
	protected void init(String dirForAgentKnowledgesBase) {
		String dir = dirForAgentKnowledgesBase;
		if (dir == null) {
			dir = getDirForAgentKnowledgesBaseInternally();
		}	
		knowledgebase = JenaHelper.createModel(dir);
	}
	
	public static ResultSet query(String sparql) {
		Query query = QueryFactory.create(sparql);
		OntModel model = getInstance().knowledgebase;
		QueryExecution queryExec = QueryExecutionFactory.create(query, model);
		ResultSet rs = queryExec.execSelect();   
		ResultSetRewindable results = ResultSetFactory.copyResults(rs);    //reset the cursor, so that the ResultSet can be repeatedly used
		ResultSetFormatter.out(System.out, results, query);
		return results;
	}
}
