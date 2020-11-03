package uk.ac.cam.cares.jps.base.query;

import org.apache.jena.update.UpdateAction;
import org.apache.jena.update.UpdateRequest;
import org.json.JSONArray;
import org.json.JSONObject;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.sql.SQLException;
import java.util.Iterator;

import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.ResultSetFactory;
import org.apache.jena.query.ResultSetRewindable;
import org.apache.jena.query.TxnType;

import com.github.owlcs.ontapi.OntManagers;
import com.github.owlcs.ontapi.Ontology;
import com.github.owlcs.ontapi.OntologyManager;

public class FileBasedKnowledgeBaseClientONTAPI extends KnowledgeBaseClient {
	
	private String filePath;
	private Ontology ont;
	
	/**
	 * Default constructor
	 */
	public FileBasedKnowledgeBaseClientONTAPI() {}
	
	/**
	 * Constructor with file path provided
	 * 
	 * @param filePath
	 */
	public FileBasedKnowledgeBaseClientONTAPI(String filePath) {
		this.filePath = filePath;
	}
	
	/*
	 * Load file
	 * 
	public void loadOntology(String filePath) {
		this.filePath = filePath;
		this.loadOntology();
	}
	 */
	
	/*
	 * Load file
	 */
	@Override
	public void load() {
		
		OntologyManager m = OntManagers.createONT();
		File file = new File(this.filePath);
		try {
			this.ont = m.loadOntologyFromOntologyDocument(file);
		} catch (OWLOntologyCreationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}		
	}
	
	/*
	 * Write file, note this writes to the same file
	 */
	@Override
	public void finish() {
		
		try (OutputStream out = new FileOutputStream(filePath)){
			ont.saveOntology(out);
			out.flush();
		}catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}catch (OWLOntologyStorageException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} 
	}
	
	/*
	 * Perform update 
	 */	
	// sparql update as UpdateRequest from sparql builder
//	public void executeUpdate(UpdateRequest update) {
//		UpdateAction.execute(update, this.ont.asGraphModel());
//	}
	@Override
	public int executeUpdate() {
		return executeUpdate(this.query);
	}
	
	@Override
	public int executeUpdate(String update) {
		
		UpdateAction.parseExecute(update, this.ont.asGraphModel());
		return 0; //return a useful integer?
	}
	
	/*
	 * Perform a select query
	 */
	private ResultSet performExecuteQuery(Query query) {
		
		QueryExecution queryExec = QueryExecutionFactory.create(query, this.ont.asGraphModel());
		ResultSet rs = queryExec.execSelect();   
			
		//reset the cursor, so that the ResultSet can be repeatedly used
		ResultSetRewindable results = ResultSetFactory.copyResults(rs);    
		return results;
	}

	@Override
	public JSONArray executeQuery(String sparql) {
		
		Query query = QueryFactory.create(sparql);
		return convert(performExecuteQuery(query));
	}	
	
	@Override
	public JSONArray executeQuery() {
		return executeQuery(this.query);
	}
	
	private JSONArray convert(ResultSet resultSet) {
		
		JSONArray json = new JSONArray();
		
		while (resultSet.hasNext()) {
			QuerySolution qs = resultSet.next();
			JSONObject obj = new JSONObject();
			Iterator<String> it = qs.varNames(); 
			while(it.hasNext()) {
				String var = it.next(); 
				obj.put(var, qs.get(var));
			}
			json.put(obj);
		}
		return json;
	}
}