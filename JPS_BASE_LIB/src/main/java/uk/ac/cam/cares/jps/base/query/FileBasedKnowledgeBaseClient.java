package uk.ac.cam.cares.jps.base.query;

import org.apache.jena.update.UpdateAction;
import org.apache.jena.update.UpdateRequest;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.ResultSetFactory;
import org.apache.jena.query.ResultSetRewindable;

import com.github.owlcs.ontapi.OntManagers;
import com.github.owlcs.ontapi.Ontology;
import com.github.owlcs.ontapi.OntologyManager;

public class FileBasedKnowledgeBaseClient {
	
	private String filePath;
	private Ontology ont;
	
	/*
	 * Default constructor
	 */
	public FileBasedKnowledgeBaseClient() {}
	
	/**
	 * Constructor with file path provided
	 * 
	 * @param filePath
	 */
	public FileBasedKnowledgeBaseClient(String filePath) {
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
	public void loadOntology() throws OWLOntologyCreationException {
		
		OntologyManager m = OntManagers.createONT();
		File file = new File(this.filePath);
		this.ont = m.loadOntologyFromOntologyDocument(file);	
	}
	
	/*
	 * Write file, note this writes to the same file
	 */
	public void writeOntology() throws OWLOntologyStorageException, IOException {
		
		OutputStream out = null;
		try {
			out = new FileOutputStream(filePath);
			ont.saveOntology(out);
			out.flush();
		} finally {
			out.close();
		}
	}
	
	/*
	 * Perform update 
	 */
	public void executeUpdate(String update) {
		UpdateAction.parseExecute(update, this.ont.asGraphModel());
	}
	
	// sparql update as UpdateRequest from sparql builder
	public void executeUpdate(UpdateRequest update) {
		UpdateAction.execute(update, this.ont.asGraphModel());
	}
	
	/*
	 * Perform a select query
	 */
	public ResultSet executeQuery(String sparql) {
		
		Query query = QueryFactory.create(sparql);
		return executeQuery(query);
	}
	
	public ResultSet executeQuery(Query query) {
		
		QueryExecution queryExec = QueryExecutionFactory.create(query, this.ont.asGraphModel());
		ResultSet rs = queryExec.execSelect();   
			
		//reset the cursor, so that the ResultSet can be repeatedly used
		ResultSetRewindable results = ResultSetFactory.copyResults(rs);    
		return results;
	}

}