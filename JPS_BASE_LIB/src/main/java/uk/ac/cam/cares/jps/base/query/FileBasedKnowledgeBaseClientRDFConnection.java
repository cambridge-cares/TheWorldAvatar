package uk.ac.cam.cares.jps.base.query;

import java.io.ByteArrayOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.sql.SQLException;
import java.util.Iterator;
import java.util.List;

import org.apache.jena.query.Dataset;
import org.apache.jena.query.DatasetFactory;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.ResultSetFormatter;
import org.apache.jena.query.TxnType;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdfconnection.RDFConnection;
import org.apache.jena.rdfconnection.RDFConnectionFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.interfaces.KnowledgeBaseClientInterface;

public class FileBasedKnowledgeBaseClientRDFConnection extends KnowledgeBaseClient implements KnowledgeBaseClientInterface{

	private Dataset dataset;
	private RDFConnection conn;
	private String filePath;
	
	//TODO: check connection active before performing process; default constructor and initialise post-construction
	
	// constructor
	public FileBasedKnowledgeBaseClientRDFConnection(String filePath) {
		
		this.filePath = filePath;
		init();
	}
		
	// constructor
	public FileBasedKnowledgeBaseClientRDFConnection(String filePath, String sparql) {
		
		super(sparql); 
		this.filePath = filePath;
		init();
	}
	
	// initialise connection
	private void init() {
		
		dataset = DatasetFactory.create();
		conn = RDFConnectionFactory.connect(dataset);
		conn.load(filePath);
	}
	
	//
	public void load(String filePath) {
		this.filePath = filePath;
		init();
	}
	
	// close connection
	public void close() throws IOException {	//cannot be closed unless in base class
		
		writeToFile();
		conn.close();
	}
	
	public int executeUpdate() {
		return executeUpdate(this.query);
	}
	
	public int executeUpdate(String update) {
		
		conn.begin( TxnType.WRITE );
		try {
			conn.update( update );
			conn.commit();
		} finally {
			conn.end();
		}
		return 0; //return a useful integer?
	}

	public JSONArray executeQuery(String sparql) {
		ResultSet results = perfromExecuteQuery(sparql);
		return convert(results);
	}	
	
	public JSONArray executeQuery() {
		return executeQuery(this.query);
	}
			
	private ResultSet perfromExecuteQuery(String sparql) {
		conn.begin( TxnType.READ );	
		try {
			QueryExecution queryExec = conn.query(sparql);
			ResultSet results = queryExec.execSelect();
			return results;
		} finally {
			conn.end();
		}
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

	// write to same file
	// put this at the end of update or in close?
	public void writeToFile() throws IOException {
		
		writeToFile(this.filePath);
	}
	
	// write to new file
	public void writeToFile(String filePath) throws IOException {
		
		try (OutputStream out = new FileOutputStream(filePath)){
			
			RDFDataMgr.write(out, dataset.getDefaultModel(), Lang.RDFXML); // check getdefaultmodel
			//RDFDataMgr.write(out, dataset.getDefaultModel(), Lang.NQ); // check getdefaultmodel
			out.flush();
		}
	}
}
