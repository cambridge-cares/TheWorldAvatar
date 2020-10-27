package uk.ac.cam.cares.jps.base.query;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import org.apache.jena.query.Dataset;
import org.apache.jena.query.DatasetFactory;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.TxnType;
import org.apache.jena.rdfconnection.RDFConnection;
import org.apache.jena.rdfconnection.RDFConnectionFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;

public class FileBasedKnowledgeBaseClientRDFConnection {

	private String filePath;
	private Dataset dataset;
	private RDFConnection conn;
	
	// constructor
	public FileBasedKnowledgeBaseClientRDFConnection(String filePath) {
		
		this.filePath = filePath;
		dataset = DatasetFactory.create();
		conn = RDFConnectionFactory.connect(dataset);
		conn.load(this.filePath);
	}
	
	// close connection
	public void close() {
		conn.close();
	}
	
	// Sparql Update
	public void executeUpdate(String update) {
	
		conn.begin( TxnType.WRITE );
		try {
			conn.update( update );
			conn.commit();
		} finally {
			conn.end();
		}
	}

	//Sparql Select query
	public ResultSet executeQuery(String sparql) {
			
		conn.begin( TxnType.READ );	
		try {
			QueryExecution queryExec = conn.query(sparql);
			ResultSet results = queryExec.execSelect();
			return results;
		} finally {
			conn.end();
		}
	}
	
	// write to same file
	public void writeToFile() throws IOException {
		
		writeToFile(this.filePath);
	}
	
	// write to new file
	public void writeToFile(String filePath) throws IOException {
		
		try (OutputStream out = new FileOutputStream(filePath)){
			
			RDFDataMgr.write(out, dataset.getDefaultModel(), Lang.RDFXML); // check getdefaultmodel
			out.flush();
		}
	}
	
}
