package uk.ac.cam.cares.jps.base.query;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import org.apache.jena.query.Dataset;
import org.apache.jena.query.DatasetFactory;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdfconnection.RDFConnection;
import org.apache.jena.rdfconnection.RDFConnectionFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.system.StreamRDFWriter;

public class FileBasedKnowledgeBaseClientStreamRDF {

	private StreamRDFToConnection stream;
	private RDFConnection conn;
	private Dataset dataset;
	private String filePath;
	
	// constructor
	public FileBasedKnowledgeBaseClientStreamRDF(String filePath) {
		
		this.filePath = filePath;
		this.dataset = DatasetFactory.create();
		this.conn = RDFConnectionFactory.connect(this.dataset);
		this.stream = new StreamRDFToConnection(this.conn);
		RDFDataMgr.parse(stream, filePath);
	}
	
	public void executeUpdate(String update) {
		// stream.start(); // start does nothing
		
		stream.finish();	//flush stream to connection before updating
		conn.update(update);
	}
	
	//Sparql query
	public ResultSet executeQuery(String sparql) {
		
		//TODO : use transactions
		
		QueryExecution queryExec = this.conn.query(sparql);
		ResultSet results = queryExec.execSelect();
		return results;
	}
	
	// write
	public void writeToFile() throws FileNotFoundException, IOException {
		 
		try (OutputStream out = new FileOutputStream(filePath)){
			StreamRDFWriter.write(out, dataset.asDatasetGraph(), Lang.NQ); // cannot write stream to RDFXML
		}
	}	
}
