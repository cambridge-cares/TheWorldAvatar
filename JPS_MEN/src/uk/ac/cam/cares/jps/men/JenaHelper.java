package uk.ac.cam.cares.jps.men;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.charset.StandardCharsets;

import org.apache.jena.base.Sys;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.ResultSetFactory;
import org.apache.jena.query.ResultSetFormatter;
import org.apache.jena.query.ResultSetRewindable;
import org.apache.jena.rdf.model.ModelFactory;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class JenaHelper {

	/**
	 * {@see #read(String, OntModel)}
	 * 
	 * @param path
	 * @return
	 */
	public static OntModel createModel(String path) {
		OntModel result = createModel();
		read(path, result);
		return result;
	}
	
	public static OntModel createModel() {
		return ModelFactory.createOntologyModel(OntModelSpec.OWL_DL_MEM);
	}

	/**
	 * If <code>path</code> denotes an URL, the OWL file will be read from the URL and imported to the model.
	 * If <code>path</code> denotes one OWL file, the file is imported to the model.
	 * If <code>path</code> denotes a directory, all OWL files of this directory are
	 * imported to the model. Other directories in this directory are ignored.
	 * 
	 * @param path
	 * @return
	 */
	public static void read(String path, OntModel model) {

		try {
			URL url = new URL(path);
			readFromUrl(url, model);
			return;
		} catch (MalformedURLException e) {
			// nothing to do here
		}
		
		File file = new File(path);
		if (file.isFile()) {
			readFromFile(file, model);
		} else {
			for (File current : file.listFiles()) {
				readFromFile(current, model);
			}
		}
	}
	
	public static void readFromUrl(URL url, OntModel model) {
		try {
			InputStream is = url.openStream();
			read(is, model);
		} catch (IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
		
	}
	
	public static void readFromFile(File owlFile, OntModel model) {
		try {
			if (owlFile.isFile() && owlFile.getName().endsWith(".owl")) {
				InputStream is = new FileInputStream(owlFile);
				read(is, model);
			}
		} catch (IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}	
	}
	
	public static void readFromString(String s, OntModel model) {
		InputStream is = new ByteArrayInputStream( s.getBytes(StandardCharsets.UTF_8) );
		read(is, model);
	}

	public static void read(InputStream is, OntModel model) {
		try {
			model.read(is, null);
			is.close();
		} catch (IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}

	public static synchronized ResultSet query(String sparql, OntModel model) {
		Query query = QueryFactory.create(sparql);
		QueryExecution queryExec = QueryExecutionFactory.create(query, model);
		ResultSet rs = queryExec.execSelect();   
		//reset the cursor, so that the ResultSet can be repeatedly used
		ResultSetRewindable results = ResultSetFactory.copyResults(rs);    
		return results;
	}
}
