package uk.ac.cam.cares.jps.base.query;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.charset.StandardCharsets;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.ResultSetFactory;
import org.apache.jena.query.ResultSetRewindable;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JenaReadHook;
import uk.ac.cam.cares.jps.base.scenario.ScenarioClient;

public class JenaHelper {

	/**
	 * {@see #read(String, OntModel)}
	 * 
	 * @param path
	 * @return
	 */
	public static OntModel createModel(String path) {

		if (path.startsWith("http")) {
			String scenarioUrl = BucketHelper.getScenarioUrl();
			if (!BucketHelper.isBaseScenario(scenarioUrl)) {
				path = new ScenarioClient().getReadUrl(scenarioUrl, path).toString();
			}
		}
			
		OntModel result = createModel();
		read(path, result);
		return result;
	}
	
	public static OntModel createModel() {
		JenaReadHook.prepareReadHook();
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
			if (owlFile.isFile() && (owlFile.getName().endsWith(".owl") || owlFile.getName().endsWith(".rdf"))) {
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

	public static ResultSet query(OntModel model, String sparql) {
		Query query = QueryFactory.create(sparql);
		QueryExecution queryExec = QueryExecutionFactory.create(query, model);
		ResultSet rs = queryExec.execSelect();   
		//reset the cursor, so that the ResultSet can be repeatedly used
		ResultSetRewindable results = ResultSetFactory.copyResults(rs);    
		//ResultSetFormatter.out(System.out, results, query);
		return results;
	}
	
	public static ResultSet queryFile(String file, String sparql) {
		OntModel model = createModel();
		readFromFile(new File(file), model);
		ResultSet resultSet = query(model, sparql);
		return resultSet;
	}
	
	public static ResultSet queryUrl(String url, String sparql) {
		OntModel model = createModel();
		try {
			readFromUrl(new URL(url), model);
		} catch (MalformedURLException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
		ResultSet resultSet = query(model, sparql);
		return resultSet;
	}
	
	public static ResultSet queryInputStream(InputStream is, String sparql) {
		OntModel model = createModel();
		read(is, model);
		ResultSet resultSet = query(model, sparql);
		return resultSet;
	}
	
	public static void writeAsFile(Model model, String path) {
		FileOutputStream fos;
		try {
			fos = new FileOutputStream(path);
		} catch (FileNotFoundException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
		model.write(fos, "RDF/XML-ABBREV");
	}
	
	public static String writeToString(Model model) {
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		model.write(os, "RDF/XML-ABBREV");
		return new String(os.toByteArray());
	}
}
