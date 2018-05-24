package uk.ac.cam.cares.jps.discovery.knowledgebase;

import java.io.File;
import java.io.FileInputStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntModelSpec;
import com.hp.hpl.jena.query.Query;
import com.hp.hpl.jena.query.QueryExecution;
import com.hp.hpl.jena.query.QueryExecutionFactory;
import com.hp.hpl.jena.query.QueryFactory;
import com.hp.hpl.jena.query.ResultSet;
import com.hp.hpl.jena.query.ResultSetFactory;
import com.hp.hpl.jena.query.ResultSetFormatter;
import com.hp.hpl.jena.query.ResultSetRewindable;
import com.hp.hpl.jena.rdf.model.ModelFactory;

public class JenaHelper {
	
	private static Logger logger = LoggerFactory.getLogger(JenaHelper.class);
	
	/**
	 * If <code>path</code> denotes one OWL file, the file is imported to the model. If <code>path</code> denote a directory, 
	 * all OWL files of this directory are imported to the model. Other directories in this directory are ignored.
	 * 
	 * @param path
	 * @return
	 */
	public static OntModel createModel(String path) {
		// this class is a singleton. It is enough to read the ontology only once
		//logger.debug("createing Jena Model, path  = " + path);
		OntModel result = ModelFactory.createOntologyModel(OntModelSpec.OWL_DL_MEM);
		File file = new File(path);
		if (file.isFile()) {
			//logger.debug("Jena Model is importing file = " + file.getAbsolutePath());
			result.read(file.toURI().toString());
		} else {
			for (File current : file.listFiles()) {
				if (current.isFile()) {
					//logger.debug("Jena Model is importing file = " + file.getAbsolutePath());
					result.read(current.toURI().toString());
				}
			}
		}
		return result;
	}
	
	public static ResultSet sparql (String filepath, String query) {
		
		OntModel model = ModelFactory.createOntologyModel(OntModelSpec.OWL_DL_MEM);

		try {
		    File file = new File(filepath);
		    FileInputStream reader = new FileInputStream(file);
		    model.read(reader,null);     //load the ontology model
		} catch (Exception e) {
		    e.printStackTrace();
		}

		Query queryCreated = QueryFactory.create(query);
		QueryExecution qe = QueryExecutionFactory.create(queryCreated, model);
		
		//return qe.execSelect();
		
		ResultSet rs = qe.execSelect();                                    //the ResultSet can only be iterated once
		ResultSetRewindable results = ResultSetFactory.copyResults(rs);    //reset the cursor, so that the ResultSet can be repeatedly used
		ResultSetFormatter.out(System.out, results, queryCreated);
		return results;
	}
	
}
