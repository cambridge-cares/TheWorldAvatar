package uk.ac.cam.cares.jps.discovery.knowledgebase;

import java.io.File;
import java.io.FileInputStream;

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

public class RDFHelper {

	
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
		ResultSet rs = qe.execSelect();                                    //the ResultSet can only be iterated once
		ResultSetRewindable results = ResultSetFactory.copyResults(rs);    //reset the cursor, so that the ResultSet can be repeatedly used
		
		//TODO-AE
		ResultSetFormatter.out(System.out, results, queryCreated);
				
		return results;
	}
	
	public static OntModel loadModel(String filepath) {
		OntModel model = ModelFactory.createOntologyModel(OntModelSpec.OWL_DL_MEM);

		try {
		    File file = new File(filepath);
		    FileInputStream reader = new FileInputStream(file);
		    model.read(reader,null);     
		} catch (Exception e) {
			//TODO-AE
		    e.printStackTrace();
		}
		
		return model;
	}
}
