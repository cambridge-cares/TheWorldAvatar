package uk.ac.cam.ceb.como.compchem.ontology.query;

import java.io.FileNotFoundException;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.ResultSetFormatter;

import org.apache.jena.rdf.model.ModelFactory;

import org.apache.jena.util.FileManager;

public class CompChemQuery {
	
	 public static final String TBOX_SOURCE = "./src/test/resources/ontology/compchem_ontology/compchem.spin.rdf";
	 public static final String ABOX_SOURCE = "./src/test/resources/ontology/compchem_abox/TiCl4.rdf";
	 
	 public static final String q = "PREFIX ontokin: <https://como.cheng.cam.ac.uk/kb/ontokin/>"
	 		+ "PREFIX gc: <http://purl.org/gc/>"
	 		+ "SELECT ?atomName ?atomNumber WHERE { "
	 		+ "?s ontokin:hasInitialization ?o . "
	 		+ "?o gc:hasMoleculeProperty ?mp . "
	 		+ "?mp gc:hasMolecule ?mol . "
	 		+ "?mol gc:hasNumberOfAtoms ?atomNumber."
	 		+ "?mol gc:hasAtom ?atom . "
	 		+ "?atom gc:isElement ?atomName . }";
	 
	public static void main(String[] args) throws FileNotFoundException {
		
		OntModel model = getOntModel(TBOX_SOURCE, ABOX_SOURCE);
		
		performQuery(model,q);

  }
	
	public static OntModel getOntModel(String tboxSource, String aboxSource) {
		
		OntModel model = ModelFactory.createOntologyModel(OntModelSpec .OWL_DL_MEM_TRANS_INF);
		
		FileManager.get().readModel( model, tboxSource);
		FileManager.get().readModel( model, aboxSource);
	
		return model;
	
	}
	
	public static void performQuery(OntModel model, String q) {		
		
         Query query = QueryFactory.create( q );
		
        QueryExecution qexec = QueryExecutionFactory.create( query, model );
        
        try {
            ResultSet results = qexec.execSelect();
            
            ResultSetFormatter.outputAsJSON(System.out, results);
     
        }
        
        finally {
        	
            qexec.close();
     }       
        
	}
}