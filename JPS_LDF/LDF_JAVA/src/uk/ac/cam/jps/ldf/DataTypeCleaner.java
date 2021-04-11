package uk.ac.cam.jps.ldf;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.InputStream;

import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.ResIterator;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.vocabulary.RDFS;
import org.apache.jena.vocabulary.XSD;

 
 
public class DataTypeCleaner {

	public static void main(String[] args) throws FileNotFoundException {
		// TODO Auto-generated method stub
		System.out.println("Working Directory = " + System.getProperty("user.dir"));

		InputStream inputstream = new FileInputStream("D:\\ontocompchem\\final.ttl");
	 	final Model rdffile = ModelFactory.createDefaultModel();
		System.out.println("================ Start reading the file ====================");
		rdffile.read(inputstream, null, "Turtle") ;		
		StmtIterator statements = rdffile.listStatements();
		
		System.out.println("================ Done reading the file ====================");
		while (statements.hasNext()) {
			Statement the_statement = statements.next();
			// Property p = the_statement.getPredicate();
			
			Resource s = the_statement.getSubject();
			Resource p = the_statement.getPredicate();
			RDFNode o = the_statement.getObject();
			if (p.toString().contentEquals("http://purl.org/gc/hasName")) {
				
				
				String v = o.toString();
				v = v.replace(" ", "");
				// rdffile.remove(the_statement);
				Statement x = rdffile.createStatement(s, RDFS.label, v);
				rdffile.add(x);
				System.out.println(the_statement);
				System.out.println("=======================================");
			}
			
		    
 		}
		
		FileOutputStream test = new FileOutputStream("D:\\ontocompchem\\clean.ttl");
		rdffile.write(test, "Turtle");

	}

}
