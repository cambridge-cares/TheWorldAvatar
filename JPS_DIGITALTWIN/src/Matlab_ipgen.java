import java.util.List;

import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.sparql.engine.binding.Binding;

import java.io.File;  // Import the File class
import java.io.IOException;  // Import the IOException class to handle errors
import java.io.FileWriter; 
import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.PrintStream;
import java.net.URI;
import java.io.ByteArrayOutputStream;
import java.util.Arrays;
import java.util.List;

import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntModelSpec;
import com.hp.hpl.jena.query.*;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;

import com.hp.hpl.jena.rdf.model.impl.ResourceImpl;

//import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
//import uk.ac.cam.cares.jps.base.query.QueryBroker;
//import uk.ac.cam.cares.jps.base.util.MatrixConverter;

//import uk.ac.cam.cares.jps.powsys.nuclear.IriMapper;
//import uk.ac.cam.cares.jps.powsys.nuclear.QueryBroker;

public class Matlab_ipgen {
	
	public static void main(String[] args) {
		try {
		      File myObj = new File("filename.txt");
		      if (myObj.createNewFile()) {
		        System.out.println("File created: " + myObj.getName());
		      } else {
		        System.out.println("File already exists.");
		      }
		    } catch (IOException e) {
		      System.out.println("An error occurred.");
		      e.printStackTrace();
		    }
    

      
    	String filePath = "/Users/gourab/Downloads/EP_001.owl";
    	
    	OntModel model = ModelFactory.createOntologyModel(OntModelSpec.OWL_DL_MEM);
    	


		try {
		    File file = new File(filePath);
		    FileInputStream reader = new FileInputStream(file);
		    model.read(reader,null);     //load the ontology model
		} catch (Exception e) {
		    e.printStackTrace();
		}
    	
    	String sparqlQuery = "SELECT ?xap "
    			+ "WHERE {"
//    			+ "?s ?p ?o . "
//    			+ " ?x a <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#ElectricalPump> . "
    			+ " ?xap a <http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#MaximumActivePower> . "
//    			+ " ?vxap a <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue> .  "
//    			+ " ?x <http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasActivePowerAbsorbed> ?xap . "
//    			+ " ?xap <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> ?vxap . "
//    			+ " ?vxap <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericValue> ?aep . "
    			+ "}";
//   	System.out.println(sparqlQuery);
//    	System.err.println(sparqlQuery);
    	
    	Query query = QueryFactory.create(sparqlQuery);
    
    	QueryExecution qe = QueryExecutionFactory.create(query, model);
    	ResultSet results = qe.execSelect();
    	
    	while (results.hasNext()) {
//    		System.out.println(results.next().getResource("x"));
    		System.out.println(results.next().getResource("xap"));
    	}
    	
    	qe.close();
    	
    	
//    	for ( ; results.hasNext() ; )
//        {
//          QuerySolution soln = results.nextSolution() ;
//          RDFNode x = (RDFNode) soln.get("s") ;       // Get a result variable by name.
//          System.out.println(x.toString());
//          Resource r = soln.getResource("VarR") ; // Get a result variable - must be a resource
//          Literal l = soln.getLiteral("VarL") ;   // Get a result variable - must be a literal
//        }
		//ResultSetFormatter.out(System.out, results, query);			
//		ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();	
//		ResultSetFormatter.outputAsCSV(byteArrayOutputStream,results);
//		String s=byteArrayOutputStream.toString();
//		System.out.println(s);
		//List se=Arrays.asList(s.split("\\s*,\\s*|http"));
		//System.out.println(se);
//		String[] sa= s.split("\\r?\\n");
		//System.out.println(Arrays.toString(sa[1]));
//		System.out.println(sa[1]);
    	
    				
	}	
    	

}