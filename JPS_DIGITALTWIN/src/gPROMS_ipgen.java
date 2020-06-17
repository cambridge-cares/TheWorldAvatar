
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
import com.hp.hpl.jena.rdf.model.ModelFactory;


public class gPROMS_ipgen {
	

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
//Extracting required variables from owl files
		   String filePath = "/Users/aravindd/Downloads/debutaniser_section.owl";

			OntModel model = ModelFactory.createOntologyModel(OntModelSpec.OWL_DL_MEM);

			try {
			    File file = new File(filePath);
			    FileInputStream reader = new FileInputStream(file);
			    model.read(reader,null);     //load the ontology model
			} catch (Exception e) {
			    e.printStackTrace();
			}
// Sample Sparql query
//			String sparqlQuery =
//					"SELECT ?Temp\n"+
//					"WHERE {\n "+
//					"?x a <http://www.semanticweb.org/caresadmin1/ontologies/2020/5/untitled-ontology-396#Temperature> .\n"+	
//					"?x  <http://www.semanticweb.org/caresadmin1/ontologies/2020/5/untitled-ontology-396#hasValue>  ?Temp .\n"+
//					"}" ;
//			//System.err.println(sparqlQuery); //Prints the query
//			Query query = QueryFactory.create(sparqlQuery);
//
//			QueryExecution qe = QueryExecutionFactory.create(query, model);
//
//			ResultSet results = qe.execSelect();
//			//ResultSetFormatter.out(System.out, results, query);			
//			ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();	
//			ResultSetFormatter.outputAsCSV(byteArrayOutputStream,results);
//			String s=byteArrayOutputStream.toString();
//			System.out.println(s);
//			//List se=Arrays.asList(s.split("\\s*,\\s*|http"));
//			//System.out.println(se);
//			String[] sa= s.split("\\r?\\n");
//			//System.out.println(Arrays.toString(sa[1]));
//			System.out.println(sa[1]);

//Trial one with the debutaniser file
			
			String TempQuery =
					"PREFIX process:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#>\r\n" +
					"PREFIX system:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>\r\n" +
					"\r \n"+
					"SELECT ?Temp\n"+
					"WHERE {\n "+
					"?x a  system:ScalarValue  .\n" + 
					"?x system:value ?Temp .\n"+
					"}" ;
			//System.err.println(TempQuery); //Prints the query
			Query queryt = QueryFactory.create(TempQuery);

			QueryExecution qet = QueryExecutionFactory.create(queryt, model);

			ResultSet results = qet.execSelect();
			//ResultSetFormatter.out(System.out, results, query);			
			ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();	
			ResultSetFormatter.outputAsCSV(byteArrayOutputStream,results);
			String s=byteArrayOutputStream.toString();
			System.out.println(s);
			//List se=Arrays.asList(s.split("\\s*,\\s*|http"));
			//System.out.println(se);
			String[] sa= s.split("\\r?\\n");
			//System.out.println(Arrays.toString(sa[1]));
			System.out.println(sa[1]);			
			
//Once the file is created, data has to be written to it
		  try {
		      FileWriter myWriter = new FileWriter("Settings.input");
		      myWriter.write("Feed__T \n");
		      myWriter.write(sa[1]);
		      myWriter.write("\n");
		      myWriter.write("Feed__P\n");
		      myWriter.write(sa[2]);
		      myWriter.write("\nstep1__initial_value \n");
		      myWriter.write("step1__final_value \n");
		      myWriter.write("step2__initial_value \n");
		      myWriter.write("step2__final_value \n");
		      myWriter.write("step3__initial_value \n");
		      myWriter.write("step3__final_value \n");
		      myWriter.write("step4__initial_value \n");
		      myWriter.write("step4__final_value \n");
		      
		      myWriter.close();
		      System.out.println("Successfully wrote to the file.");
		    } catch (IOException e) {
		      System.out.println("An error occurred.");
		      e.printStackTrace();
		    }
	}
}

