package valuechanger;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URISyntaxException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.UUID;

import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.ObjectProperty;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.ResultSetFactory;
import org.apache.jena.query.ResultSetRewindable;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;



public class Valuechanger {
	
	static Individual xvalue;
	static Individual yvalue;
	static Individual busnum;
	
	OntModel jenaOwlModel = null;
	
	static Individual co2value;
	private DatatypeProperty numericalvalue = null;
	//public static String baseURL2 = "D:\\KBDev-git/irp3-JPS-KBDev-git/Server Ontology Configuration Root/kb/sgp/jurongisland/jurongislandpowernetwork/";
	public static String baseURL2 = "D:\\KBDev-git/irp3-JPS-KBDev-git/Server Ontology Configuration Root/kb/powerplants/";
	
	

	public void savefile(OntModel jenaOwlModel, String filePath2) throws URISyntaxException, FileNotFoundException {

		FileOutputStream out = new FileOutputStream(filePath2);
		
		Collection errors = new ArrayList();
		jenaOwlModel.write(out, "RDF/XML-ABBREV");

		
		System.out.println("File saved with " + errors.size() + " errors.");
	}
	
	
	
	public void initOWLClasses(OntModel jenaOwlModel) {
		
		numericalvalue = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
		


		
	}
	
	public static synchronized ResultSet query(String sparql, OntModel model) {
		Query query = QueryFactory.create(sparql);
		QueryExecution queryExec = QueryExecutionFactory.create(query, model);
		ResultSet rs = queryExec.execSelect();   
		//reset the cursor, so that the ResultSet can be repeatedly used
		ResultSetRewindable results = ResultSetFactory.copyResults(rs);    
		//ResultSetFormatter.out(System.out, results, query); ?? don't know why this is needed to be commented
		return results;
	}
	
	public Double queryPowerplantProperty(String iri) throws Exception {
		

		
		String value1 = null;
		String capacity = null;
		Double outputvalue;
		String plantInfo = "PREFIX cp:<http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#> " 
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#> "
				+ "SELECT ?generation ?vcapa "
				+ "WHERE {?entity  a  cp:PowerPlant  ." 
				+ "?entity   j3:realizes ?generation ."
				+ "?entity   j4:designCapacity ?capa  ."
				+ "?capa   j2:hasValue ?valuecapa ."
				+ "?valuecapa   j2:numericalValue ?vcapa ." 
				+ "}";

		ResultSet rs_plant = Valuechanger.query(plantInfo,jenaOwlModel); 
		
		for (; rs_plant.hasNext();) {			
			QuerySolution qs_p = rs_plant.nextSolution();

			Resource cpiri = qs_p.getResource("generation");
			value1 = cpiri.toString();
			Literal cap = qs_p.getLiteral("vcapa"); // extract the name of the source
			capacity = cap.getString();
	
		}

		if (value1.contentEquals(
				"http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#CoalGeneration")) {
			outputvalue = Double.valueOf(capacity) * 1000 * 0.8 * 0.001;
		} else if (value1.contentEquals(
				"http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#OilGeneration")) {
			outputvalue = Double.valueOf(capacity) * 1000 * 0.5 * 0.001;
		} else if (value1.contentEquals(
				"http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGasGeneration")) {
			outputvalue = Double.valueOf(capacity) * 750 * 0.5 * 0.001;
		} else {
			throw new Exception("unknown generation type: " + value1);
		}
		return outputvalue;
	}
	
	
	public void startConversion() throws Exception {
		String myDirectoryPath ="D:/KBDev-git/irp3-JPS-KBDev-git/Server Ontology Configuration Root/kb/powerplants";
		File dir = new File(myDirectoryPath);
		  File[] directoryListing = dir.listFiles();
		
		//===================================================
		  if (directoryListing != null) {
			    for (File child : directoryListing) {
			    	
			    	//Path path = Paths.get(child.getPath());
			    	//Path path2 = Paths.get("D:/JParkSimulator-git-dev-database/kbirichanger/outcome/"+child.getName());
			    	//Path path2 = Paths.get("D:/KBDev-git/irp3-JPS-KBDev-git/Server Ontology Configuration Root/kb/powerplants/"+child.getName());
			    	//Path path2 = Paths.get("D:/tmp/newpowerplants/"+child.getName());
			    	String filePath2 = "D:\\tmp/newpowerplants2/" + child.getName(); // the result of written owl file
			    	//System.out.println("what is path taken= "+child.getPath());

			    	 
			    	
			    	
			   
			    
//					jenaOwlModel = ModelFactory.createOntologyModel();	
//					jenaOwlModel.read(child.getPath(), null);
					
					
					FileInputStream inFile = new FileInputStream(child.getPath());
	    			Reader in = new InputStreamReader(inFile, "UTF-8");
	    				    			
	    			jenaOwlModel = ModelFactory.createOntologyModel();
	    			jenaOwlModel.read(in, null);

	    			initOWLClasses(jenaOwlModel);
	    			System.out.println ("childpath= "+child.getPath());
	    			Double valueco2emission=queryPowerplantProperty(child.getPath());
	    			System.out.println ("co2= "+valueco2emission);
    				doConversion(jenaOwlModel,child.getName(),String.valueOf(valueco2emission));	
    				savefile(jenaOwlModel, filePath2);
			    
			    }
		  
		  
		  //=======================================================
		
		
		  }

	
	}
	
	
	public void doConversion(OntModel jenaOwlModel, String plantname ,String co2emission){
		
		//xvalue=jenaOwlModel.getIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/"+busname+".owl#V_x_"+busname);
		//yvalue=jenaOwlModel.getIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/"+busname+".owl#V_y_"+busname);
		//busnum=jenaOwlModel.getIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/"+busname+".owl#V_BusNumber_"+busname);
		System.out.println("plantname= "+plantname);
		System.out.println("statusemission= "+co2emission);
		System.out.println("status= "+"http://www.theworldavatar.com/kb/powerplants/"+plantname+"#v_CO2Emission_of_"+plantname.split(".owl")[0]);
		co2value=jenaOwlModel.getIndividual("http://www.theworldavatar.com/kb/powerplants/"+plantname+"#v_CO2Emission_of_"+plantname.split(".owl")[0]);
		
//		xvalue.setPropertyValue(numericalvalue, jenaOwlModel.createTypedLiteral(x));
//		yvalue.setPropertyValue(numericalvalue, jenaOwlModel.createTypedLiteral(y));
//		busnum.setPropertyValue(numericalvalue, jenaOwlModel.createTypedLiteral(Integer.parseInt(busname.split("-")[1])));
		co2value.setPropertyValue(numericalvalue, jenaOwlModel.createTypedLiteral(co2emission));
								
		}
	
	public static void main(String[] args) throws Exception {
		System.out.println("Starting Process");
		Valuechanger converter = new Valuechanger();
		converter.startConversion();

	}
}

