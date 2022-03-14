package informationQuery;

import de.derivo.sparqldlapi.Query;
import de.derivo.sparqldlapi.QueryArgument;
import de.derivo.sparqldlapi.QueryBinding;
import de.derivo.sparqldlapi.QueryEngine;
import de.derivo.sparqldlapi.QueryResult;
import de.derivo.sparqldlapi.exceptions.QueryEngineException;
import de.derivo.sparqldlapi.exceptions.QueryParserException;
import de.derivo.sparqldlapi.impl.QueryBindingImpl;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.InferenceType;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory;

public class InformationQuery {

	private static QueryEngine engine;
	
	public static String Query(String queryT) {
				
		String QueryTask = null;
		QueryTask = queryT;
		System.out.println(QueryTask);
		String result=null;
		
		try {
			// Create an ontology manager
			OWLOntologyManager manager = OWLManager.createOWLOntologyManager(); 

			// Load the wine ontology from the web.
            OWLOntology ont = manager.loadOntologyFromOntologyDocument(IRI.create("File:/C:/apache-tomcat-8.0.24/webapps/ROOT/BiodieselPlant-old.owl")); 

			// Create an instance of an OWL API reasoner (we use the OWL API built-in StructuralReasoner for the purpose of demonstration here)
            StructuralReasonerFactory factory = new StructuralReasonerFactory(); 
			OWLReasoner reasoner = factory.createReasoner(ont);
            // Optionally let the reasoner compute the most relevant inferences in advance
			reasoner.precomputeInferences(InferenceType.CLASS_ASSERTIONS,InferenceType.OBJECT_PROPERTY_ASSERTIONS);

			// Create an instance of the SPARQL-DL query engine
			engine = QueryEngine.create(manager, reasoner, true);
/**
                          // process raw material information -  
                     	processQuery(
                              "PREFIX process: <file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/CPS_function/process.owl#>\n" +
                              "PREFIX biodiesel: <http://www.jparksimulator.com/BiodieselPlant.owl#>\n" +
                              "PREFIX substance: <file:/C:/OntoCAPE/OntoCAPE/material/substance/substance.owl#>\n" +
				"SELECT ?x ?b WHERE {\n" +
				      "Type(?x, process:RawMaterial)" +  ","+   
                                      "PropertyValue(?x, ?y,?z)" +  ","+  
                                      "PropertyValue(?z, ?w,?t)" +  ","+  
                                      "PropertyValue(?t, ?v,?a)" +  ","+  
                                      "PropertyValue(?a, substance:molecularFormula,?b)" +
				"}"	
			); 
**/               
			
                     result=processQuery(
        						"PREFIX apparatus: <file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/CPS_realization/plant_equipment/apparatus.owl#>\n" +
        				        "PREFIX technical_system: <file:/C:/OntoCAPE/OntoCAPE/upper_level/technical_system.owl#>\n" +
        						"PREFIX geometry: <file:/C:/OntoCAPE/OntoCAPE/supporting_concepts/geometry/geometry.owl#>\n" +
        						"PREFIX space_time: <file:/C:/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time.owl#>\n" +
        						"PREFIX system: <file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#>\n" + 
        						"SELECT ?x ?b  WHERE {\n" +
        						   " Type(?x, apparatus:StirredTank)" + "," +
        						          "PropertyValue(?x, ?y, ?z)" + "," +
        						          "PropertyValue(?z, ?w, ?t)" + "," +
        						          
        						          "Type(?z, geometry:Volume)" + "," +						          
        						          "PropertyValue(?z, system:hasValue, ?v)" + "," +
        						          "Type(?v, system:QuantitativeValue)" + "," +	
        						          "PropertyValue(?v, system:numericalValue, ?b)" + 
//        						          "PropertyValue(?v, system:hasUnitOfMeasure, ?unit)" + 
                                       						          
//        						          "Type(?z, space_time:StraightCoordinate)" + "," +
//        						          "PropertyValue(?z, system:hasValue, ?v)" + "," +
//        						          "Type(?v, system:QuantitativeValue)" + "," +	
//        						          "PropertyValue(?v, system:numericalValue, ?cord)" +
                                       					          
        						"}"
        						);                  
                        
        }
        catch(UnsupportedOperationException exception) {
            System.out.println("Unsupported reasoner operation.");
        }
        catch(OWLOntologyCreationException e) {
            System.out.println("Could not load the ontology: " + e.getMessage());
        }
		return result;
	}
	
	public static String processQuery(String q){
		String[] val1=null;
        String val=null;
		try {
                    
			long startTime = System.currentTimeMillis();
			
			// Create a query object from it's string representation
			Query query = Query.create(q);
			
			System.out.println("Excecute the query:");
			System.out.println(q);
			System.out.println("-------------------------------------------------");
			
			// Execute the query and generate the result set
			QueryResult result = engine.execute(query);
                                                
			if(query.isAsk()) {
				System.out.print("Result: ");
				if(result.ask()) {
					System.out.println("yes");
				}
				else {
					System.out.println("no");
				}
			}
			else {
				if(!result.ask()) {
					System.out.println("Query has no solution.\n");
				}
				else {
					System.out.println("Results:");
					System.out.print(result);
                                        
                                        for (int i = 0; i < result.size(); i++) {
//                                            QueryBinding q = result.get(i);
                                                QueryBindingImpl binding = (QueryBindingImpl) result.get(i);
                                                Set<QueryArgument> argument = binding.getBoundArgs();
                                                for (QueryArgument a : argument) {
                                                    QueryArgument qa = binding.get(a);
                                                    String test = a.getValue();
                                                    
                                                    if (a.getValue().compareToIgnoreCase("x") == 0) {
                                                        val1 = qa.getValue().split("#");
                                                        System.out.println("The molecular formular of " + val1[1] + " is:");
                                                    }
                                                    if (a.getValue().compareToIgnoreCase("b") == 0) {
                                                        val = qa.getValue();
                                                        System.out.println(val);
                                                        
                                                    }
                                                }
                                                System.out.println();
                                        }
                                        
                                        
					System.out.println("-------------------------------------------------");
					System.out.println("Size of result set: " + result.size());
				}
			}

			System.out.println("-------------------------------------------------");
			System.out.println("Finished in " + (System.currentTimeMillis() - startTime) / 1000.0 + "s\n");
		}
        catch(QueryParserException e) {
        	System.out.println("Query parser error: " + e);
        }
        catch(QueryEngineException e) {
        	System.out.println("Query engine error: " + e);
        }
		return val;
	}
}
