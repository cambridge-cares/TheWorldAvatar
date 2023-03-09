/** This piece of code is write to execute query function for the RawMaterials in Biodiesel plant-1*/
package rawMaterialQuery;
//import all the necessary libraries
import de.derivo.sparqldlapi.Query;
import de.derivo.sparqldlapi.QueryArgument;
import de.derivo.sparqldlapi.QueryEngine;
import de.derivo.sparqldlapi.QueryResult;
import de.derivo.sparqldlapi.exceptions.QueryEngineException;
import de.derivo.sparqldlapi.exceptions.QueryParserException;
import de.derivo.sparqldlapi.impl.QueryBindingImpl;

import java.util.ArrayList;
import java.util.Set;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.InferenceType;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory;


public class RawMaterialQuery {
	
	private static QueryEngine engine;
	
	public static ArrayList<String> executeRawMaterialQuery () {
		ArrayList<String> result = null;
		try {
			// Create an ontology manager
			OWLOntologyManager manager = OWLManager.createOWLOntologyManager();

			// Load the biodiesel plant owl file.
//              OWLOntology BiodieselPlant1 = manager.loadOntologyFromOntologyDocument(IRI.create("File:/C:/apache-tomcat-8.0.24/webapps/ROOT/BiodieselPlant1WOWHR.owl"));  //try testing the ontology built by YiRen
              OWLOntology BiodieselPlant1 = manager.loadOntologyFromOntologyDocument(IRI.create("File:/C:/apache-tomcat-8.0.24/webapps/ROOT/BiodieselPlant.owl")); 

			// Create an instance of an OWL API reasoner (we use the OWL API built-in StructuralReasoner for the purpose of demonstration here)
            StructuralReasonerFactory factory = new StructuralReasonerFactory();
			OWLReasoner reasoner = factory.createReasoner(BiodieselPlant1);
            // Optionally let the reasoner compute the most relevant inferences in advance
			reasoner.precomputeInferences(InferenceType.CLASS_ASSERTIONS,InferenceType.OBJECT_PROPERTY_ASSERTIONS);

			// Create an instance of the SPARQL-DL query engine
			engine = QueryEngine.create(manager, reasoner, true);
                 	
			result = processQuery(
                              "PREFIX process: <file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/CPS_function/process.owl#>\n" +
                              "PREFIX behavior: <file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/CPS_behavior/behavior.owl#>\n" +
                              "PREFIX CPS: <file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/chemical_process_system.owl#>\n" +
                              "PREFIX biodiesel: <http://www.jparksimulator.com/BiodieselPlant.owl#>\n" +
                              "PREFIX system: <file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#>\n" +
                              "PREFIX substance: <file:/C:/OntoCAPE/OntoCAPE/material/substance/substance.owl#>\n" +
                              "PREFIX material: <file:/C:/OntoCAPE/OntoCAPE/material/material.owl#>\n" +
                              "PREFIX phaseSystem: <file:/C:/OntoCAPE/OntoCAPE/material/phase_system/phase_system.owl#>\n" +
				"SELECT ?x ?material ?ME ?MF ?F ?FnumericalValue ?FUnit ?T ?TnumericalValue ?TUnit WHERE {\n" +
				           "Type(?x, process:RawMaterial)" +  ","+   
                                      "PropertyValue(?x, CPS:refersToGeneralizedAmount,?b)" + ","+ 
                                      "PropertyValue(?b, system:hasSubsystem,?c)" + ","+ 
                                      "PropertyValue(?c, behavior:refersToMaterial,?material)" + ","+ 
                                      "PropertyValue(?material, material:intrinsicCharacteristics, ?S)" + ","+                                       
                                      "PropertyValue(?S, substance:hasMolecularStructure, ?ME)" + ","+ 
                                      "PropertyValue(?ME, ?molecularFormular, ?MF)" + ","+ 
                                      "PropertyValue(?c, system:hasProperty,?F)" + ","+ 
                                      "PropertyValue(?F, system:hasValue,?VF)" + ","+ 
                                      "PropertyValue(?VF, system:numericalValue,?FnumericalValue)" + ","+ 
                                      "PropertyValue(?VF, system:hasUnitOfMeasure,?FUnit)" + ","+ 
                                                                           
                                      "PropertyValue(?material, material:thermodynamicBehavior, ?TB)" + "," +
                                       "PropertyValue(?TB, phaseSystem:has_temperature,?T)" + ","+ 
                                      "PropertyValue(?T, system:hasValue,?TF)" + ","+ 
                                      "PropertyValue(?TF, system:numericalValue,?TnumericalValue)" + ","+ 
                                      "PropertyValue(?TF, system:hasUnitOfMeasure,?TUnit)" + 
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
	
public static ArrayList<String> processQuery(String q){
		
		ArrayList<String> Aresult = new ArrayList<String>();
        String Rresult = null;
		String[] val=null, val1=null, val2=null, val3=null, val4=null, val5=null, val6=null, val7=null, val8=null;
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
//					System.out.print(result);
                                        
                                        for (int i = 0; i < result.size(); i++) {
//                                            QueryBinding q = result.get(i);
                                                QueryBindingImpl binding = (QueryBindingImpl) result.get(i);
                                                Set<QueryArgument> argument = binding.getBoundArgs();
                                                for (QueryArgument a : argument) {
                                                    QueryArgument qa = binding.get(a);
                                                    String test = a.getValue();
                                                    
                                                    if (a.getValue().compareToIgnoreCase("x") == 0) {
                                                        val = qa.getValue().split("#");                                                           
                                                    }
                                                    
                                                    if (a.getValue().compareToIgnoreCase("ME") == 0) {
                                                        val1 = qa.getValue().split("#");                                                       
                                                    }
                                                    
                                                    if (a.getValue().compareToIgnoreCase("MF") == 0) {
                                                        val2 = qa.getValue().split("#");                                                       
                                                    }
                                                   
                                                    if (a.getValue().compareToIgnoreCase("F") == 0) {
                                                        val3 = qa.getValue().split("#");                                                        
                                                    }
                                                    
                                                    if (a.getValue().compareToIgnoreCase("FnumericalValue") == 0) {
                                                        val4 = qa.getValue().split("#");                                                        
                                                    }                                                    
                                                    if (a.getValue().compareToIgnoreCase("FUnit") == 0) {
                                                        val5 = qa.getValue().split("#");                                                        
                                                    }                                                   
                                                    
                                                    if (a.getValue().compareToIgnoreCase("T") == 0) {
                                                        val6 = qa.getValue().split("#");                                                        
                                                    }
                                                   
                                                    if (a.getValue().compareToIgnoreCase("TnumericalValue") == 0) {
                                                        val7 = qa.getValue().split("#");                                                       
                                                    }
                                                    
                                                    if (a.getValue().compareToIgnoreCase("TUnit") == 0) {
                                                        val8 = qa.getValue().split("#");                                                       
                                                    }
                                                }
                                                Rresult = ("The information we want for " + val[1] + " is " +  val1[1] + ": " + val2[0] + "   " + val3[1] + " = " + val4[0] + " " + val5[1] +"   " + val6[1] + "=" + val7[0] +" " +val8[1]);
                                                Aresult.add(Rresult);
                                                System.out.println(Rresult);
                                                
                                        }
                                        System.out.println(Aresult);
                                        
                                        
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
		return Aresult;
	}
}
