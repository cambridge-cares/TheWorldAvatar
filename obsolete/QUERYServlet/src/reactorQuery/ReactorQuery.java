/** This piece of code is write to execute query function for the RawMaterials in Biodiesel plant-1*/
package reactorQuery;
//import all the necessary libraries
import de.derivo.sparqldlapi.Query;
import de.derivo.sparqldlapi.QueryArgument;
import de.derivo.sparqldlapi.QueryEngine;
import de.derivo.sparqldlapi.QueryResult;
import de.derivo.sparqldlapi.exceptions.QueryEngineException;
import de.derivo.sparqldlapi.exceptions.QueryParserException;
import de.derivo.sparqldlapi.impl.QueryBindingImpl;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.jdom.output.XMLOutputter;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.InferenceType;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory;

public class ReactorQuery {
	
private static QueryEngine engine;
public static String QueryResult;


	public static ArrayList<String> executeReactorQuery () {
		ArrayList<String> result = null;
		try {
			// Create an ontology manager
			OWLOntologyManager manager = OWLManager.createOWLOntologyManager();

			// Load the biodiesel plant owl file.
              OWLOntology BiodieselPlant1 = manager.loadOntologyFromOntologyDocument(IRI.create("File:/C:/apache-tomcat-8.0.24/webapps/ROOT/BiodieselPlant3.owl"));  //try testing the ontology built by YiRen

			// Create an instance of an OWL API reasoner (we use the OWL API built-in StructuralReasoner for the purpose of demonstration here)
            StructuralReasonerFactory factory = new StructuralReasonerFactory();
			OWLReasoner reasoner = factory.createReasoner(BiodieselPlant1);
            // Optionally let the reasoner compute the most relevant inferences in advance
			reasoner.precomputeInferences(InferenceType.CLASS_ASSERTIONS,InferenceType.OBJECT_PROPERTY_ASSERTIONS);

			// Create an instance of the SPARQL-DL query engine
			engine = QueryEngine.create(manager, reasoner, true);
       
			
		   // SPARQL DL code that does the query
			
			result = processQuery(
         			"PREFIX process: <file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/CPS_function/process.owl#>\n" +
 					"PREFIX apparatus: <file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/CPS_realization/plant_equipment/apparatus.owl#>\n" +
					"PREFIX CPS: <file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/chemical_process_system.owl#>\n" +
					"PREFIX behavior: <file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/CPS_behavior/behavior.owl#>\n" +
			        "PREFIX technical_system: <file:/C:/OntoCAPE/OntoCAPE/upper_level/technical_system.owl#>\n" +
					"PREFIX geometry: <file:/C:/OntoCAPE/OntoCAPE/supporting_concepts/geometry/geometry.owl#>\n" +
					"PREFIX space_time: <file:/C:/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time.owl#>\n" +
				    "PREFIX material: <file:/C:/OntoCAPE/OntoCAPE/material/material.owl#>\n" +
				    "PREFIX substance: <file:/C:/OntoCAPE/OntoCAPE/material/substance/substance.owl#>\n" +
					"PREFIX plant: <file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/CPS_realization/plant.owl#>\n" +
					"PREFIX chemical_process_system: <file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/chemical_process_system.owl#>\n" +
 					
					"PREFIX system: <file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#>\n" + 
					"PREFIX topology: <file:/C:/OntoCAPE/meta_model/topology/topology.owl#>\n" + 
					"PREFIX space_and_time_extended: <file:/C:/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time_extended.owl#>\n" + 
 
			
		 	"SELECT ?x ?GCxnv ?GCynv ?connector ?heatduty_value ?reaction_network ?Input ?Output ?flowrate_v ?flowrate_v_out ?input_flowrate_value ?output_flowrate_value WHERE  {\n" +
	      " Type(?x, apparatus:StirredTank)" + "," + 
					          
		
/*                             *//** Query for the geographic coordinate of the reactor*//*
                           
                           
                             */ 
                             
                              "PropertyValue(?x, space_and_time_extended:hasGISCoordinateSystem, ?GSystem)" + "," + 
                              "PropertyValue(?x, plant:hasConnector, ?connector)" + "," + 
							  "PropertyValue(?x, technical_system:realizes, ?reaction)" + "," + 

                              
                              
                              // Query for the coordinates 
                              "PropertyValue(?GSystem, space_and_time_extended:hasProjectedCoordinate_x, ?GCx)" + "," +	
							  "PropertyValue(?GCx, system:hasValue, ?GCXV)" + "," +	
							  "PropertyValue(?GCXV, system:numericalValue, ?GCxnv)" + "," +	
							  
                              "PropertyValue(?GSystem, space_and_time_extended:hasProjectedCoordinate_y, ?GCy)" + "," +	
							  "PropertyValue(?GCy, system:hasValue, ?GCYV)" + "," +	
							  "PropertyValue(?GCYV, system:numericalValue, ?GCynv)" +   "," +	
							  
							  // Query for the heatduty and inputs/outputs 
							  "PropertyValue(?reaction, behavior:hasHeatDuty, ?heatduty)" + "," + 
							  "PropertyValue(?reaction,topology:hasInput , ?Input)" + "," + 
							  "PropertyValue(?reaction,topology:hasOutput , ?Output)" + "," + 
							  // Query for the reactions 
							  "PropertyValue(?heatduty, system:hasValue, ?valueheatduty)" +  "," + 
							  "PropertyValue(?valueheatduty, system:numericalValue, ?heatduty_value)" +  "," + 
							  "PropertyValue(?reaction,process:hasChemicalReactionNetwork , ?reaction_network)" + "," + 
							  // Query for the flow rate
							 "PropertyValue(?Input,chemical_process_system:refersToGeneralizedAmount , ?amount)" + "," + 
							 "PropertyValue(?amount,system:hasSubsystem , ?material)" + "," + 
							"PropertyValue(?material,system:hasProperty , ?flowrate)" + "," + 
							"PropertyValue(?flowrate,system:hasValue , ?flowrate_v)" +  "," + 
							"PropertyValue(?flowrate_v,system:numericalValue , ?input_flowrate_value)" +  "," + 
							
			  				"PropertyValue(?Output,chemical_process_system:refersToGeneralizedAmount , ?amount_out)" + "," + 
			  				"PropertyValue(?amount_out,system:hasSubsystem , ?material_out)" + "," + 
			  				"PropertyValue(?material_out,system:hasProperty , ?flowrate_out)" + "," + 
							"PropertyValue(?flowrate_out,system:hasValue , ?flowrate_v_out)" +  "," + 
							"PropertyValue(?flowrate_v_out,system:numericalValue , ?output_flowrate_value)" + // "," + 
							
							
						
				// system:hasValue		   <behavior:hasHeatDuty rdf:resource="http://www.jparksimulator.com/BiodieselPlant3.owl#HeatDutyOfR-301"/>
      //  <process:hasChemicalReactionNetwork rdf:resource="http://www.jparksimulator.com/BiodieselPlant3.owl#ReactionNetwork_Bio"/>
     //   <topology:hasInput rdf:resource="http://www.jparksimulator.com/BiodieselPlant3.owl#ProcessStream_3-3"/>

	 
	 
	 
	 
	 
	 
                 
  					          
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
		String[][] valt= new String[10][], valt1= new String[10][], valt2= new String[10][], valt3= new String[10][], valt4= new String[10][], valt5= new String[10][], valt6= new String[10][], valt7= new String[10][], valt8= new String[10][], valt9= new String[10][], valt10= new String[10][], valt11= new String[10][], valt12= new String[10][];
		String[][] OP= new String[10][], VOP= new String[10][], UOP= new String[10][], OT= new String[10][], VOT= new String[10][], UOT= new String[10][];
		String[][] GISCx = new String[10][], GISCy = new String[10][];
		
		String[] QResult[]=null;
		
		try {
                    
			long startTime = System.currentTimeMillis();
			
			// Create a query object from it's string representation
			Query query = Query.create(q);
			
			System.out.println("Excecute the query:");
			System.out.println(q);
			System.out.println("-------------------------------------------------");
			
			// Execute the query and generate the result set
			QueryResult result = engine.execute(query);
		//	QueryResult = result.toString();
         //   System.out.println(result);

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
					System.out.println("Results: -- we are upto here");
					                    String Rresult = null;
                	                    String rresult = null, rresult1 = null;
                	                    int count=0;
                	                    
                	                   
                	                    
                	                    
                	                    
                						System.out.println("Results: -- look at result " + result.toString());
                						System.out.println("The size of the result array" + result.size());
                            
                                        for(int i  = 0; i < result.size() ; i ++)
                                        {
                                        	
                                             
                                            QueryBindingImpl binding = (QueryBindingImpl) result.get(i);
                                            Set<QueryArgument> argument = binding.getBoundArgs();
                    						
                                        	for(QueryArgument a : argument)
                                        	{
                                        		QueryArgument qa = binding.get(a);
                                        		System.out.println(a.getValue());
                                        		Aresult.add(a.getValue() + "---" + qa.getValue());
                                        		
                                        		
                                        	}
                                        	
                                        	
                                        }
                                           
                                      
                                        

                                  
                                          
                                                        
                                      
                                   //     Aresult.add("% There are " + count + " Reactors in this Plant. %"); 
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

