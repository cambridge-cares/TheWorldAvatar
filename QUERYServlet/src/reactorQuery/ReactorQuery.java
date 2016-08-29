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
import java.util.Set;

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
	
	public static ArrayList<String> executeReactorQuery () {
		ArrayList<String> result = null;
		try {
			// Create an ontology manager
			OWLOntologyManager manager = OWLManager.createOWLOntologyManager();

			// Load the biodiesel plant owl file.
              OWLOntology BiodieselPlant1 = manager.loadOntologyFromOntologyDocument(IRI.create("File:/C:/apache-tomcat-8.0.24/webapps/ROOT/BiodieselPlant.owl"));  //try testing the ontology built by YiRen

			// Create an instance of an OWL API reasoner (we use the OWL API built-in StructuralReasoner for the purpose of demonstration here)
            StructuralReasonerFactory factory = new StructuralReasonerFactory();
			OWLReasoner reasoner = factory.createReasoner(BiodieselPlant1);
            // Optionally let the reasoner compute the most relevant inferences in advance
			reasoner.precomputeInferences(InferenceType.CLASS_ASSERTIONS,InferenceType.OBJECT_PROPERTY_ASSERTIONS);

			// Create an instance of the SPARQL-DL query engine
			engine = QueryEngine.create(manager, reasoner, true);
                 	
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
					"PREFIX system: <file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#>\n" + 
					"PREFIX topology: <file:/C:/OntoCAPE/meta_model/topology/topology.owl#>\n" + 
					"PREFIX p7: <file:/C:/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time_extended.owl#>\n" + 
			"SELECT ?x ?GCxnv ?GCynv ?V ?Volume ?unit ?F ?FnumericalValue ?FUnit ?P ?Pressure ?Punit ?T ?Temperature ?Tunit ?Input ?Fo ?FonumericalValue ?FoUnit ?Output WHERE {\n" +
					   " Type(?x, apparatus:StirredTank)" + "," +
					          
                             /** Query for the geographic coordinate of the reactor*/
                              "PropertyValue(?x, p7:hasProjectedCoordinate_x, ?GCx)" + "," +	
                              "PropertyValue(?GCx, system:hasValue, ?GCxv)" + "," +	
                              "PropertyValue(?GCxv, system:numericalValue, ?GCxnv)" + "," +
                              "PropertyValue(?x, p7:hasProjectedCoordinate_y, ?GCy)" + "," +	
                              "PropertyValue(?GCy, system:hasValue, ?GCyv)" + "," +	
                              "PropertyValue(?GCyv, system:numericalValue, ?GCynv)" + "," +

                             /** Query for the volume of the reactor*/
					          "PropertyValue(?x, geometry:has_volume, ?V)" + "," +						          
					          "PropertyValue(?V, system:hasValue, ?Vv)" + "," +	
					          "PropertyValue(?Vv, system:numericalValue, ?Volume)" + "," +
					          "PropertyValue(?Vv, system:hasUnitOfMeasure, ?unit)" + "," +
					          
                           	/**Query for the operating pressure*/
                              "PropertyValue(?x, process:hasOperatingPressure, ?P)" + "," +						          
                              "PropertyValue(?P, system:hasValue, ?Pv)" + "," +	
                              "PropertyValue(?Pv, system:numericalValue, ?Pressure)" + "," +
                              "PropertyValue(?Pv, system:hasUnitOfMeasure, ?Punit)" + "," +
                           /**Query for the operating temperature*/   
                              "PropertyValue(?x, process:hasOperatingTemperature, ?T)" + "," +						          
                              "PropertyValue(?T, system:hasValue, ?Tv)" + "," +	
                              "PropertyValue(?Tv, system:numericalValue, ?Temperature)" + "," +
                              "PropertyValue(?Tv, system:hasUnitOfMeasure, ?Tunit)" + "," +
					          
					          "PropertyValue(?x, topology:hasInput, ?Input)" + ","+ 
					          "PropertyValue(?Input, CPS:refersToGeneralizedAmount,?b)" + ","+         						          
                              "PropertyValue(?b, system:hasSubsystem,?c)" + ","+         						          
                              "PropertyValue(?c, behavior:refersToMaterial,?material)" + ","+ 
                              
                              "PropertyValue(?c, system:hasProperty,?F)" + ","+ 
                              "PropertyValue(?F, system:hasValue,?VF)" + ","+ 
                              "PropertyValue(?VF, system:numericalValue,?FnumericalValue)" + ","+ 
                              "PropertyValue(?VF, system:hasUnitOfMeasure,?FUnit)" + ","+ 

                              "PropertyValue(?x, topology:hasOutput, ?Output)" + ","+ 
                              "PropertyValue(?Output, CPS:refersToGeneralizedAmount,?bo)" + ","+         						          
                              "PropertyValue(?bo, system:hasSubsystem,?co)" + ","+         						          
                              "PropertyValue(?co, behavior:refersToMaterial,?materialo)" + ","+ 
     
                              "PropertyValue(?co, system:hasProperty,?Fo)" + ","+ 
                              "PropertyValue(?Fo, system:hasValue,?VFo)" + ","+ 
                              "PropertyValue(?VFo, system:numericalValue,?FonumericalValue)" + ","+ 
                              "PropertyValue(?VFo, system:hasUnitOfMeasure,?FoUnit)" + 
  					          
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
					                    String Rresult = null;
                	                    String rresult = null, rresult1 = null;
                	                    int count=0;
                                        for (int i = 0; i < result.size(); i++) {
                                        	
                                                QueryBindingImpl binding = (QueryBindingImpl) result.get(i);
                                                Set<QueryArgument> argument = binding.getBoundArgs();
                                                for (QueryArgument a : argument) {
                                                    QueryArgument qa = binding.get(a);
                                                    if (a.getValue().compareToIgnoreCase("x") == 0) {                                                       
                                                        valt[i] = qa.getValue().split("#");                                                       	                    
                                                    }     
                                                    
                                                    if (a.getValue().compareToIgnoreCase("GCxnv") == 0) {                                                       
                                                    	GISCx[i] = qa.getValue().split("#");                                                       	                    
                                                    }
                                                    
                                                    if (a.getValue().compareToIgnoreCase("GCynv") == 0) {                                                       
                                                    	GISCy[i] = qa.getValue().split("#");                                                       	                    
                                                    }
                                                    
                                                    if (a.getValue().compareToIgnoreCase("V") == 0) {
                                                        valt1[i] = qa.getValue().split("#");           
                                                    }
                                                     
                                                    if (a.getValue().compareToIgnoreCase("Volume") == 0) {
                                                        valt2[i] = qa.getValue().split("#");
                                                    }
                                                   
                                                    if (a.getValue().compareToIgnoreCase("unit") == 0) {
                                                        valt3[i] = qa.getValue().split("#");                                                       
                                                    }
                                                    /** Operating Pressure*/
                                                    if (a.getValue().compareToIgnoreCase("P") == 0) {
                                                        OP[i] = qa.getValue().split("#");           
                                                    }
                                                     
                                                    if (a.getValue().compareToIgnoreCase("Pressure") == 0) {
                                                        VOP[i] = qa.getValue().split("#");
                                                    }
                                                   
                                                    if (a.getValue().compareToIgnoreCase("Punit") == 0) {
                                                        UOP[i] = qa.getValue().split("#");                                                       
                                                    }
                                                    /** Operating Temperature*/
                                                    if (a.getValue().compareToIgnoreCase("T") == 0) {
                                                        OT[i] = qa.getValue().split("#");           
                                                    }
                                                     
                                                    if (a.getValue().compareToIgnoreCase("Temperature") == 0) {
                                                        VOT[i] = qa.getValue().split("#");
                                                    }
                                                   
                                                    if (a.getValue().compareToIgnoreCase("Tunit") == 0) {
                                                        UOT[i] = qa.getValue().split("#");                                                       
                                                    }
                                                    /** Flowrate*/
                                                    if (a.getValue().compareToIgnoreCase("F") == 0) {
                                                        valt4[i] = qa.getValue().split("#");                                                        
                                                    }
                                                    
                                                    if (a.getValue().compareToIgnoreCase("FnumericalValue") == 0) {
                                                        valt5[i] = qa.getValue().split("#");                                                        
                                                    }                                                    
                                                    if (a.getValue().compareToIgnoreCase("FUnit") == 0) {
                                                        valt6[i] = qa.getValue().split("#");                                                        
                                                    }  
                                                    /** Input Stream*/                                                   
                                                    if (a.getValue().compareToIgnoreCase("Input") == 0) {
                                                        valt7[i] = qa.getValue().split("#");                                                        
                                                    }                                                    
                                                                                                     
                                                    if (a.getValue().compareToIgnoreCase("MF") == 0) {
                                                        valt9[i] = qa.getValue().split("#");                                                       
                                                    }
                                                   
                                                    if (a.getValue().compareToIgnoreCase("output") == 0) {
                                                        valt10[i] = qa.getValue().split("#");                                                        
                                                    }   
                                                    if (a.getValue().compareToIgnoreCase("Fo") == 0) {
                                                        valt8[i] = qa.getValue().split("#");                                                        
                                                    }
                                                    if (a.getValue().compareToIgnoreCase("FonumericalValue") == 0) {
                                                        valt11[i] = qa.getValue().split("#");                                                        
                                                    }
                                                    if (a.getValue().compareToIgnoreCase("FoUnit") == 0) {
                                                        valt12[i] = qa.getValue().split("#");                                                        
                                                    }                                                
                                                }
                                                
                                                if(i==0){
                                                	rresult = ("The information we want for " + valt[0][1] + " is: %" + "GISLocation( " + GISCx[i][0] +" ; " + GISCy[i][0] + " );%" + valt1[0][1] + " = "+ valt2[0][0] + " "+ valt3[0][1] + ";  " + OP[0][1] + " = " + VOP[0][0] + " " + UOP[0][1] + ";  " + OT[0][1] + " = " + VOT[0][0] + " " + UOT[0][1] +  "%InputStream:  " + valt7[i][1] + "  " + valt4[0][1] + " = " + valt5[0][0] + " " +  valt6[0][1] +"%OutputStream:  " + valt10[i][1] +"  " + valt8[i][1] + " = "+ valt11[i][0]+" "+ valt12[i][1]);
                                                	Aresult.add(rresult);
                                                	count ++;
                                                }
                                                else if(i>0 ){
                                                	if(!valt[i][1].equals( valt[i-1][1])){
                                                	rresult1 = ("The information we want for " + valt[i][1] + " is: %" + "GISLocation( " + GISCx[i][0] +" ; " + GISCy[i][0] + " );%" + valt1[i][1] + " = "+ valt2[i][0] + " "+ valt3[i][1] + ";  " + OP[i][1] + " = " + VOP[i][0] + " " + UOP[i][1] + ";  " + OT[i][1] + " = " + VOT[i][0] + " " + UOT[i][1] +  "%InputStream:  " + valt7[i][1] + "  " + valt4[i][1] + " = " + valt5[i][0] + " " +  valt6[i][1] +"%OutputStream:  " + valt10[i][1] +"  " + valt8[i][1] + " = "+ valt11[i][0]+" "+ valt12[i][1]);
                                                	
                                                	Aresult.add(rresult1);
                                                	count ++;
                                                	}
                                                }

                                                Aresult.add(Rresult);
                                                        
                                        }
                                        Aresult.add("% There are " + count + " Reactors in this Plant. %"); 
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

