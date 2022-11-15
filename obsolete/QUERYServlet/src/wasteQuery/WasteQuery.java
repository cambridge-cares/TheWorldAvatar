/** This piece of code is write to execute query function for the waste in Biodiesel plant-1*/
package wasteQuery;
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

public class WasteQuery {

private static QueryEngine engine;
	
	public static ArrayList<String> executeRawMaterialQuery () {
		ArrayList<String> result = null;
		try {
			// Create an ontology manager
			OWLOntologyManager manager = OWLManager.createOWLOntologyManager();

			// Load the biodiesel plant owl file.
              OWLOntology BiodieselPlant1 = manager.loadOntologyFromOntologyDocument(IRI.create("File:/C:/apache-tomcat-8.0.24/webapps/ROOT/BiodieselPlant1WOWHR.owl"));  //try testing the ontology built by YiRen

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
				           "Type(?x, process:ReusableWasteProduct)" +  ","+   
                                      "PropertyValue(?x, CPS:refersToGeneralizedAmount,?b)" + ","+ 
                                      "PropertyValue(?b, system:hasSubsystem,?c)" + ","+ 
                                      "PropertyValue(?c, behavior:refersToMaterial,?material)" + ","+ 
                                      /**
                                      "PropertyValue(?material, material:intrinsicCharacteristics, ?S)" + ","+                                       
                                      "PropertyValue(?S, substance:hasMolecularStructure, ?ME)" + ","+ 
                                      "PropertyValue(?ME, ?molecularFormular, ?MF)" + ","+ 
                                      "PropertyValue(?c, system:hasProperty,?F)" + ","+ 
                                      "PropertyValue(?F, system:hasValue,?VF)" + ","+ 
                                      "PropertyValue(?VF, system:numericalValue,?FnumericalValue)" + ","+ 
                                      "PropertyValue(?VF, system:hasUnitOfMeasure,?FUnit)" + ","+ 
                                       */                                    
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

	private static ArrayList<String> processQuery(String string) {
		// TODO Auto-generated method stub
		return null;
	}
	
}
