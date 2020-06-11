import java.io.File;
import java.util.Iterator;

import org.semanticweb.HermiT.Configuration;
import org.semanticweb.HermiT.Reasoner.ReasonerFactory;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import com.hp.hpl.jena.rdf.model.InfModel;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.reasoner.ValidityReport;
import com.hp.hpl.jena.util.FileManager;

public class reasoner_jena {
		
		
		
		 public static void main(String[] args) throws Exception {
		        // First, we create an OWLOntologyManager object. The manager will load and 
		    	   //OWLOntologyManager manager=OWLManager.createOWLOntologyManager();
		           // We will create several things, so we save an instance of the data factory
		           //OWLDataFactory dataFactory=manager.getOWLDataFactory();
		           // Now, we create the file from which the ontology will be loaded. 
		           // Here the ontology is stored in a file locally in the ontologies subfolder
		           // of the examples folder.
		          // File inputOntologyFile = new File("src/test_abox/debutaniser_section.owl");
		           // We use the OWL API to load the ontology. 
		           //OWLOntology ontology=manager.loadOntologyFromOntologyDocument(inputOntologyFile);
		           
		           
		           Model data = FileManager.get().loadModel("src/test_abox/debutaniser_section.owl");
		           InfModel infmodel = ModelFactory.createRDFSModel(data);
		           ValidityReport validity = infmodel.validate();
		           if (validity.isValid()) {
		               System.out.println("OK");
		           } else {
		               System.out.println("Conflicts");
		               for (Iterator i = validity.getReports(); i.hasNext(); ) {
		                   System.out.println(" - " + i.next());
		               }
		           }

		        // Lets make things worth and turn Pizza into an inconsistent ontology by asserting that the 
		        // unsatisfiable icecream class has some instance. 
		        // First, create an instance of the OWLClass object for the unsatisfiable  icecream class.

		       
		    }
		
		
	
}
