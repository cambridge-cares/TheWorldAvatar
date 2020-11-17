package uk.ac.cam.cares.jps.agent.DLchecker;

import java.io.File;
import java.util.Iterator;
import com.hp.hpl.jena.rdf.model.InfModel;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.reasoner.ValidityReport;
import com.hp.hpl.jena.util.FileManager;

public class Cosnistency {
	public static boolean checkSingleFile(String targetFile) throws Exception {
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
           
           boolean consistent;
           Model data = FileManager.get().loadModel(targetFile);
           InfModel infmodel = ModelFactory.createRDFSModel(data);
           ValidityReport validity = infmodel.validate();
           if (validity.isValid()) {
               consistent=true;
           } else {
               consistent =false;
               for (Iterator i = validity.getReports(); i.hasNext(); ) {
                   System.out.println(" - " + i.next());
               }
           }
           return consistent;


}
}
