import java.io.File;
import org.semanticweb.HermiT.Configuration;
import org.semanticweb.HermiT.Reasoner.ReasonerFactory;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.OWLReasoner;


public class Reasoner {
	public static void main(String[] args) throws OWLOntologyCreationException {
		
		
		
		  //InputStream stream = OwlUtil.class.getResourceAsStream(uri);
		//File f = new File("http://www.theworldavatar.com/ontology/ontopowsys/OntoPowSys.owl");
		  
		File[] files = new File("src/test_Abox").listFiles();
		
		for(File f : files){
			try{	
		OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
		OWLDataFactory dataFactory = manager.getOWLDataFactory();

		if(f.getName().contains(".owl")){
			
		OWLOntology ontology = manager.loadOntologyFromOntologyDocument(f);

		Configuration configuration = new Configuration();

		configuration.throwInconsistentOntologyException = true;

		ReasonerFactory factory = new ReasonerFactory();

		OWLReasoner reasoner = factory.createReasoner(ontology, configuration);
		
		//System.out.println(f.getName() + " is consistent: " + reasoner.isConsistent());
		System.out.println(f + " is consistent: " + reasoner.isConsistent());
		}
		}
		catch(org.semanticweb.owlapi.model.UnloadableImportException e){
			continue;
		}
		
		
	
}}
	
}
