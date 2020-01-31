package uk.ac.cam.ceb.como.compchem.ontology;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Set;

import org.semanticweb.HermiT.Configuration;
import org.semanticweb.HermiT.Reasoner;
import org.semanticweb.HermiT.Reasoner.ReasonerFactory;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import com.clarkparsia.owlapi.explanation.BlackBoxExplanation;
import com.clarkparsia.owlapi.explanation.HSTExplanationGenerator;

import uk.ac.cam.ceb.como.jaxb.parsing.utils.FileUtility;
import uk.ac.cam.ceb.como.jaxb.parsing.utils.Utility;

/**
 * 
 * The Class InconsistencyExplanation.
 *
 * @author nk510 <p>This class implements the inconsistency explanation for Ontochem
 *         ontology given in'src/test/resources/ontochem-ontology/' folder . We use
 *         Hermit API and OWL API. It checks consistency, and in case of
 *         inconsistency it returns inconsistency explanation.</p>
 * 
 *         <p>Hermit reasoner licence type is: 'GNU Lesser General Public License'
 *         Hermit reasoner licence is available at:
 *         http://www.hermit-reasoner.com/download/1.3.8/readme.txt</p>
 *
 */

public class InconsistencyExplanation {	

	/** File path to generated ontologies (Abox of Compchem ontology). **/
    static String compchemFilePath = "src/test/resources/ontology/compchem_abox"; //ontokin compchem_abox
    
//  static String compchemFilePath = "C:\\Users\\NK\\git\\thermochemistry\\CoMoOntology\\src\\test\\resources\\ontology\\compchem_abox";
//  static String compchemFilePath = "C:\\Users\\NK\\git\\thermochemistry\\CoMoOntology\\src\\test\\resources\\ontology\\onto_engine";
    
	/**
	 * 
	 * The main method.
	 *
	 * @param args the arguments
	 * @throws OWLOntologyCreationException the OWL ontology creation exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * 
	 */
      
	public static void main(String[] args) throws OWLOntologyCreationException, IOException {
		
		Utility utility = new FileUtility();
		
		/**
		 * 
		 * @author nk510 <p>list of ontology files. Allowed format are owl, rdf, and ttl.</p>
		 * 
		 */

		File[] fileList = utility.getFileList(compchemFilePath,".owl", ".rdf", ".ttl");
		
		/**
		 * 
		 * @author nk510 <p>Checks consistency. In case of inconsistency it provides
		 *         explanation. </p>
		 * 
		 */
		

		getReasoningExplanation(fileList);

	}

	/**
	 * Gets the inconsistency explanation.
	 *
	 * @author nk510
	 * @param reasoner the reasoner
	 * @param factory the factory
	 * @param dataFactory the data factory
	 * @param ontology the ontology
	 * @throws IOException <p>Returns all the explanations for the given unsatisfiable ontology
	 *             class (owl:Thing). To provide explanation of inconsistencies in
	 *             Abox one needs to define all individuals to be instance of
	 *             owl:Thing. </p>
	 */

	public static void getInconsistencyExplanation(OWLReasoner reasoner, ReasonerFactory factory,
			OWLDataFactory dataFactory, OWLOntology ontology) throws IOException {

		if (!reasoner.isConsistent()) {

			System.out.println("Inconsistency explanation:");
			
			factory = getOntologyReasonerFactoryForHermit(ontology, factory);
			
			HSTExplanationGenerator multExplanator = getHstExplanationGenerator(reasoner, factory, dataFactory,
					ontology);
			
			printExplanation(multExplanator, dataFactory);

		}
	}

	 /**
	 * 
 	 * Gets the ontology reasoner factory for HermiT.
 	 *
 	 * @param ontology the ontology as a file
 	 * @param factory the factory
 	 * @return factory the reasoner factory
 	 * 
 	 */

	public static ReasonerFactory getOntologyReasonerFactoryForHermit(
				OWLOntology ontology, ReasonerFactory factory) {
		 
			factory = new Reasoner.ReasonerFactory() {

				protected OWLReasoner createHermiTOWLReasoner(org.semanticweb.HermiT.Configuration configuration,
						OWLOntology ontology) {

					/**
					 * 
					 * @author nk510 <p>Should not throw an exception in case of inconsistency, and set
					 *         configuration's exception to be false.</p>
					 * 
					 */

					configuration.throwInconsistentOntologyException = false;

					return new Reasoner(configuration, ontology);
				}
			};
			
			return factory;
	 }
	 
	/**
	 * Gets the reasoning explanation.
	 *
	 * @param fileList the file list
	 * @throws OWLOntologyCreationException the OWL ontology creation exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	
	public static void getReasoningExplanation(File[] fileList) throws OWLOntologyCreationException, IOException {

		/**
		 * 
		 * @author nk510 <p>Iterates over list of files given in folder
		 *         'src/test/resources/ontology/compchem_ontology' and checks
		 *         consistency of each ontology.</p>
		 *
		 */

		for (File f : fileList) {

			/**
			 * 
			 * @author nk510 <p>Instance of OWLOntologyManager will be used to load and save
			 *         ontologies from local file.</p>
			 * 
			 */

			OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
			
			/**
			 * 
			 * @author nk510 <p>An instance of the data factory used in inference explanation.</p>
			 * 
			 */

			OWLDataFactory dataFactory = manager.getOWLDataFactory();

			/**
			 * 
			 * @author nk510 <p>We use the OWL API to load ontologies from local file.</p>
			 * 
			 */

			OWLOntology ontology = manager.loadOntologyFromOntologyDocument(f);

			Configuration configuration = new Configuration();

			/**
			 * 
			 * @author nk510 <p>Does not throw an exception in case of inconsistency. </p>
			 * 
			 */

			configuration.throwInconsistentOntologyException = false;

			/**
			 * 
			 * @author nk510 <p>Hermit reasoner does not support explanation for inconsistency.
			 *         To do that, we have to instantiate Hermit reasoner as an owl reasoner
			 *         (OWLReasoner). For that reason, we instantiate ReasonerFactory. </p>
			 *
			 */

			ReasonerFactory factory = new ReasonerFactory();

			/**
			 * 
			 * @author nk510 <p>A line below uses an instance of ReasonerFactory to obtain an
			 *         instance of HermiT as an OWLReasoner. </p>
			 * 
			 */

			OWLReasoner reasoner = factory.createReasoner(ontology, configuration);

			System.out.println("Is the ' " + f.getName() + " ' ontology consistent? : " + reasoner.isConsistent());

			/**
			 * 
			 * @author nk510 <p>If inconsistency is detected then code generates inconsistency
			 *         explanation. </p>
			 * 
			 */

			if (!reasoner.isConsistent()) {

				getInconsistencyExplanation(reasoner, factory, dataFactory, ontology);
			
			}

		}
	}	

	/**
	 * Gets the hst explanation generator.
	 *
	 * @param reasoner the reasoner
	 * @param factory the factory
	 * @param dataFactory the data factory
	 * @param ontology the ontology
	 * @return the hst explanation generator
	 */
	
	public static HSTExplanationGenerator getHstExplanationGenerator(OWLReasoner reasoner, ReasonerFactory factory,
			OWLDataFactory dataFactory, OWLOntology ontology) {

		/**
		 * 
		 * @author nk510 <p>Instantiation of explanation (BlackBoxExplanation) classes.</p>
		 * 
		 */

		BlackBoxExplanation exp = new BlackBoxExplanation(ontology, factory, reasoner);
		HSTExplanationGenerator multExplanator = new HSTExplanationGenerator(exp);

		return multExplanator;
	}

	/**
	 * 
	 * Prints the explanation.
	 *
	 * @param multiExplanator the multi explanator
	 * @param dataFactory the data factory
	 * 
	 */
	
	public static void printExplanation(HSTExplanationGenerator multiExplanator, OWLDataFactory dataFactory) {

		/**
		 * 
		 * @author nk510 <p>Inconsistency sources are OWL axioms, and we save inconsistency
		 *         cases as a set of OWLAxiom. </p>
		 * 
		 */

		Set<Set<OWLAxiom>> explanations = multiExplanator.getExplanations(dataFactory.getOWLThing());

		/**
		 * 
		 * @author nk510 <p>Iteration over set of OWLAxiom in order to list inconsistency
		 *         explanation. </p>
		 * 
		 */

		for (Set<OWLAxiom> explanation : explanations) {

			for (OWLAxiom axioms : explanation) {

				/**
				 * @author nk510 <p>Inconsistency explanation report is generated on System.out
				 *         (console).</p>
				 */

				System.out.println(axioms);

			}
		}
	}
	
	/**
	 * Gets the reasoner factory.
	 *
	 * @param reasoner the reasoner
	 * @param factory the factory
	 * @return the reasoner factory
	 */
	
	public static ReasonerFactory getReasonerFactory(OWLReasoner reasoner, ReasonerFactory factory) {		

			System.out.println("Inconsistency explanation:");

			factory = new Reasoner.ReasonerFactory() {

				protected OWLReasoner createHermiTOWLReasoner(org.semanticweb.HermiT.Configuration configuration,
						OWLOntology ontology) {

					/**
					 * 
					 * @author nk510 <p>Should not throw an exception in case of inconsistency, and set
					 *         configuration's exception to be false.</p>
					 * 
					 */

					configuration.throwInconsistentOntologyException = false;

					return new Reasoner(configuration, ontology);
				}
			};

		return factory;
	}
	
	/**
	 * 
	 * @param aboxOwlFilePath <p>Input owl file (as an Abox set of statement) for which consistency is checked with respect to Compchem ontology file.</p>
	 * @return true if input owl file is consistent, and false if input owl file is inconsistent.
	 * @throws OWLOntologyCreationException  creation ontology exception.
	 * @throws FileNotFoundException throws exception if ontology does not exist, or if there is problem with ontology creation. 
	 * 
	 */
	
public static boolean getConsistencyOWLFile(String aboxOwlFilePath) throws OWLOntologyCreationException, FileNotFoundException {
		
		boolean inconsistency = true;
		
		FileInputStream aboxFile = new FileInputStream(new File(aboxOwlFilePath));
		
		/**
		 * 
		 * @author nk510 <p>Instance of {@link org.semanticweb.owlapi.model.OWLOntologyManager} will be used to load and save/load
		 *         ontologies from local file.</p>
		 * 
		 */
		
		OWLOntologyManager manager = OWLManager.createOWLOntologyManager();

		/**
		 * 
		 * @author nk510 <p>We use the OWL API to load ontologies from local file.</p>
		 * 
		 */

		OWLOntology ontology = manager.loadOntologyFromOntologyDocument(aboxFile);		

		Configuration configuration = new Configuration();
		
		/**
		 * 
		 * @author nk510 <p>Hermit reasoner does not support explanation for inconsistency.
		 *         To do that, we have to instantiate Hermit reasoner as an owl reasoner
		 *         ({@link org.semanticweb.owlapi.reasoner.OWLReasoner}). For that reason, we instantiate {@link org.semanticweb.HermiT.Reasoner.ReasonerFactory}. </p>
		 *
		 */

		ReasonerFactory factory = new ReasonerFactory();

		/**
		 * 
		 * @author nk510 <p>A line below uses an instance of ReasonerFactory to obtain an
		 *         instance of HermiT as an {@link org.semanticweb.owlapi.reasoner.OWLReasoner}. </p>
		 * 
		 */

		OWLReasoner reasoner = factory.createReasoner(ontology, configuration);
		
		inconsistency= reasoner.isConsistent();
		
		return inconsistency;
	}
}