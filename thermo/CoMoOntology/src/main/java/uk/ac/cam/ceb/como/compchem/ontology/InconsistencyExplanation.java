package uk.ac.cam.ceb.como.compchem.ontology;

import java.io.File;
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

/**
 * 
 * @author nk510 This class implements the inconsistency explanation for Compchem
 *         ontology given in'src/test/resources/compchem-ontology/' folder . We use
 *         Hermit API and OWL API. It checks consistency, and in case of
 *         inconsistency it returns inconsistency explanation.
 * 
 *         Hermit reasoner licence type is: 'GNU Lesser General Public License'
 *         Hermit reasoner licence is available at:
 *         http://www.hermit-reasoner.com/download/1.3.8/readme.txt
 * 
 */

public class InconsistencyExplanation {

	static String compchemIriPath = "https://como.cheng.cam.ac.uk/kb/ontokin/";
	static String aboxFilePath = "src/test/resources/ontology/compchem_abox/";
	static String compchemFilePath = "src/test/resources/ontology/compchem_ontology/";

	static FileUtility fileUtility = new FileUtility();

	public static void main(String[] args) throws OWLOntologyCreationException, IOException {

		/**
		 * 
		 * @author nk510 File list where all ontologies are saved.
		 * 
		 */

		File[] fileList = fileUtility.getOntologyFileList(compchemFilePath);

		/**
		 * 
		 * @author nk510 Checks consistency, and in case of inconsistency it provides
		 *         explanation.
		 * 
		 */
		
		getReasoningExplanation(fileList);

	}

	/**
	 * @author nk510
	 * @param reasoner
	 * @param factory
	 * @param dataFactory
	 * @param ontology
	 * @throws IOException
	 * 
	 *             Returns all the explanations for the given unsatisfiable ontology
	 *             class (owl:Thing). To provide explanation of inconsistencies in
	 *             Abox one needs to define all individuals to be instance of
	 *             owl:Thing.
	 * 
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
	 * @param ontology
	 * @param factory
	 * @return factory
	 */

	public static ReasonerFactory getOntologyReasonerFactoryForHermit(
				OWLOntology ontology, ReasonerFactory factory) {
		 
			factory = new Reasoner.ReasonerFactory() {

				protected OWLReasoner createHermiTOWLReasoner(org.semanticweb.HermiT.Configuration configuration,
						OWLOntology ontology) {

					/**
					 * 
					 * @author nk510 Should not throw an exception in case of inconsistency, and set
					 *         configuration's exception to be false.
					 * 
					 */

					configuration.throwInconsistentOntologyException = false;

					return new Reasoner(configuration, ontology);
				}
			};
			
			return factory;
	 }
	 
	/**
	 * @param fileList
	 * @throws OWLOntologyCreationException
	 * @throws IOException
	 */
	
	public static void getReasoningExplanation(File[] fileList) throws OWLOntologyCreationException, IOException {

		/**
		 * 
		 * @author nk510 Iterates over list of files given in folder
		 *         'src/test/resources/ontology/compchem_ontology' and checks
		 *         consistency of each ontology.
		 *         
		 */

		for (File f : fileList) {

			/**
			 * 
			 * @author nk510 Instance of OWLOntologyManager will be used to load and save
			 *         ontologies from local file.
			 * 
			 */

			OWLOntologyManager manager = OWLManager.createOWLOntologyManager();

			/**
			 * 
			 * @author nk510 An instance of the data factory used in inference explanation.
			 * 
			 */

			OWLDataFactory dataFactory = manager.getOWLDataFactory();

			/**
			 * 
			 * @author nk510 We use the OWL API to load ontologies from local file.
			 * 
			 */

			OWLOntology ontology = manager.loadOntologyFromOntologyDocument(f);

			Configuration configuration = new Configuration();

			/**
			 * 
			 * @author nk510 Does not throw an exception in case of inconsistency
			 * 
			 */

			configuration.throwInconsistentOntologyException = false;

			/**
			 * 
			 * @author nk510 Hermit reasoner does not support explanation for inconsistency.
			 *         To do that, we have to instantiate Hermit reasoner as an owl reasoner
			 *         (OWLReasoner). For that reason, we instantiate ReasonerFactory.
			 *
			 */

			ReasonerFactory factory = new ReasonerFactory();

			/**
			 * 
			 * @author nk510 A line below uses an instance of ReasonerFactory to obtain an
			 *         instance of HermiT as an OWLReasoner.
			 * 
			 */

			OWLReasoner reasoner = factory.createReasoner(ontology, configuration);

			System.out.println("Is the ' " + f.getName() + " ' ontology consistent? : " + reasoner.isConsistent());

			/**
			 * 
			 * @author nk510 If inconsistency is detected then code generates inconsistency
			 *         explanation.
			 * 
			 */

			if (!reasoner.isConsistent()) {

				getInconsistencyExplanation(reasoner, factory, dataFactory, ontology);
			
			}

		}
	}	

	/**
	 * @param reasoner
	 * @param factory
	 * @param dataFactory
	 * @param ontology
	 * @return
	 */
	
	public static HSTExplanationGenerator getHstExplanationGenerator(OWLReasoner reasoner, ReasonerFactory factory,
			OWLDataFactory dataFactory, OWLOntology ontology) {

		/**
		 * 
		 * @author nk510 Instantiation of explanation classes.
		 * 
		 */

		BlackBoxExplanation exp = new BlackBoxExplanation(ontology, factory, reasoner);
		HSTExplanationGenerator multExplanator = new HSTExplanationGenerator(exp);

		return multExplanator;
	}

	/**
	 * @param multExplanator
	 * @param dataFactory
	 */
	
	public static void printExplanation(HSTExplanationGenerator multExplanator, OWLDataFactory dataFactory) {

		/**
		 * 
		 * @author nk510 Inconsistency sources are OWL axioms, and we save inconsistency
		 *         cases as a set of OWLAxiom.
		 * 
		 */

		Set<Set<OWLAxiom>> explanations = multExplanator.getExplanations(dataFactory.getOWLThing());

		/**
		 * 
		 * @author nk510 Iteration over set of OWLAxiom in order to list inconsistency
		 *         explanation.
		 * 
		 */

		for (Set<OWLAxiom> explanation : explanations) {

			for (OWLAxiom axioms : explanation) {

				/**
				 * @author nk510 Inconsistency explanation report is generated on System.out
				 *         (console).
				 */

				System.out.println(axioms);

			}
		}
	}
	
	/**
	 * @param reasoner
	 * @param factory
	 * @return
	 */
	
	public static ReasonerFactory getReasonerFactory(OWLReasoner reasoner, ReasonerFactory factory) {		

			System.out.println("Inconsistency explanation:");

			factory = new Reasoner.ReasonerFactory() {

				protected OWLReasoner createHermiTOWLReasoner(org.semanticweb.HermiT.Configuration configuration,
						OWLOntology ontology) {

					/**
					 * 
					 * @author nk510 Should not throw an exception in case of inconsistency, and set
					 *         configuration's exception to be false.
					 * 
					 */

					configuration.throwInconsistentOntologyException = false;

					return new Reasoner(configuration, ontology);
				}
			};

		return factory;
	}
}