/*
 * 
 */
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
 * The Class InconsistencyExplanation.
 *
 * @author nk510 <p>This class implements the inconsistency explanation for Compchem
 *         ontology given in'src/test/resources/compchem-ontology/' folder . We use
 *         Hermit API and OWL API. It checks consistency, and in case of
 *         inconsistency it returns inconsistency explanation.</p>
 * 
 *         <p>Hermit reasoner licence type is: 'GNU Lesser General Public License'
 *         Hermit reasoner licence is available at:
 *         http://www.hermit-reasoner.com/download/1.3.8/readme.txt</p>
 */

public class InconsistencyExplanation {

	/** The compchem iri path. */
	static String compchemIriPath = "https://como.cheng.cam.ac.uk/kb/ontokin/";
	
	/** The abox file path. */
	static String aboxFilePath = "src/test/resources/ontology/compchem_abox/";
	
	/** The compchem file path. */
	static String compchemFilePath = "src/test/resources/ontology/compchem_ontology/";

	/** The file utility. */
	static FileUtility fileUtility = new FileUtility();

	/**
	 * The main method.
	 *
	 * @param args the arguments
	 * @throws OWLOntologyCreationException the OWL ontology creation exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	public static void main(String[] args) throws OWLOntologyCreationException, IOException {

		/**
		 * 
		 * @author nk510 <p>Array of ontology files.</p>
		 * 
		 */

		File[] fileList = fileUtility.getOntologyFileList(compchemFilePath);

		/**
		 * 
		 * @author nk510 <p>Checks consistency, and in case of inconsistency it provides
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
	 * @return the inconsistency explanation
	 * @throws IOException             <p>Returns all the explanations for the given unsatisfiable ontology
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
 	 * Gets the ontology reasoner factory for hermit.
 	 *
 	 * @param ontology the ontology
 	 * @param factory the factory
 	 * @return factory
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
	 * @return the reasoning explanation
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
	 * Prints the explanation.
	 *
	 * @param multExplanator the mult explanator
	 * @param dataFactory the data factory
	 */
	
	public static void printExplanation(HSTExplanationGenerator multExplanator, OWLDataFactory dataFactory) {

		/**
		 * 
		 * @author nk510 <p>Inconsistency sources are OWL axioms, and we save inconsistency
		 *         cases as a set of OWLAxiom. </p>
		 * 
		 */

		Set<Set<OWLAxiom>> explanations = multExplanator.getExplanations(dataFactory.getOWLThing());

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
}