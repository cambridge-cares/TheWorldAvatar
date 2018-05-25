package uk.ac.ceb.como.gc.ontology;

import java.io.File;

import java.io.FilenameFilter;
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

/**
 * 
 * @author nk510 This class implements the inconsistency explanation for GNVC
 *         ontology given in'src/test/resources/ontology/' folder . We use
 *         Hermit API and OWL API. It checks consistency, and in case of
 *         inconsistency it returns inconsistency explanation.
 * 
 *         Hermit reasoner licence type is: 'GNU Lesser General Public License'
 *         Hermit reasoner licence is available at:
 *         http://www.hermit-reasoner.com/download/1.3.8/readme.txt
 * 
 */

public class InconsistencyExplanation {

	public static void main(String[] args) throws OWLOntologyCreationException, IOException {

		/**
		 * @author nk510 Folder where all ontologies are saved.
		 * 
		 */

		File[] fileList = getFileList("src/test/resources/ontology/");

		/**
		 * @author nk510 Iterates over file list given in folder
		 *         'src/test/resources/ontology/' and checks consistency of each
		 *         ontology.
		 */

		for (File f : fileList) {

			/**
			 * @author nk510 Instance of OWLOntologyManager will be used to load and save
			 *         ontologies from local file.
			 * 
			 */

			OWLOntologyManager manager = OWLManager.createOWLOntologyManager();

			/**
			 * @author nk510 An instance of the data factory
			 * 
			 */

			OWLDataFactory dataFactory = manager.getOWLDataFactory();

			/**
			 * @author nk510 We use the OWL API to load given ontologies from local file.
			 */

			OWLOntology ontology = manager.loadOntologyFromOntologyDocument(f);

			Configuration configuration = new Configuration();

			/**
			 * @author nk510 Does not throw an exception in case of inconsistency
			 */

			configuration.throwInconsistentOntologyException = false;

			/**
			 * @author nk510 Hermit reasoner does not support explanation for inconsistency.
			 *         To do that, we have to instantiate Hermit reasoner as an owl reasoner
			 *         (OWLReasoner). For that reason, we instantiate ReasonerFactory.
			 *
			 */

			ReasonerFactory factory = new ReasonerFactory();

			/**
			 * @author nk510 A line below uses an instance of ReasonerFactory to obtain an
			 *         instance of HermiT as an OWLReasoner.
			 */

			OWLReasoner reasoner = factory.createReasoner(ontology, configuration);

			System.out.println("Is the ' " + f.getName() + " ' ontology consistent? : " + reasoner.isConsistent());

			/**
			 * @author nk510 If inconsistency is detected then we generate inconsistency
			 *         explanation.
			 */
			getInconsistencyExplanation(reasoner, factory, dataFactory, ontology);

		}

	}

	public static void getInconsistencyExplanation(OWLReasoner reasoner, ReasonerFactory factory,

			OWLDataFactory dataFactory, OWLOntology ontology) throws IOException {

		if (!reasoner.isConsistent()) {

			System.out.println("Inconsistency explanation:");

			factory = new Reasoner.ReasonerFactory() {

				protected OWLReasoner createHermiTOWLReasoner(org.semanticweb.HermiT.Configuration configuration,
						OWLOntology ontology) {
					/**
					 * @author nk510 Does not throw an exception in case of inconsistency
					 */

					configuration.throwInconsistentOntologyException = false;

					return new Reasoner(configuration, ontology);
				}
			};

			/**
			 * @author nk510 Next two lines are instantiation of explanation classes.
			 */

			BlackBoxExplanation exp = new BlackBoxExplanation(ontology, factory, reasoner);
			HSTExplanationGenerator multExplanator = new HSTExplanationGenerator(exp);

			/**
			 * @author nk510 Inconsistency sources are OWL axioms, and we save inconsistency
			 *         cases as a set of OWLAxiom.
			 */

			Set<Set<OWLAxiom>> explanations = multExplanator.getExplanations(dataFactory.getOWLThing());

			/**
			 * @author nk510 Iteration over set of OWLAxiom in order to list inconsistency
			 *         explanation.
			 * 
			 */

			for (Set<OWLAxiom> explanation : explanations) {

				for (OWLAxiom axioms : explanation) {

					/**
					 * @author nk510 Inconsistency explanation report is generated to System.out (console)
					 *         (Console).
					 */
					System.out.println(axioms);

				}
			}
		}
	}

	/**
	 * @author nk510
	 * @param folderPath
	 * @return Method reads all ontology files in given folder path. Supported file
	 *         extensions are '.owl', '.rdf', '.ttl'.
	 */
	public static File[] getFileList(String folderPath) {

		File dir = new File(folderPath);

		File[] fileList = dir.listFiles(new FilenameFilter() {
			public boolean accept(File dir, String name) {
				return (name.endsWith(".owl") || name.endsWith(".rdf") || name.endsWith(".ttl"));
			}
		});

		return fileList;
	}

}