package uk.ac.ceb.como.spin;

import java.util.List;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;

import org.topbraid.spin.constraints.ConstraintViolation;
import org.topbraid.spin.constraints.SPINConstraints;
import org.topbraid.spin.inference.SPINInferences;
import org.topbraid.spin.system.SPINLabels;
import org.topbraid.spin.system.SPINModuleRegistry;

/**
 * This code is adapted from Kennedys's example borrowed from SPIN example
 * library created by @author Holger Knublauch. It loads the Kennedys SPIN
 * ontology and runs inferences and then constraint checks on it.
 * 
 * @author NK510 (caresssd@hermes.cam.ac.uk)
 * @author msff2 (msff2@cam.ac.uk)
 * 
 */

public class ConstraintViolations {

	static String ontologyURL = "http://topbraid.org/examples/kennedysSPIN";

	public static void main(String[] args) {

		/**
		 * Initialize SPIN system functions and templates.
		 */
		SPINModuleRegistry.get().init();

		/**
		 * Create ontology Model with imports
		 */
		OntModel ontModel = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM,
				new ConstraintViolations().getBaseModel(ontologyURL));

		Model inferrenceTriples = new ConstraintViolations().inferredTriples(ontModel);		
		/**
		 * Lists inferred triples.
		 */
		new ConstraintViolations().getInferredTriples(inferrenceTriples);
		
		/**
		 * Lists all constraint violations.
		 */
		new ConstraintViolations().getAllConstraintViolations(ontModel);

	}

	public Model getBaseModel(String ontologyURL) {

		/**
		 * Load ontology file to Jena model.
		 */
		Model baseModel = ModelFactory.createDefaultModel();
		baseModel.read(ontologyURL);

		return baseModel;
	}

	public Model inferredTriples(OntModel ontologyModel) {
		/**
		 * Add a Model to inferred triples
		 */
		Model inferredTriples = ModelFactory.createDefaultModel();
		ontologyModel.addSubModel(inferredTriples);

		/**
		 * Register locally defined functions
		 */
		SPINModuleRegistry.get().registerAll((org.apache.jena.rdf.model.Model) ontologyModel, null);

		/**
		 * Run SPIN inference engine to infer new triples
		 */
		SPINInferences.run(ontologyModel, inferredTriples, null, null, false, null);

		return inferredTriples;

	}

	/**
	 * 
	 * @param inferrenceTriples model that represents inferred triples.
	 */
	public void getInferredTriples(Model inferrenceTriples) {

		System.out.println("Inferred triples size: " + inferrenceTriples.size());

		System.out.println("Prints inferred triples: ");

		StmtIterator stmtIterator = inferrenceTriples.listStatements();

		int i = 1;
		while (stmtIterator.hasNext()) {

			Statement statement = stmtIterator.next();

			System.out.println(i + ". ( " + statement.getSubject().getLocalName() + " , "
					+ statement.getPredicate().getLocalName() + " , " + statement.getObject().toString() + " )");
			i++;
		}

	}

	/**
	 * Lists all constraint violations
	 * 
	 * @param ontModel the ontology model
	 */
	public void getAllConstraintViolations(OntModel ontModel) {

		/**
		 * Run all constraints
		 */

		List<ConstraintViolation> cvs = SPINConstraints.check(ontModel, null);
		System.out.println("Constraint violations:");
		for (ConstraintViolation cv : cvs) {
			System.out.println(" - at " + SPINLabels.get().getLabel(cv.getRoot()) + ": " + cv.getMessage());
		}

		/**
		 * Run constraints on a single instance only
		 */
		Resource person = cvs.get(0).getRoot();
		List<ConstraintViolation> localCVS = SPINConstraints.check(person, null);
		System.out.println("Constraint violations for " + SPINLabels.get().getLabel(person) + ": " + localCVS.size());

	}

}
