# Developing a SPARQL Inferencing Notation (SPIN) Project
### Authors
* [Nenad Krdzavac](caresssd@hermes.cam.ac.uk)
* [Feroz Farazi](msff2@cam.ac.uk)

SPIN is an industry-standard to represent SPARQL rules and constraints on Semantic Web models [[1](https://spinrdf.org)]. This short document aims to describe the steps required to develop a SPIN project using Java.

### Create a Maven project and add dependencies

In the development of this SPIN project, you will employ [SPIN - SPARQL syntax](https://www.w3.org/Submission/spin-sparql/). Create a Maven project and include the following dependencies in the pom.xml file to enable the use of different SPIN related features supported by this software.
 
        <!-- https://mvnrepository.com/artifact/org.topbraid/spin -->
        <dependency>
        <groupId>org.topbraid</groupId>
        <artifactId>spin</artifactId>
        <version>2.0.0</version>
        </dependency>

        <!-- https://mvnrepository.com/artifact/org.apache.jena/jena-core -->
        <dependency>
        <groupId>org.apache.jena</groupId>
        <artifactId>jena-core</artifactId>
        <version>3.1.0</version>
        </dependency>

        <!-- https://mvnrepository.com/artifact/org.apache.jena/jena-arq -->
        <dependency>
        <groupId>org.apache.jena</groupId>
        <artifactId>jena-arq</artifactId>
        <version>3.1.0</version>
        </dependency>

The dependencies provided above are collected from the following Maven repositories.

* [SPIN API](https://mvnrepository.com/artifact/org.topbraid/spin)
* [Jena Core](https://mvnrepository.com/artifact/org.apache.jena/jena-core)
* [Jena ARQ](https://mvnrepository.com/artifact/org.apache.jena/jena-arq) 


### Apply relevant ontologies

In general, three ontologies are required to develop a SPIN project. a) [SPIN](http://spinrdf.org/spin) is the common ontology used across all SPIN projects or applications; b) the target ontology on top of which rules and constraints are written; and c) the rule and constraint ontology that contains rules and constraints created or reused in the project. 

You can either provide the URLs of ontologies or copy them into a specific folder, as described below. In this implementation, URLs are used.

* Copy the *kennedys.ttl* ontology from [here](http://topbraid.org/examples/kennedys) into the *resources* (/resources) folder in the Maven project you created.

* Copy the *kennedysSPIN.ttl* ontology from [here](http://topbraid.org/examples/kennedysSPIN) into the same folder as above.

You will notice that

* The *kennedys.ttl* ontology is imported into the *kennedysSPIN.ttl* ontology.
* The [spin.ttl](http://spinrdf.org/spin) ontology is imported into the *kennedysSPIN.ttl* ontology.

If you use different target and rule and constraint ontologies, it is important to remember that these ontologies' expressivity should not exceed the capability of [OWL2 RL](https://www.w3.org/TR/owl2-profiles/#OWL_2_RL). If the ontologies contain only simple class hierarchies, object properties, and data properties as provided in the spin.ttl, kennedys.ttl, and kennedysSPIN.ttl ontologies, they are OWL2 RL compatible.


### Create SPIN rules

To create SPIN rules, use the [TopBraid Composer](https://www.topquadrant.com) editor. Each SPIN rule is attached to one or more ontology classes/properties using the *spin:rule* construct. The following SPIN rule is used by the SPIN inference engine to infer the age of a person based on values of the property *kennedys:birthYear*. It takes the current year and performs a subtraction operation between the current year and birth year to infer the age of a particular person (an instance of the ontology class *kennedys:Person*). The resulted age is stored as the value of the *kspin:age* property.

	 CONSTRUCT {
       ?this kspin:age ?age .
    }
    WHERE {
    ?this kennedys:birthYear ?birthYear .
    BIND (kspin:getCurrentYear() AS ?currentYear) .
    BIND ((?currentYear - ?birthYear) AS ?age) .
    }


### Create SPIN constraints

To model SPIN constraints, use *spin:constraint*. The SPIN inference engine uses SPIN constraints to detect whether all members of a class fulfil certain conditions. The following constraint is attached to the ontology class *kennedys:Person*. It checks whether each instance of the class has age outside of the range [0,120].  


	ASK WHERE {
    ?this kspin:age ?age .
    FILTER ((?age < 0) || (?age > 120)) .
    }

### Develop and run Java code

Create a Java class with the name ConstraintViolations under the package uk.ac.ceb.como.spin, remove all content from the class file, copy and paste the following code into this empty file, and run it as a Java Application.

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
 