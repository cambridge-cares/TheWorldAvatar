package uk.ac.ceb.como.molhub.model;

import org.apache.log4j.Logger;

import java.io.IOException;
import java.util.ArrayList;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.QueryLanguage;
import org.eclipse.rdf4j.query.TupleQuery;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.repository.RepositoryConnection;

import com.google.common.collect.Sets;

import aima.core.logic.propositional.kb.data.Clause;
import aima.core.logic.propositional.kb.data.Literal;
import aima.core.logic.propositional.parsing.ast.Sentence;
import uk.ac.cam.ceb.como.io.chem.file.parser.formula.EmpiricalFormulaParser;
import uk.ac.ceb.como.molhub.bean.AtomicMass;
import uk.ac.ceb.como.molhub.bean.Frequency;
import uk.ac.ceb.como.molhub.bean.MoleculeProperty;
import uk.ac.ceb.como.molhub.bean.QueryString;
import uk.ac.ceb.como.molhub.bean.RotationalConstant;
import uk.ac.ceb.como.molhub.controler.ConnectionToTripleStore;

public class QueryManager {

	/** The server url. */
	static String serverUrl = "http://localhost:8080/rdf4j-server/repositories/compchemkb";

	final static Logger logger = Logger.getLogger(QueryManager.class.getName());

	static Set<String> result = new HashSet<String>();

	/**
	 * 
	 * @param sentence
	 *            input query string given as propositional formula. Each literal in
	 *            this query string contains atom name and number of atoms.
	 * @return a list of molecule names as a result of sparql queries on RDF4J
	 *         triple store.
	 * @throws IOException
	 */
	public static Set<String> performSPARQLQueryOnQueryString(Sentence sentence) throws IOException {

		/**
		 * @author nk510 List of Strings that represents final solution of querying
		 *         triple store by using input string (propositional formula).
		 */
		List<Set<String>> listMoleculeNameSet = new ArrayList<Set<String>>();
		/**
		 * @author nk510 Set of all clauses built based on input query.
		 */
		Set<Clause> clauseSet = SentenceManager.getClauseSet(sentence);

		/**
		 * @author nk510 Here we use Philip's parser for empirical formula.
		 */
		EmpiricalFormulaParser empiricalFormulaParser = new EmpiricalFormulaParser();

		try (RepositoryConnection connection = ConnectionToTripleStore.getSPARQLRepositoryConnection(serverUrl)) {

			int step = 0;

			for (Clause c : clauseSet) {

				step++;

				// if (!c.isTautology()) {

				Set<String> setB = new HashSet<String>();

				Set<Literal> literalSet = c.getLiterals();

				for (Literal literal : literalSet) {

					String queryString = "";

					/**
					 * @author nk510 Returns atom name by parsing each literal in clause. Here we
					 *         use {@author pb556} parser.
					 */
					String atomName = empiricalFormulaParser.getAtomName(literal.getAtomicSentence().toString());

					/**
					 * @author nk510 Returns number of atoms by parsing literal in clause. Here we
					 *         use {@author pb556} parser.
					 */
					int numberOfAtoms = empiricalFormulaParser.getAtomSum(literal.getAtomicSentence().toString());

					/**
					 * @author nk510 If literal is positive then query manager returns sparql query
					 *         string that will query those molecule name containing selected atom
					 *         name and selected number of atoms.
					 */
					if (literal.isPositiveLiteral()) {

						queryString = QueryString.getAllTriplesForPositiveLiteral(atomName, numberOfAtoms);
					}

					/**
					 * @author nk510 If literal is negative then query manager returns sparql query
					 *         string that will query those molecule name not containing selected
					 *         atom name and selected number of atoms.
					 */

					if (literal.isNegativeLiteral()) {

						queryString = QueryString.getAllTriplesForNegativeLiteral(atomName, numberOfAtoms);

					}

					TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);

					try (TupleQueryResult result = tupleQuery.evaluate()) {

						/**
						 * 
						 * @author nk510 Evaluates sparql query and populates 'MoleculeProperty' beans.
						 * 
						 */

						Set<String> setA = new HashSet<String>();

						while (result.hasNext()) {

							BindingSet bindingSet = result.next();

							/**
							 * 
							 * @author nk510 Add all results into a set for one literal .
							 * 
							 */

							setA.add(bindingSet.getValue("name").toString());
						}

						setB.addAll(setA);

					} catch (Exception e) {

						e.getMessage();
					}
				}

				if (clauseSet.size() <= 1) {

					return setB;
				}

				listMoleculeNameSet.add(setB);
				// }
			}

			/**
			 * @author nk510 Calculates intersection of all results for all clauses
			 *         (unions).
			 * 
			 */
			return intersection(listMoleculeNameSet);
		}
	}

	/**
	 * 
	 * @author nk510
	 * @param moleculeName
	 *            a name of a molecule
	 * @return for given molecule name sparql returns uuid, level of theory, and
	 *         basis set.
	 * 
	 */
	public static List<MoleculeProperty> performSPARQLForMoleculeName(String moleculeName) {

		List<MoleculeProperty> moleculePropertyList = new ArrayList<MoleculeProperty>();

		String queryString = QueryString.getAllTriplesMoleculeProperty(moleculeName);

		try (RepositoryConnection connection = ConnectionToTripleStore.getSPARQLRepositoryConnection(serverUrl)) {

			TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);

			try (TupleQueryResult result = tupleQuery.evaluate()) {

				while (result.hasNext()) {

					BindingSet bindingSet = result.next();

					MoleculeProperty moleculeProperty = new MoleculeProperty(bindingSet.getValue("uuid").stringValue(),
							moleculeName, bindingSet.getValue("levelOfTheory").toString(),
							bindingSet.getValue("basisSetValue").toString());

					moleculePropertyList.add(moleculeProperty);
				}

			} catch (Exception e) {

				e.getMessage();
			}

		}

		return moleculePropertyList;

	}

	/**
	 * 
	 * @author nk510
	 * @param listOfSets
	 *            a list that contains sets of all unions in all clauses.
	 * @return intersection of all clauses as a set of strings.
	 * 
	 */
	public static Set<String> intersection(List<Set<String>> listOfSets) {

		Set<String> finalSet = new HashSet<String>(listOfSets.get(0));

		logger.info("step " + 0 + ". final set before intersection:");

		for (String mps : finalSet) {

			logger.info("final set molecule name: " + mps);
		}

		for (int i = 1; i < listOfSets.size(); i++) {

			logger.info("set. no. " + i + " is empty: " + listOfSets.get(i).isEmpty());

			for (String mps : listOfSets.get(i)) {

				logger.info("listOfSets.get(1), molecule name: " + mps);
			}

			finalSet = Sets.intersection(finalSet, listOfSets.get(i));

			logger.info("step " + i + ". final set after intersection :");

			for (String mps : finalSet) {

				logger.info("final set molecule name: " + mps);
			}
		}

		return finalSet;
	}

	/**
	 * @author nk510
	 * @param uuid
	 *            unique folder name.
	 * @return a list of all frequencies for given uuid.
	 * 
	 */
	public static List<Frequency> getAllFrequencies(String uuid) {

		List<Frequency> frequencyList = new ArrayList<Frequency>();

		String queryString = QueryString.geFrequency(uuid);

		logger.info("queryString (frequency): " + queryString);

		try (RepositoryConnection connection = ConnectionToTripleStore.getSPARQLRepositoryConnection(serverUrl)) {

			TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);

			try (TupleQueryResult result = tupleQuery.evaluate()) {

				while (result.hasNext()) {

					BindingSet bindingSet = result.next();

					Frequency frequency = new Frequency(bindingSet.getValue("frequenciesSize").stringValue(),
							bindingSet.getValue("frequenciesValue").stringValue(),
							bindingSet.getValue("frequenciesUnit").stringValue());

					frequencyList.add(frequency);
				}

			}
		}

		return frequencyList;

	}

	public static List<MoleculeProperty> getAllNonCompositetMoleculeProperties(String uuid) {

		String queryString = QueryString.geNonCompositetMoleculeProperties(uuid);

		List<MoleculeProperty> moleculePropertyList = new ArrayList<MoleculeProperty>();

		logger.info("queryString (nonComposite): " + queryString);

		try (RepositoryConnection connection = ConnectionToTripleStore.getSPARQLRepositoryConnection(serverUrl)) {

			TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);

			try (TupleQueryResult result = tupleQuery.evaluate()) {

				while (result.hasNext()) {

					BindingSet bindingSet = result.next();

					MoleculeProperty moleculeProperty = new MoleculeProperty(uuid,
							bindingSet.getValue("moleculeName").stringValue(),
							bindingSet.getValue("basisSetValue").stringValue(),
							bindingSet.getValue("levelOfTheory").stringValue(),
							bindingSet.getValue("geometryTypeValue").stringValue());

					moleculePropertyList.add(moleculeProperty);

				}
			}
		}
		return moleculePropertyList;
	}

	public static String getAllRotationalSymmertyNumber(String uuid) {

		String queryString = QueryString.getRotationalSymmertyNumber(uuid);

		String rotationalSymmetryNumber = new String();

		try (RepositoryConnection connection = ConnectionToTripleStore.getSPARQLRepositoryConnection(serverUrl)) {

			TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);

			try (TupleQueryResult result = tupleQuery.evaluate()) {

				while (result.hasNext()) {

					BindingSet bindingSet = result.next();

					rotationalSymmetryNumber = bindingSet.getValue("rotationalSymmetryNumber").stringValue();

				}
			}
		}
		return rotationalSymmetryNumber;
	}

	public static String getAllSpinMultiplicity(String uuid) {

		String queryString = QueryString.getSpinMultiplicity(uuid);

		String spinMultiplicityValue = new String();

		try (RepositoryConnection connection = ConnectionToTripleStore.getSPARQLRepositoryConnection(serverUrl)) {

			TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);

			try (TupleQueryResult result = tupleQuery.evaluate()) {

				while (result.hasNext()) {

					BindingSet bindingSet = result.next();

					spinMultiplicityValue = bindingSet.getValue("spinMultiplicityValue").stringValue();

				}
			}
		}
		return spinMultiplicityValue;
	}

	public static List<AtomicMass> getAllAtomicMass(String uuid) {

		List<AtomicMass> atomicMassList = new ArrayList<AtomicMass>();

		String queryString = QueryString.getAtomicMass(uuid);

		logger.info("queryString (frequency): " + queryString);

		try (RepositoryConnection connection = ConnectionToTripleStore.getSPARQLRepositoryConnection(serverUrl)) {

			TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);

			try (TupleQueryResult result = tupleQuery.evaluate()) {

				while (result.hasNext()) {

					BindingSet bindingSet = result.next();

					AtomicMass atomicMass = new AtomicMass(bindingSet.getValue("atomicName").stringValue(),
							bindingSet.getValue("massValue").stringValue(),
							bindingSet.getValue("massUnit").stringValue());

					atomicMassList.add(atomicMass);
				}

			}
		}

		return atomicMassList;

	}
	
	public static List<RotationalConstant> getAllRotationalConstant(String uuid) {

		List<RotationalConstant> rotationalConstantList = new ArrayList<RotationalConstant>();

		String queryString = QueryString.getRotationalConstant(uuid);

		try (RepositoryConnection connection = ConnectionToTripleStore.getSPARQLRepositoryConnection(serverUrl)) {

			TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);

			try (TupleQueryResult result = tupleQuery.evaluate()) {

				while (result.hasNext()) {

					BindingSet bindingSet = result.next();

					RotationalConstant rotationalConstant = new RotationalConstant(bindingSet.getValue("rotationalConstantsSize").stringValue(),
							bindingSet.getValue("rotationalConstantsValue").stringValue(),
							bindingSet.getValue("rotationalConstantsUnit").stringValue());

					rotationalConstantList.add(rotationalConstant);
				}

			}
		}

		return rotationalConstantList;

	}
	

}