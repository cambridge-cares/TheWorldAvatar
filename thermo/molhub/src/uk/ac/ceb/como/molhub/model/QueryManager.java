package uk.ac.ceb.como.molhub.model;

import org.apache.log4j.Logger;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.rdf4j.IsolationLevels;
import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.QueryLanguage;
import org.eclipse.rdf4j.query.TupleQuery;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;
import org.eclipse.rdf4j.repository.http.HTTPRepository;

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


/**
 * @author nk510
 * The Class QueryManager. Implements methods for query RDF4J repository. 
 */
public class QueryManager {

	/** The server url. */
	static String serverUrl = "http://localhost:8080/rdf4j-server/repositories/compchemkb";

	/** The Constant logger. */
	final static Logger logger = Logger.getLogger(QueryManager.class.getName());

	/**
	 * Perform SPARQL query on query string.
	 *
	 * @param sentence
	 *            input query string given as propositional formula. Each literal in
	 *            this query string contains atom name and number of atoms.
	 * @return a list of molecule names as a result of sparql queries on RDF4J
	 *         triple store.
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	public static Set<String> performSPARQLQueryOnQueryString(Sentence sentence) throws IOException {
//	public static Map<String,String> performSPARQLQueryOnQueryString(Sentence sentence) throws IOException {
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
		
        Repository repository =  new HTTPRepository(serverUrl); 
		
		repository.initialize();
		
		RepositoryConnection connection = repository.getConnection();
		
		try {

			connection.begin(IsolationLevels.SNAPSHOT_READ);

			int step = 0;

			for (Clause c : clauseSet) {

				step++;

//				HashSet<String> setB = new HashSet<String>();
				
				Map<String,String> setB = new HashMap<String,String>();

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

					TupleQueryResult result = tupleQuery.evaluate();

					try {

						/**
						 * 
						 * @author nk510 Evaluates sparql query and populates 'MoleculeProperty' beans.
						 * 
						 */

//						HashSet<String> setA = new HashSet<String>();

						Map<String,String> setA = new HashMap<String, String>();
						
						while (result.hasNext()) {

							BindingSet bindingSet = result.next();

							/**
							 * 
							 * @author nk510 Add all results into a set for one literal .
							 * 
							 */

//							setA.add(bindingSet.getValue("name").toString());
							setA.put(bindingSet.getValue("uuid").toString(), bindingSet.getValue("name").toString());
						}
						
						

//						setB.addAll(setA);
						
						setB.putAll(setA);
						
						connection.commit();

					} catch (Exception e) {

						logger.info(e.getMessage());

					} finally {
						
						result.close();
					}
				}
				
				
				HashSet<String> setBa = new HashSet<String>();
				
				for (String set : setB.values()) {
					
					setBa.add(set);
				}
				

				if (clauseSet.size() <= 1) {

//					return setB;
					return setBa;
				}

//				listMoleculeNameSet.add(setB);
				
				listMoleculeNameSet.add(setBa);
				
			}
			
		} catch(RepositoryException e) {
			
			logger.info("Repository Exception: " + e.getMessage());
			
			connection.rollback();
			
		}finally {

			
			connection.close();
			
			repository.shutDown();

		}
		/**
		 * 
		 * @author nk510 Calculates intersection of all results for all clauses
		 *         (unions).
		 * 
		 */

		return intersection(listMoleculeNameSet);

	}

	/**
	 * Perform SPARQL for molecule name.
	 *
	 * @author nk510
	 * @param moleculeName
	 *            a name of a molecule
	 * @return for given molecule name sparql returns uuid, level of theory, and
	 *         basis set.
	 */
	public static Map<String,MoleculeProperty> performSPARQLForMoleculeName(String moleculeName) {
//	public static Set<MoleculeProperty> performSPARQLForMoleculeName(String moleculeName) {

//		Set<MoleculeProperty> moleculePropertyList = new HashSet<MoleculeProperty>();

		Map<String,MoleculeProperty> moleculePropertyList = new HashMap<String,MoleculeProperty>();
		
		String queryString = QueryString.getAllTriplesMoleculeProperty(moleculeName);

        Repository repository =  new HTTPRepository(serverUrl); 
			
		repository.initialize();
			
	    RepositoryConnection connection = repository.getConnection();
	    
		try {

			connection.begin(IsolationLevels.SNAPSHOT_READ);
			
			TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);
			
			TupleQueryResult result = tupleQuery.evaluate();

			try {

				while (result.hasNext()) {

					BindingSet bindingSet = result.next();

					MoleculeProperty moleculeProperty = new MoleculeProperty(bindingSet.getValue("uuid").stringValue(),
							moleculeName, bindingSet.getValue("levelOfTheory").toString(),
							bindingSet.getValue("basisSetValue").toString());

					moleculePropertyList.put(bindingSet.getValue("uuid").stringValue(),moleculeProperty);
				}

			} catch (Exception e) {

				e.getMessage();
				
			} finally {

				result.close();
			}
			

			connection.commit();

		} catch (RepositoryException e) {

			logger.info(e.getMessage());
			
			connection.rollback();
			
		} finally {

			connection.close();
			
			repository.shutDown();

		}

		return moleculePropertyList;

	}

	/**
	 * Intersection.
	 *
	 * @author nk510
	 * @param listOfSets
	 *            a list that contains sets of all unions in all clauses.
	 * @return intersection of all clauses as a set of strings.
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
	 * Gets the all frequencies.
	 *
	 * @author nk510
	 * @param uuid
	 *            unique folder name.
	 * @return a list of all frequencies (size, value, unit) for given uuid.
	 */
	public static List<Frequency> getAllFrequencies(String uuid) {

		List<Frequency> frequencyList = new ArrayList<Frequency>();

		String queryString = QueryString.geFrequency(uuid);
		
		Repository repository =  new HTTPRepository(serverUrl); 
		
		repository.initialize();
		
		RepositoryConnection connection = repository.getConnection();

		try {

			connection.begin(IsolationLevels.SNAPSHOT_READ);
			
			TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);

			TupleQueryResult result = tupleQuery.evaluate();

			try {

				while (result.hasNext()) {

					BindingSet bindingSet = result.next();

					Frequency frequency = new Frequency(bindingSet.getValue("frequenciesSize").stringValue(),
							bindingSet.getValue("frequenciesValue").stringValue(),
							bindingSet.getValue("frequenciesUnit").stringValue());

					frequencyList.add(frequency);
				}
				
				connection.commit();

			} catch (Exception e) {

				logger.info(e.getMessage());
				
			} finally {

				result.close();
			}

			
			
		} catch (RepositoryException e) {
			
			logger.info(e.getMessage());
			
			connection.rollback();
			
		} finally {
			
			connection.close();
			
			repository.shutDown();
		}

		return frequencyList;

	}

	/**
	 * Gets the all non compositet molecule properties.
	 *
	 * @param uuid
	 *            the uuid is name for unique folder name
	 * @return the all non compositet molecule properties
	 */
	public static List<MoleculeProperty> getAllNonCompositetMoleculeProperties(String uuid) {

		String queryString = QueryString.geNonCompositetMoleculeProperties(uuid);

		List<MoleculeProperty> moleculePropertyList = new ArrayList<MoleculeProperty>();
		
		Repository repository =  new HTTPRepository(serverUrl); 
		
		repository.initialize();
		
		RepositoryConnection connection = repository.getConnection();

		try {

			connection.begin(IsolationLevels.SNAPSHOT_READ);
			
			TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);

			TupleQueryResult result = tupleQuery.evaluate();

			try {

				while (result.hasNext()) {

					BindingSet bindingSet = result.next();

					MoleculeProperty moleculeProperty = new MoleculeProperty(uuid,
							bindingSet.getValue("moleculeName").stringValue(),
							bindingSet.getValue("basisSetValue").stringValue(),
							bindingSet.getValue("levelOfTheory").stringValue(),
							bindingSet.getValue("geometryTypeValue").stringValue());

					moleculePropertyList.add(moleculeProperty);

				}
				
				connection.commit();
				
			} catch (Exception e) {

				logger.info(e.getMessage());

			} finally {
				
				result.close();
				
			}			
			
		} catch (RepositoryException e) {

			logger.info(e.getMessage());
			
			connection.rollback();
			
		} finally {
			
			connection.close();
			
			repository.shutDown();
		}

		return moleculePropertyList;
	}

	/**
	 * Gets the all rotational symmerty number.
	 *
	 * @param uuid
	 *            the uuid is name for unique folder
	 * @return the all rotational symmerty number
	 */
	public static String getAllRotationalSymmertyNumber(String uuid) {

		String queryString = QueryString.getRotationalSymmertyNumber(uuid);

		String rotationalSymmetryNumber = new String();
		
		Repository repository =  new HTTPRepository(serverUrl); 
		
		repository.initialize();
		
		RepositoryConnection connection = repository.getConnection();

		try {

			connection.begin(IsolationLevels.SNAPSHOT_READ);
			
			TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);

			TupleQueryResult result = tupleQuery.evaluate();

			try {

				while (result.hasNext()) {

					BindingSet bindingSet = result.next();

					rotationalSymmetryNumber = bindingSet.getValue("rotationalSymmetryNumber").stringValue();

				}
				
				connection.commit();
				

			} catch (Exception e) {

				logger.info(e.getMessage());
				
			} finally {
				
				result.close();
			}

			
			
			
		} catch (RepositoryException e) {

			logger.info(e.getMessage());
			
			connection.rollback();
			
		} finally {

			connection.close();
			
			repository.shutDown();

		}

		return rotationalSymmetryNumber;
	}

	/**
	 * Gets the all spin multiplicity.
	 *
	 * @param uuid
	 *            the uuid is name for unique folder
	 * @return the all spin multiplicity value
	 */
	public static String getAllSpinMultiplicity(String uuid) {

		String queryString = QueryString.getSpinMultiplicity(uuid);

		String spinMultiplicityValue = new String();
		
		Repository repository =  new HTTPRepository(serverUrl); 
		
		repository.initialize();
		
		RepositoryConnection connection = repository.getConnection();
		

		try {

			connection.begin(IsolationLevels.SNAPSHOT_READ);
			
			TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);

			TupleQueryResult result = tupleQuery.evaluate();

			try {

				while (result.hasNext()) {

					BindingSet bindingSet = result.next();

					spinMultiplicityValue = bindingSet.getValue("spinMultiplicityValue").stringValue();

				}
				
				connection.commit();

			} catch (Exception e) {

				logger.info(e.getMessage());

			} finally {

				result.close();
			}
			
			
			
		} catch (RepositoryException e) {

			logger.info(e.getMessage());
			
			connection.rollback();

		} finally {

			connection.close();
			
			repository.shutDown();
		}

		return spinMultiplicityValue;
	}

	/**
	 * Gets the all atomic mass.
	 *
	 * @param uuid
	 *            the uuid is name for unique folder
	 * @return the all atomic mass (atom name, atomic mass value, atomic mass unit)
	 */
	public static List<AtomicMass> getAllAtomicMass(String uuid) {

		List<AtomicMass> atomicMassList = new ArrayList<AtomicMass>();

		String queryString = QueryString.getAtomicMass(uuid);
		
		Repository repository =  new HTTPRepository(serverUrl); 
		
		repository.initialize();
		
		RepositoryConnection connection = repository.getConnection();
		

		try {

			connection.begin(IsolationLevels.SNAPSHOT_READ);
			
			TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);

			TupleQueryResult result = tupleQuery.evaluate();

			try {

				while (result.hasNext()) {

					BindingSet bindingSet = result.next();

					AtomicMass atomicMass = new AtomicMass(bindingSet.getValue("atomicName").stringValue(),
							bindingSet.getValue("massValue").stringValue(),
							bindingSet.getValue("massUnit").stringValue());

					atomicMassList.add(atomicMass);
				}
				
				connection.commit();

			} catch (Exception e) {

				logger.info(e.getMessage());

			} finally {

				result.close();
			}
			
			


		} catch (RepositoryException e) {

			logger.info(e.getMessage());
			
			connection.rollback();

		} finally {

			connection.close();
			
			repository.shutDown();
		}

		return atomicMassList;

	}

	/**
	 * Gets the all rotational constant.
	 *
	 * @param uuid
	 *            the uuid is name for unique folder.
	 * @return the all rotational constant (rotational constant size, rotational
	 *         constant value, rotational constant unit).
	 */
	public static List<RotationalConstant> getAllRotationalConstant(String uuid) {

		List<RotationalConstant> rotationalConstantList = new ArrayList<RotationalConstant>();

		String queryString = QueryString.getRotationalConstant(uuid);
		
		Repository repository =  new HTTPRepository(serverUrl); 
		
		repository.initialize();
		
		RepositoryConnection connection = repository.getConnection();

		try {

			connection.begin(IsolationLevels.SNAPSHOT_READ);

			TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);

			TupleQueryResult result = tupleQuery.evaluate();

			try {

				while (result.hasNext()) {

					BindingSet bindingSet = result.next();

					RotationalConstant rotationalConstant = new RotationalConstant(
							bindingSet.getValue("rotationalConstantsSize").stringValue(),
							bindingSet.getValue("rotationalConstantsValue").stringValue(),
							bindingSet.getValue("rotationalConstantsUnit").stringValue());

					rotationalConstantList.add(rotationalConstant);
				}

				
				connection.commit();
				
			} catch (Exception e) {

				logger.info(e.getMessage());
				
			} finally {
				
				result.close();
				
			}

		} catch (RepositoryException e) {

			logger.info(e.getMessage());
			
			connection.rollback();

		} finally {
			
			connection.close();
			
			repository.shutDown();

		}

		return rotationalConstantList;
	}
}