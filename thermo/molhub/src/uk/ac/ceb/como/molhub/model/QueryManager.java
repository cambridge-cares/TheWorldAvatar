package uk.ac.ceb.como.molhub.model;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;

import org.apache.log4j.Logger;
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
import uk.ac.ceb.como.molhub.bean.ElectronicEnergy;
import uk.ac.ceb.como.molhub.bean.FormalCharge;
import uk.ac.ceb.como.molhub.bean.Frequency;
import uk.ac.ceb.como.molhub.bean.MoleculeProperty;
import uk.ac.ceb.como.molhub.bean.QueryString;
import uk.ac.ceb.como.molhub.bean.RotationalConstant;

/**
 * 
 * The Class QueryManager.
 *
 * 
 *         <p>
 *         The Class QueryManager. Implements methods for query remote RDF4J triple store.
 *         </p>
 *         
 *  @author Nenad Krdzavac (caresssd@hermes.cam.ac.uk)
 *  @author Feroz Farazi (msff2@cam.ac.uk)
 * 
 */

public class QueryManager {

	/** The Constant logger. */
	final static Logger logger = Logger.getLogger(QueryManager.class.getName());
	
	private static Properties kbProperties = PropertiesManager.loadProperties(QueryManager.class.getClassLoader().getResourceAsStream("kb.ontocompchem.management.properties"));
	private static String serverUrl = kbProperties.getProperty("ontocompchem.kb.local.rdf4j.server.url");
	
	/**
	 * <p>
	 * Runs SPARQL query.
	 * </p>
	 *
	 * @param sentence
	 *                 <p>
	 *                 Input query string given as propositional formula. Each
	 *                 literal in this query string contains atom name and number of
	 *                 atoms.
	 *                 </p>
	 * @return a list of molecule names as a result of SPARQL queries over RDF4J triple store
	 *
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	public static Set<String> performSPARQLQueryOnQueryString(Sentence sentence) throws IOException {

		
		
		/**
		 * @author nk510
		 *         <p>
		 *         List of Strings that represents final solution of querying triple
		 *         store by using input string (propositional logic formula).
		 *         </p>
		 */
		List<Set<String>> listMoleculeNameSet = new ArrayList<Set<String>>();

		/**
		 * @author nk510
		 *         <p>
		 *         Set of all clauses built based on input query.
		 *         </p>
		 */

		Set<Clause> clauseSet = SentenceManager.getClauseSet(sentence);

		/**
		 * @author nk510
		 *         <p>
		 *         Here we use Philip's parser for empirical formula.
		 *         </p>
		 */
		EmpiricalFormulaParser empiricalFormulaParser = new EmpiricalFormulaParser();

		Repository repository = new HTTPRepository(serverUrl);

		repository.initialize();

		RepositoryConnection connection = repository.getConnection();

		try {

			connection.begin();

//			int step = 0;

			for (Clause c : clauseSet) {

				HashSet<String> setB = new HashSet<String>();

				Set<Literal> literalSet = c.getLiterals();

				for (Literal literal : literalSet) {

					String queryString = "";

					/**
					 * @author nk510
					 *         <p>
					 *         Returns atom name by parsing each literal in clause. Here we use
					 *         {@author pb556} parser.
					 *         </p>
					 */
					String atomName = empiricalFormulaParser.getAtomName(literal.getAtomicSentence().toString());

					/**
					 * @author nk510
					 *         <p>
					 *         Returns number of atoms by parsing literal in clause. Here we use
					 *         {@author pb556} parser.
					 *         </p>
					 */
					int numberOfAtoms = empiricalFormulaParser.getAtomSum(literal.getAtomicSentence().toString());

					/**
					 * @author nk510
					 *         <p>
					 *         If literal is positive then query manager returns SPARQL query string
					 *         that will query those molecule name containing selected atom name and
					 *         selected number of atoms.
					 *         </p>
					 */

					if (literal.isPositiveLiteral()) {

						queryString = QueryString.getAllTriplesForPositiveLiteral(atomName, numberOfAtoms);
					}

					/**
					 * @author nk510
					 *         <p>
					 *         If literal is negative then query manager returns SPARQL query string
					 *         that will query those molecule name not containing selected atom name
					 *         and selected number of atoms.
					 *         </p>
					 */

					if (literal.isNegativeLiteral()) {

						queryString = QueryString.getAllTriplesForNegativeLiteral(atomName, numberOfAtoms);

					}

					TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);

					TupleQueryResult result = tupleQuery.evaluate();

					try {

						/**
						 * 
						 * @author nk510
						 *         <p>
						 *         Evaluates sparql query and populates 'MoleculeProperty' beans.
						 *         </p>
						 * 
						 */

						HashSet<String> setA = new HashSet<String>();

						while (result.hasNext()) {

							BindingSet bindingSet = result.next();

							/**
							 * 
							 * @author nk510
							 *         <p>
							 *         Add all query results for one literal into a set.
							 *         </p>
							 * 
							 */

							setA.add(bindingSet.getValue("name").toString());

						}

						setB.addAll(setA);

						connection.commit();

					} catch (Exception e) {

						logger.info(e.getMessage());

					} finally {

						result.close();
					}
				} // literalSet

				if (clauseSet.size() <= 1) {

					return setB;

				}

				listMoleculeNameSet.add(setB);

			} // clauseSet

		} catch (RepositoryException e) {

			logger.info("Repository Exception: " + e.getMessage());

			connection.rollback();

		} finally {

			connection.close();

			repository.shutDown();

		}

		/**
		 * 
		 * @author nk510
		 *         <p>
		 *         Calculates intersection of all results for all clauses (unions).
		 *         </p>
		 * 
		 */

		return intersection(listMoleculeNameSet);

	}

	/**
	 * Perform SPARQL for molecule name.
	 *
	 * @author nk510
	 * @param moleculeName a name of a molecule
	 * @return a Java Set.
	 *         <p>
	 *         For given molecule name SPARQL returns UUID, level of theory, and
	 *         basis set.
	 *         </p>
	 */

	public static Set<MoleculeProperty> performSPARQLForMoleculeName(String moleculeName) {

		Set<MoleculeProperty> moleculePropertyList = new HashSet<MoleculeProperty>();

		String queryString = QueryString.getAllTriplesMoleculeProperty(moleculeName);

		Repository repository = new HTTPRepository(serverUrl);

		repository.initialize();

		RepositoryConnection connection = repository.getConnection();

		try {

			connection.begin(IsolationLevels.SNAPSHOT_READ);

			TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);

			TupleQueryResult result = tupleQuery.evaluate();

			try {

				while (result.hasNext()) {

					BindingSet bindingSet = result.next();			
					
					String uuIdentifider = bindingSet.getValue("uuid").stringValue();
					
					String uuid = uuIdentifider.split("#")[1];
					
					String fileNameId = uuIdentifider.split("#")[0];
					
					String uniqueIdentifier = fileNameId.split("/")[1];

					MoleculeProperty moleculeProperty = new MoleculeProperty(uuid, uniqueIdentifier, 
//							moleculeName,
							/**
							 * @author nk510
							 * It removes any occurrence of "1" and removes  spaces in the resulting output string of species name.
							 */
							SentenceManager.removeNumberAndSpaces(moleculeName),
							bindingSet.getValue("levelOfTheory").toString(),
							bindingSet.getValue("basisSetValue").toString());

					moleculePropertyList.add(moleculeProperty);
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
	 *                   <p>
	 *                   A list that contains sets of all unions in all clauses.
	 *                   </p>
	 * @return a Java Set.
	 *         <p>
	 *         Intersection of all clauses as sets of strings.
	 *         </p>
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
	 * Gets the all frequencies
	 * 
	 * @param uuid
	 * @param uuidFile
	 * @return A Java List.
	 *         <p>
	 *         A list of all frequencies (size, value, unit) for given UUID.
	 *         </p>

	 */
	public static List<Frequency> getAllFrequencies(String uuid, String uuidFile) {

		List<Frequency> frequencyList = new ArrayList<Frequency>();

		String queryString = QueryString.geFrequency(uuid, uuidFile);

		Repository repository = new HTTPRepository(serverUrl);

		repository.initialize();

		RepositoryConnection connection = repository.getConnection();

		try {

			connection.begin(IsolationLevels.SNAPSHOT_READ);

			TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);

			TupleQueryResult result = tupleQuery.evaluate();

			try {

				while (result.hasNext()) {

					BindingSet bindingSet = result.next();
					
					logger.info("frequency: " + bindingSet.getValue("frequenciesSize").stringValue() +" " + bindingSet.getValue("frequenciesValue").stringValue() +" " + bindingSet.getValue("frequenciesUnit").stringValue());

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
	 * 
	 * Gets the all non compositet molecule properties.
	 *
	 * @param uuid the UUID is name for unique folder name
	 * @param uuidFile the UUID of uploaded OWL file
	 * @return A Java List.
	 *         <p>
	 *         all non composite molecule properties . Non composite molecule
	 *         properties includes: molecule name, basis set value, level of theory,
	 *         and geometry type.
	 *         </p>
	 * 
	 */

	public static List<MoleculeProperty> getAllNonCompositetMoleculeProperties(String uuid, String uuidFile) {

		String queryString = QueryString.geNonCompositetMoleculeProperties(uuid, uuidFile);

		List<MoleculeProperty> moleculePropertyList = new ArrayList<MoleculeProperty>();

		Repository repository = new HTTPRepository(serverUrl);

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
							/**
							 * @author nk510
							 * It removes any occurrence of "1" and removes spaces in the resulting output string for species name.
							 */
							SentenceManager.removeNumberAndSpaces(bindingSet.getValue("moleculeName").stringValue()),
							bindingSet.getValue("basisSetValue").stringValue(),
							bindingSet.getValue("levelOfTheory").stringValue(),
							bindingSet.getValue("geometryTypeValue").stringValue(),bindingSet.getValue("mn0").stringValue());

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
	 * 
	 * Gets the all rotational symmerty number.
	 *
	 * @param uuid the UUID is name for unique folder name.
	 * @return the rotational symmerty number.
	 * 
	 */
	public static String getAllRotationalSymmertyNumber(String uuid, String uuidFile) {

		String queryString = QueryString.getRotationalSymmertyNumber(uuid, uuidFile);

		String rotationalSymmetryNumber = new String();

		Repository repository = new HTTPRepository(serverUrl);

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
	 * 
	 * Gets the all spin multiplicity.
	 *
	 * @param uuid the UUID is name for unique folder name.
	 * @return the spin multiplicity value.
	 * 
	 */

	public static String getAllSpinMultiplicity(String uuid, String uuidFile) {

		String queryString = QueryString.getSpinMultiplicity(uuid, uuidFile);

		String spinMultiplicityValue = new String();

		Repository repository = new HTTPRepository(serverUrl);

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
	 * 
	 * Gets the all formal charge.
	 *
	 * @param uuid the UUID denotes unique folder name
	 * @return the all formal charge value
	 * 
	 */

	public static List<FormalCharge> getAllFormalCharge(String uuid, String uuidFile) {

		List<FormalCharge> formalChargeList = new ArrayList<FormalCharge>();

		String queryString = QueryString.getFormalCharge(uuid, uuidFile);

		Repository repository = new HTTPRepository(serverUrl);

		repository.initialize();

		RepositoryConnection connection = repository.getConnection();

		try {

			connection.begin(IsolationLevels.SNAPSHOT_READ);

			TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);

			TupleQueryResult result = tupleQuery.evaluate();

			try {

				while (result.hasNext()) {

					BindingSet bindingSet = result.next();

					FormalCharge formalCharge = new FormalCharge(bindingSet.getValue("formalChargeValue").stringValue(),
							bindingSet.getValue("formalChargeUnit").stringValue());

					formalChargeList.add(formalCharge);
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

		return formalChargeList;
	}

	/**
	 * 
	 * Gets the all atomic mass.
	 *
	 * @param uuid the UUID denotes unique folder name.
	 * @return the atomic masses.
	 *         <p>
	 *         These data are given as the following 3-tuple (atom name, atomic mass
	 *         value, atomic mass unit).
	 *         </p>
	 * 
	 */
	public static List<AtomicMass> getAllAtomicMass(String uuid, String uuidFile) {

		List<AtomicMass> atomicMassList = new ArrayList<AtomicMass>();

		String queryString = QueryString.getAtomicMass(uuid, uuidFile);

		Repository repository = new HTTPRepository(serverUrl);

		repository.initialize();

		RepositoryConnection connection = repository.getConnection();

		try {

			connection.begin(IsolationLevels.SNAPSHOT_READ);

			TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);

			TupleQueryResult result = tupleQuery.evaluate();

			try {

				while (result.hasNext()) {

					BindingSet bindingSet = result.next();
					
					if((bindingSet.getValue("massUnit").stringValue()!="") || (bindingSet.getValue("massUnit").stringValue()!=null)) {

					AtomicMass atomicMass = new AtomicMass(bindingSet.getValue("atomicName").stringValue(),
							bindingSet.getValue("massValue").stringValue(),
							bindingSet.getValue("massUnit").stringValue());

					atomicMassList.add(atomicMass);
					} 
					
					if((bindingSet.getValue("massUnit").stringValue()=="") || (bindingSet.getValue("massUnit").stringValue()==null)) {	
						
						AtomicMass atomicMass = new AtomicMass(bindingSet.getValue("atomicName").stringValue(),
								bindingSet.getValue("massValue").stringValue());

						atomicMassList.add(atomicMass);
						
					}
					
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
	 * 
	 * Gets the all rotational constant.
	 * 
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 *
	 * @param uuid the UUID denotes unique folder name.
	 * @param uuidFile the UUID of owl file used in IRIs
	 * @return the rotational constant.
	 *         <p>
	 *         These data are given as the following 3-tuple (rotational constant
	 *         size, rotational constant value, rotational constant unit).
	 *         </p>
	 * 
	 */

	public static List<RotationalConstant> getAllRotationalConstant(String uuid, String uuidFile) {

		List<RotationalConstant> rotationalConstantList = new ArrayList<RotationalConstant>();

		String queryString = QueryString.getRotationalConstant(uuid, uuidFile);

		Repository repository = new HTTPRepository(serverUrl);

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
	
	
	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param uuid  the UUID stands for unique folder name.
	 * @param electronicEnergyClass different type of electronic energy classes such as ScfEnergy, ZeroPointEnergy.
	 * @return The List of electronic energy.
	 */
	public static List<ElectronicEnergy> getElectronicEnergy(String uuid, String uuidFile, String electronicEnergyClass){
		
		
		List<ElectronicEnergy> electronicEnergyList = new ArrayList<ElectronicEnergy>();
		
		String queryString = QueryString.getElectronicEnergy(uuid, uuidFile,electronicEnergyClass);
		
		
		Repository repository = new HTTPRepository(serverUrl);

		repository.initialize();

		RepositoryConnection connection = repository.getConnection();

		try {

			connection.begin(IsolationLevels.SNAPSHOT_READ);

			TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);

			TupleQueryResult result = tupleQuery.evaluate();

			try {

				while (result.hasNext()) {

					BindingSet bindingSet = result.next();

					ElectronicEnergy electronicEnergy = new ElectronicEnergy(							
							bindingSet.getValue("energyValue").stringValue(),
							bindingSet.getValue("energyUnit").stringValue());

					electronicEnergyList.add(electronicEnergy);
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
		
		
		return electronicEnergyList;
		
		
	}
}