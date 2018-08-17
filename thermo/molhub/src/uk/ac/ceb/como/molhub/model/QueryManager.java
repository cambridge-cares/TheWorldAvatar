package uk.ac.ceb.como.molhub.model;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.QueryLanguage;
import org.eclipse.rdf4j.query.TupleQuery;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.repository.RepositoryConnection;

import aima.core.logic.propositional.kb.data.Clause;
import aima.core.logic.propositional.kb.data.Literal;
import aima.core.logic.propositional.parsing.ast.Sentence;
import uk.ac.cam.ceb.como.io.chem.file.parser.formula.EmpiricalFormulaParser;
import uk.ac.ceb.como.molhub.bean.MoleculeProperty;
import uk.ac.ceb.como.molhub.controler.ConnectionToTripleStore;

public class QueryManager {

	/** The server url. */
	static String serverUrl = "http://localhost:8080/rdf4j-server/repositories/compchemkb";

	static Set<MoleculeProperty> finalSearchResultSet = new HashSet<MoleculeProperty>();

	public static List<MoleculeProperty> performListQuery(Sentence sentence) {

		Set<Clause> clauseSet = SentenceManager.getClauseSet(sentence);

		EmpiricalFormulaParser empiricalFormulaParser = new EmpiricalFormulaParser();

		List<MoleculeProperty> qResult = new ArrayList<MoleculeProperty>();

		try (RepositoryConnection connection = ConnectionToTripleStore.getSPARQLRepositoryConnection(serverUrl)) {

			for (Clause c : clauseSet) {

				if (!c.isTautology()) {

					Set<Literal> literalSet = c.getLiterals();					

					for (Literal literal : literalSet) {

						String queryString = "";
						/**
						 * @author nk510 Returns atom name by parsing each literal in clause. Here we use {@author pb556} parser.
						 */
						String atomName = empiricalFormulaParser.getAtomName(literal.getAtomicSentence().toString());

						/**
						 * @author nk510 Returns number of atoms by parsing literal in clause. Here we use {@author pb556} parser.
						 */
						int numberOfAtoms = empiricalFormulaParser.getAtomSum(literal.getAtomicSentence().toString());
						
						/**
						 * @author nk510
						 * If literal is positive then query manager returns sparql query string that will query those molecule name containing atom name and selected number of atoms.
						 */
						if(literal.isPositiveLiteral()) {

						queryString = QueryManager.getAllTriplesForPositiveLiteral(atomName, numberOfAtoms);
						
						}
						
						if(literal.isNegativeLiteral()) {
						
						queryString = QueryManager.getAllTriplesForNegativeLiteral(atomName, numberOfAtoms);
						
						}
						TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);

						try (TupleQueryResult result = tupleQuery.evaluate()) {

							/**
							 * @author nk510 Evaluates sparql query and populates 'MoleculeProperty' bean.
							 * 
							 */
							while (result.hasNext()) {

								BindingSet bindingSet = result.next();

								MoleculeProperty moleculeProperty = new MoleculeProperty();

								moleculeProperty.setMoleculeName(bindingSet.getValue("name").toString());

								qResult.add(moleculeProperty);
							}

						} catch (Exception e) {

							e.getMessage();
						}
					}
				}
				
			}

			return qResult;
		}
	}

	/**
	 * @author nk510
	 * @param atomName
	 *            name of literal that represents atom name from periodic table
	 * @param atomNumber
	 *            number of atoms which appear in a molecule for give atom name
	 * @return Query as a String. Result of that query should be all molecule names
	 *         which contain atom name and number of atoms for given atom name.
	 */
	
	public static String getAllTriplesForPositiveLiteral(String atomName, int atomNumber) {

		String query = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>"
		        + "PREFIX gc: <http://purl.org/gc/>"
		        + "PREFIX ontochem: <http://ontochem.theworldavatar.com/kb/OntoChem.owl#>"
				+ "SELECT ?name " 
				+ "WHERE { "
                + "?s ontochem:hasInitialization ?in . "
                + "?in gc:hasMoleculeProperty ?mp . "
                + "?mp gc:hasName ?name ."
                + "?mp gc:hasMolecule ?molecule ."
                + "?molecule gc:hasAtom ?atom. "
                + "?atom gc:isElement <http://www.daml.org/2003/01/periodictable/PeriodicTable.owl#" + atomName + ">."
                + "?molecule gc:hasNumberOfAtoms ?n ." 
                + "FILTER(str(?n)='" + atomNumber + "') ."
				+ "}";

		return query;
	}

	public static String getAllTriplesForNegativeLiteral(String atomName, int atomNumber) {

		String query = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>"
		        + "PREFIX gc: <http://purl.org/gc/>"
		        + "PREFIX ontochem: <http://ontochem.theworldavatar.com/kb/OntoChem.owl#>"
				+ "SELECT ?name " 
				+ "WHERE { "
                + "?s ontochem:hasInitialization ?in . "
                + "?in gc:hasMoleculeProperty ?mp . "
                + "?mp gc:hasName ?name ."
                + "FILTER NOT EXISTS { "
                + "?mp gc:hasMolecule ?molecule ."
                + "?molecule gc:hasAtom ?atom. "                
                + "?molecule gc:hasNumberOfAtoms ?n ."
                + "?atom gc:isElement <http://www.daml.org/2003/01/periodictable/PeriodicTable.owl#" + atomName + ">."                
                + "FILTER(str(?n)='" + atomNumber + "') ."
                + "}"
				+ "}";

		return query;
	}

	

	
}