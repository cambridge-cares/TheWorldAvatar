package uk.ac.ceb.como.molhub.model;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.rdf4j.model.Value;
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
	
	public static Set<MoleculeProperty> performQuery(Sentence sentence ) {
   
	Set<Clause> clauseSet = SentenceManager.getClauseSet(sentence);
	
	EmpiricalFormulaParser empiricalFormulaParser = new EmpiricalFormulaParser();	
	
	try (RepositoryConnection connection = ConnectionToTripleStore.getSPARQLRepositoryConnection(serverUrl,"compchemkb")) {
		
		for (Clause c : clauseSet) {
			
//			if(!c.isTautology()) {
				
				Set<Literal> literalSet = c.getLiterals();
				
				Set<MoleculeProperty> literalUnionSet = new HashSet<MoleculeProperty>();
				
				for (Literal literal : literalSet) {
					
					String atomName = empiricalFormulaParser
							.getAtomName(literal.getAtomicSentence().toString());
					int numberOfAtoms = empiricalFormulaParser
							.getNumberOfAllAtoms(literal.getAtomicSentence().toString());
					
					String queryString = QueryManager.getQueryForPositiveLiteral(atomName,numberOfAtoms);

					TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL,queryString);
					
					TupleQueryResult result = tupleQuery.evaluate();
					
					try (TupleQueryResult resultSet = tupleQuery.evaluate()) {
						
						while (resultSet.hasNext()) { // iterate over the result
							
							BindingSet bindingSet = result.next();
							
							Value valueOfG09 = bindingSet.getValue("x");
							Value valueOfMoleculeName = bindingSet.getValue("y");   
							
							MoleculeProperty moleculeProperty = new MoleculeProperty(valueOfG09.toString(), valueOfMoleculeName.toString());
							
							Set<MoleculeProperty> setOfLiterals = new HashSet<MoleculeProperty>();
							
							setOfLiterals.add(moleculeProperty);
						
							literalUnionSet.addAll(setOfLiterals);						
														
						}
						
						
					} catch (Exception e) {
						
						e.getMessage();
					}
					
					
					finalSearchResultSet.retainAll(literalUnionSet);
					
				}
//			}
			
		}
		
		connection.close();
		
		
		return finalSearchResultSet;

	}	
}

public static List<MoleculeProperty> performListQuery(Sentence sentence ) {
	
	Set<Clause> clauseSet = SentenceManager.getClauseSet(sentence);
	
	EmpiricalFormulaParser empiricalFormulaParser = new EmpiricalFormulaParser();
	
	List<MoleculeProperty> qResult = new ArrayList<MoleculeProperty>() ;
	
	try (RepositoryConnection connection = ConnectionToTripleStore.getSPARQLRepositoryConnection(serverUrl,"compchemkb")) {
		
		for (Clause c : clauseSet) {
			
//			if(!c.isTautology()) {
				
				Set<Literal> literalSet = c.getLiterals();
				
				Set<MoleculeProperty> literalUnionSet = new HashSet<MoleculeProperty>();
				
				for (Literal literal : literalSet) {
					
					String atomName = empiricalFormulaParser
							.getAtomName(literal.getAtomicSentence().toString());
					int numberOfAtoms = empiricalFormulaParser
							.getNumberOfAllAtoms(literal.getAtomicSentence().toString());
					
					String queryString = QueryManager.getQueryForPositiveLiteral(atomName,numberOfAtoms);
					
//					String queryString = QueryManager.getQueryForMoleculeName(atomName, numberOfAtoms);
//					String queryString = QueryManager.getQueryForNegativeLiteral(atomName, numberOfAtoms);
					
					TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL,queryString);
					
					TupleQueryResult result = tupleQuery.evaluate();
					
					
					try (TupleQueryResult resultSet = tupleQuery.evaluate()) {
						
						List<String> bindingNames = result.getBindingNames();
						
						while (resultSet.hasNext()) { // iterate over the result
							
							BindingSet bindingSet = result.next();
							
							
							Value valueOfG09 = bindingSet.getValue(bindingNames.get(0));						
							
							Value valueOfMoleculeName = bindingSet.getValue(bindingNames.get(1));
							
							MoleculeProperty moleculeProperty = new MoleculeProperty();
							
							
							moleculeProperty.setMoleculeId(valueOfG09.toString());
							
							moleculeProperty.setMoleculeName(valueOfMoleculeName.toString());
							
							
							Set<MoleculeProperty> setOfLiterals = new HashSet<MoleculeProperty>();
							
							setOfLiterals.add(moleculeProperty);
						
							literalUnionSet.addAll(setOfLiterals);							
							
							qResult.add(moleculeProperty);							
							
						}
						
						
					} catch (Exception e) {
						
						e.getMessage();
					}				
					
					finalSearchResultSet.retainAll(literalUnionSet);
					
				}
//			}
			
		}
		
		return  qResult;

	}
}

public static String getQueryForPositiveLiteral(String atomName, int numberOfAtoms) {

	String queryForPositiveLiteral = "SELECT ?x ?p  WHERE { ?x ?p <http://www.daml.org/2003/01/periodictable/PeriodicTable.owl#"+atomName+">.  }  LIMIT 10";
	
	return queryForPositiveLiteral;
}

public static String getQueryForNegativeLiteral(String atomName, int numberOfAtoms) {
	
	String queryForNegativeLiteral = "PREFIX ontochem: <http://ontochem.theworldavatar.com/kb/OntoChem.owl#>\r\n" + 
			"PREFIX gc: <http://purl.org/gc/>\r\n" + 
			"PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\r\n" + 
			"PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>\r\n" + 
			"SELECT ?g09 ?moleculeName \r\n" + 
			"WHERE { \r\n" + 
			"{\r\n" + 
			"?g09 ontochem:hasInitialization ?mn0 . \r\n" + 
			"?mn0 gc:hasMoleculeProperty ?mp0 . \r\n" + 
			"?mp0 gc:hasName ?moleculeName . \r\n" + 
			"}\r\n" + 
			"UNION\r\n" + 
			"{\r\n" + 
			"?g09 ontochem:hasInitialization ?mn1 . \r\n" + 
			"?mn1 gc:hasMoleculeProperty ?mp1 .  \r\n" + 
			"?mp1 gc:hasMolecule ?mol0 .\r\n" + 
			"?mol0 gc:hasNumberOfAtoms ?numberOfAtoms .\r\n" + 
			"?mol0 gc:hasAtom ?at0 . \r\n" + 
			"?at0 gc:isElement <http://www.daml.org/2003/01/periodictable/PeriodicTable.owl#Cl> . \r\n" + 
			"}\r\n" +

			"}";
	
	return queryForNegativeLiteral;
}
//"FILTER(regex(str(?atomName),http://www.daml.org/2003/01/periodictable/PeriodicTable.owl#"+atomName+"))\r\n" +			 
public static String getQueryForMoleculeName(String atomName, int atomNumber) {
	
	String queryForNegativeLiteral = "PREFIX ontochem: <http://ontochem.theworldavatar.com/kb/OntoChem.owl#>\r\n" + 
			"PREFIX gc: <http://purl.org/gc/>\r\n" + 
			"PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\r\n" + 
			"PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>\r\n" + 
			"SELECT ?g09 ?moleculeName \r\n" + 
			"WHERE { \r\n" + 
			"{\r\n" + 
			"?g09 ontochem:hasInitialization ?mn0 . \r\n" + 
			"?mn0 gc:hasMoleculeProperty ?mp0 . \r\n" + 
			"?mp0 gc:hasName ?moleculeName . \r\n" + 
			"}\r\n" + 
			"}"
			+ "LIMIT 10";
	
	return queryForNegativeLiteral;
}




}
