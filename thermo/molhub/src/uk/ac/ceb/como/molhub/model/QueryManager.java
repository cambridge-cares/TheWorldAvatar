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
					
					/**
					 * @author nk510
					 * ova metoda ne vraca broj atoma za dati atomski broj nego daje zbir svih atoma u molekulu.
					 */
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
	
	String numberOfAtom = String.valueOf(numberOfAtoms);
	
	String queryForPositiveLiteral = "SELECT ?numberOfAtoms ?mname  WHERE { ?g <http://ontochem.theworldavatar.com/kb/OntoChem.owl#hasInitialization> ?mn . ?mn <http://purl.org/gc/hasMoleculeProperty> ?mp . ?mp <http://purl.org/gc/hasName> ?mname . ?mp <http://purl.org/gc/hasMolecule> ?mol . ?mol <http://purl.org/gc/hasNumberOfAtoms> ?numberOfAtoms .  ?mol <http://purl.org/gc/hasAtom> ?x . ?x <http://purl.org/gc/isElement> <http://www.daml.org/2003/01/periodictable/PeriodicTable.owl#"+atomName+">. FILTER(str(?numberOfAtoms)='"+numberOfAtom+"')} LIMIT 50";
	
	return queryForPositiveLiteral;
}

public static String getQueryForNegativeLiteral(String atomName, int numberOfAtoms) {
	
	atomName= atomName.toString().replaceAll("\\s+","");
	
	String queryForNegativeLiteral = "SELECT ?g09 ?mname" 
			+"WHERE {"   
			+"?g09 <http://ontochem.theworldavatar.com/kb/OntoChem.owl#hasInitialization> ?mn ."  
			+"?mn <http://purl.org/gc/hasMoleculeProperty> ?mp .  " 
			+"?mp <http://purl.org/gc/hasName> ?mname ." 
			+"?mp <http://purl.org/gc/hasMolecule> ?mol ." 			
			+"?mol <http://purl.org/gc/hasNumberOfAtoms> ?nAtoms."  
			+"?mol <http://purl.org/gc/hasAtom> ?at ." 
			+"?at <http://purl.org/gc/isElement> <http://www.daml.org/2003/01/periodictable/PeriodicTable.owl#"+atomName+">."  
			+"}"
			+ "LIMIT 10";
	
	return queryForNegativeLiteral;
}
//"FILTER(regex(str(?atomName),http://www.daml.org/2003/01/periodictable/PeriodicTable.owl#"+atomName+"))\r\n" +
//"?mp1 gc:hasName ?moleculeName . " + 

public static String getQueryForMoleculeName(String atomName, int atomNumber) {
	
	String queryForNegativeLiteral = "SELECT ?g09 ?mname"
			+ " WHERE { "
			+ "?g09 <http://ontochem.theworldavatar.com/kb/OntoChem.owl#hasInitialization> ?mn . "
			+ "?mn <http://purl.org/gc/hasMoleculeProperty> ?mp . "
			+ "?mp <http://purl.org/gc/hasName> ?mname ."
			+ "}"
			+ "LIMIT 20";
	
	return queryForNegativeLiteral;
}




}
