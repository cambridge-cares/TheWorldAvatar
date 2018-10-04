package uk.ac.ceb.como.molhub.model;

import java.util.Set;

import aima.core.logic.propositional.kb.KnowledgeBase;
import aima.core.logic.propositional.kb.data.Clause;

import aima.core.logic.propositional.parsing.ast.Sentence;
import aima.core.logic.propositional.visitors.ConvertToCNF;
import aima.core.logic.propositional.visitors.DistributeOrOverAnd;

/**
 * The Class SentenceManager.
 */
public class SentenceManager {

/**
 * Gets the clause set.
 *
 * @author nk510
 * @param sentence Query string.
 * @return <p>A set of clauses for each query string. For example {{ not Cl2, H2}, {O1,P2}}.</p>
 */
public static Set<Clause> getClauseSet(Sentence sentence){
	
	/**
	 * @author nk510
	 * <p>Distributes of logical operation OR over logical operation AND.</p> 
	 */
	DistributeOrOverAnd distributeOrOverAnd = new DistributeOrOverAnd();

	Sentence finalSentence = sentence.accept(distributeOrOverAnd, null);
	/**
	 * @author nk510
	 * <p>Converts sentence into conjunctive normal form (CNF).</p>
	 */
	Sentence cnfSentence = ConvertToCNF.convert(finalSentence);	
	
	/**
	 * @author nk510
	 * <p>Creates knowledge base by using propositional formula.</p>
	 */
	KnowledgeBase kb = new KnowledgeBase();

	kb.tell(cnfSentence);

	Set<Clause> clause =kb.asCNF();
	
	return clause;
	
}

}
