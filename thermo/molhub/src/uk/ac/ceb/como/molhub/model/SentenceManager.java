package uk.ac.ceb.como.molhub.model;
import java.util.Set;

import aima.core.logic.propositional.kb.KnowledgeBase;
import aima.core.logic.propositional.kb.data.Clause;

import aima.core.logic.propositional.parsing.ast.Sentence;
import aima.core.logic.propositional.visitors.ConvertToCNF;
import aima.core.logic.propositional.visitors.DistributeOrOverAnd;

public class SentenceManager {

/**
 * 
 * @author nk510
 * @param sentence Query string.
 * @return A set of clauses for each query string. For example {{~Cl2, H2}, {O1,P2}}.
 * 
 */
public static Set<Clause> getClauseSet(Sentence sentence){
	
	/**
	 * @author nk510
	 * Distribution of logical operation OR over logical operation AND. 
	 */
	DistributeOrOverAnd distributeOrOverAnd = new DistributeOrOverAnd();

	Sentence finalSentence = sentence.accept(distributeOrOverAnd, null);

	/**
	 * @author nk510
	 * Convert sentence into clausal normal form known as conjunctive normal form (CNF).
	 */
	Sentence cnfSentence = ConvertToCNF.convert(finalSentence);	
	
	/**
	 * @author nk510
	 * Create knowledge base of propositional formula.
	 */
	KnowledgeBase kb = new KnowledgeBase();

	kb.tell(cnfSentence);

	Set<Clause> clause = kb.asCNF();
	
	return clause;
	
}

}
