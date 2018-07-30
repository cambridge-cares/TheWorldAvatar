package uk.ac.ceb.como.molhub.model;
import java.util.Set;

import aima.core.logic.propositional.inference.DPLL;
import aima.core.logic.propositional.kb.KnowledgeBase;
import aima.core.logic.propositional.kb.data.Clause;
import aima.core.logic.propositional.parsing.PLParser;
import aima.core.logic.propositional.parsing.ast.PropositionSymbol;
import aima.core.logic.propositional.parsing.ast.Sentence;

public class SentenceManager {

/**
 * 
 * @author nk510
 * @param sentence Query string.
 * @return A set of clauses for each query string. For example {~Cl2, H2}, {O1,P2}.
 * 
 */
public static Set<Clause> getClauseSet(Sentence sentence){
	
	KnowledgeBase kb = new KnowledgeBase();

	kb.tell(sentence);

	Set<Clause> clause = kb.asCNF();
	
	return clause;
	
}


}
