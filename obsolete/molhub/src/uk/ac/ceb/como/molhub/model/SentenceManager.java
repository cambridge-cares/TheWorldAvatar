package uk.ac.ceb.como.molhub.model;

import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.log4j.Logger;

import aima.core.logic.propositional.kb.KnowledgeBase;
import aima.core.logic.propositional.kb.data.Clause;
import aima.core.logic.propositional.parsing.ast.Sentence;
import aima.core.logic.propositional.visitors.ConvertToCNF;
import aima.core.logic.propositional.visitors.DistributeOrOverAnd;

/**
 * The Class SentenceManager.
 * 
 *  @author Nenad Krdzavac (caresssd@hermes.cam.ac.uk)
 *  @author Feroz Farazi (msff2@cam.ac.uk)
 */
public class SentenceManager {

	/** The Constant logger. */
	final static Logger logger = Logger.getLogger(SentenceManager.class.getName());
	
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

/**
 * @author nk510
 * @param speciesName The name of species in which we remove appearing a selected number (1) of molecules. Can be easily generalized to remove any number of molecules from species name. 
 * @return The species name without having number one (1) and spaces in species name.
 */
public static String removeNumberAndSpaces(String speciesName) {
	
      logger.info("Original species: " + speciesName);
		
	  speciesName= speciesName.replaceAll("\\s+", "");
		
	  logger.info("Species with no empty spaces: " + speciesName);
		
	  Pattern p = Pattern.compile("\\d+");
		
      Matcher m = p.matcher(speciesName);
       
      String newSpecies = speciesName;
       
      int start =0;
      int end = 0;
       
      while(m.find()) {
       	
      int number = Integer.parseInt(m.group().toString());
       	
      logger.info("Number found in species name : " + number);
       	
      if(number==1) {

      logger.info("m.group(): " + m.group() + " number start at index: " + m.start() + " number ends at index: "  + m.end());

      start = m.start();
      end = m.end();
           
           /**
            * Recursive call of the method. 
            */
           newSpecies = removeNumberAndSpaces(speciesName.substring(0, start) + speciesName.substring(end));

       }
       	
	}
     
     return newSpecies;
     
}

}
