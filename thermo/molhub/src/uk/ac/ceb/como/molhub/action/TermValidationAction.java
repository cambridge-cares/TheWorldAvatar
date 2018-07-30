package uk.ac.ceb.como.molhub.action;

import java.util.Set;

import com.opensymphony.xwork2.ActionSupport;

import aima.core.logic.propositional.inference.DPLL;
import aima.core.logic.propositional.inference.DPLLSatisfiable;
import aima.core.logic.propositional.kb.data.Clause;
import aima.core.logic.propositional.parsing.PLParser;
import aima.core.logic.propositional.parsing.ast.PropositionSymbol;
import aima.core.logic.propositional.parsing.ast.Sentence;
import uk.ac.cam.ceb.como.chem.periodictable.Element;
import uk.ac.cam.ceb.como.chem.periodictable.PeriodicTable;
import uk.ac.ceb.como.molhub.bean.Term;
import uk.ac.ceb.como.molhub.model.SentenceManager;

public class TermValidationAction extends ActionSupport {

	private static final long serialVersionUID = 1222255700658500383L;

	private Term term;

	private String formula;

	private boolean satisfiable;
	
	private String periodicTableElement;

	@Override
	public String execute() throws Exception {

		PLParser parser = new PLParser();

		DPLL dpll = new DPLLSatisfiable();
		
		try {		
			
			Sentence sentence = parser.parse(getSearchTerm(term));			
			
			
			Set<Clause> clauseSet = SentenceManager.getClauseSet(sentence);			

			/**
			 * 
			 * @author nk510
			 * Validation of query string:
			 * 1. Checks whether each clause starts with one or more letter and ends with one or more digit.
			 * 2. Removes numbers at the end of each clause and checks whether the letter belongs to periodic table.
			 *  
			 */
			
			for (Clause c : clauseSet) {

				Set<PropositionSymbol> ps = c.getSymbols();
				
				
				for (PropositionSymbol symbol : ps) {
					
					/**
					 * @author nk510 Extracts each propositional letter in each clause and check whether
					 *         that letter is member of periodic table as a symbol. For
					 *         example, Cl, Fe, H, O etc.
					 */
					Element elementSymbol = PeriodicTable.getElementBySymbol(symbol.getSymbol().replaceAll("[0-9]+$", ""));
					
					System.out.println(elementSymbol.getSymbol());
					
					setPeriodicTableElement(elementSymbol.getSymbol());
					
					if ((elementSymbol.getSymbol() == null)) {

						addFieldError("term.name",
								"There is at least one propositional letter ("+ symbol.toString()  +") that is not member of periodic table.");

						return ERROR;

					}
				
					/**
					 * @author nk510
					 * Checks whether each clause starts with one or more letter and ends with one or more numbers.
					 */
					if(!symbol.getSymbol().matches("([A-Za-z])+([0-9])+")) {
						
						addFieldError("term.name",
								"Query string is not valid sentence.");

						return ERROR;
						
					}
				
				}
			}
			
			
			
			/**
			 * 
			 * @author nk510 Checks whether input propositional sentence is satisfiable. It
			 *         si checked by using Davis–Putnam–Logemann–Loveland (DPLL) procedure.
			 * 
			 */
			setSatisfiable(dpll.dpllSatisfiable(sentence));

			if (dpll.dpllSatisfiable(sentence)) {

				setFormula(getSearchTerm(term));				
				

				// TODO: napraviti posebne klase koje pretrazuju bazu znanja i vracaju rezultat
				// u termvalidation.jsp ili neki Bean koji ce biti prikazan na jsp
				// pitanja za Google: (1) Show <List> data on jsp page.(2) Run query from a file
				// - vidi ConChemQuery.

				return SUCCESS;

			} else {

				addFieldError("term.name", "Query string is not Davis–Putnam–Logemann–Loveland (DPLL) satisfiable.");

				return ERROR;
			}

		} catch (Exception e) {

			/**
			 * 
			 * @author nk510
			 * 
			 *         Checks whether input query string is propositionally valid. For
			 *         example "(P and not P" is not propositionally valid statement.
			 * 
			 */

			addFieldError("term.name", getText("error.term")+ "Explanation: " +  e.getMessage());

			return ERROR;
		}

	}

	public void validate() {

		/**
		 * 
		 * @author nk510 Checks whether input query string is empty.
		 * 
		 */
		if (term.getName().length() == 0) {

			addFieldError("term.name", "Query string is empty.");
		}
	}

	public Term getTerm() {

		return term;

	}

	public void setTerm(Term term) {

		this.term = term;

	}

	public String getFormula() {
		return formula;
	}

	public void setFormula(String formula) {

		this.formula = formula;
	}

	public boolean isSatisfiable() {
		return satisfiable;
	}

	public void setSatisfiable(boolean satisfiable) {
		this.satisfiable = satisfiable;
	}

	public String getSearchTerm(Term term) {

		String formula = "";
		
        /**
         * @author nk510
         * Converts all letter into lower case.
         */
		formula = term.getName().toLowerCase();
		
		formula = formula.replaceAll("and", "&");
		formula = formula.replaceAll("or", "|");
		formula = formula.replaceAll("not", "~");
		formula = formula.replaceAll("implies", "=>");
		formula = formula.replaceAll("euqal", "<=>");

		return formula;
	}

	public String getPeriodicTableElement() {
		return periodicTableElement;
	}

	public void setPeriodicTableElement(String periodicTableElement) {
		this.periodicTableElement = periodicTableElement;
	}	

}