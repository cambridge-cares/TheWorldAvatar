package uk.ac.ceb.como.molhub.action;

import java.util.HashSet;

import java.util.Set;

import org.eclipse.rdf4j.RDF4JException;

import com.opensymphony.xwork2.ActionSupport;

import aima.core.logic.propositional.inference.DPLL;
import aima.core.logic.propositional.inference.DPLLSatisfiable;
import aima.core.logic.propositional.kb.data.Clause;
import aima.core.logic.propositional.parsing.PLParser;
import aima.core.logic.propositional.parsing.ast.PropositionSymbol;
import aima.core.logic.propositional.parsing.ast.Sentence;

import uk.ac.cam.ceb.como.chem.periodictable.Element;
import uk.ac.cam.ceb.como.chem.periodictable.PeriodicTable;
import uk.ac.ceb.como.molhub.bean.MoleculeProperty;
import uk.ac.ceb.como.molhub.bean.Term;
import uk.ac.ceb.como.molhub.model.QueryManager;
import uk.ac.ceb.como.molhub.model.SentenceManager;

/**
 * The Class TermValidationAction.
 */
public class TermValidationAction extends ActionSupport {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1222255700658500383L;

	/** The term. */
	private Term term;

	/** The formula. */
	private String formula;

	/** The satisfiable. */
	private boolean satisfiable;

	/** The periodic table element. */
	private String periodicTableElement;
	
	Set<MoleculeProperty> finalSearchResultSet = new HashSet<MoleculeProperty>();
	
	Set<MoleculeProperty> queryResult;
	
	Set<String> queryResultString;
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see com.opensymphony.xwork2.ActionSupport#execute()
	 */
	@Override
	public String execute() throws Exception {

		PLParser parser = new PLParser();

		DPLL dpll = new DPLLSatisfiable();

		String periodicTableSymbol = null;

		try {

			Sentence sentence = parser.parse(getSearchTerm(term));

			/**
			 * @author nk510 Gets a set of all clauses.
			 */
			Set<Clause> clauseSet = SentenceManager.getClauseSet(sentence);

			/**
			 * 
			 * @author nk510 Iterates over set of clauses. Validation of query string: 1.
			 *         Checks whether each clause starts with one or more letter and ends
			 *         with one or more digit. 2. Removes numbers at the end of each clause
			 *         and checks whether the literal belongs to periodic table.
			 * 
			 */

			for (Clause c : clauseSet) {

				Set<PropositionSymbol> ps = c.getSymbols();

				for (PropositionSymbol ppSymbol : ps) {
					
					/**
					 * @author nk510
					 * Checking whether propositional symbol matches regular expression of periodic table elements.
					 */
					if(!ppSymbol.getSymbol().matches("[A-Z][a-z]{0,3}[0-9]+")) {
						
						addFieldError("term.name", "Propositional letter (" + ppSymbol.getSymbol()
						+ ") does not match the naming of input query string.");

				        return ERROR;
					}else {
						
						/**
						 * @author nk510
						 * Removes appearing all numbers at the end of propositional symbol.
						 */						
						periodicTableSymbol = ppSymbol.getSymbol().replaceAll("[0-9]+","");
					}
					/**
					 * 
					 * @author nk510 Extracts each propositional letter (propositional symbol) in
					 *         each clause and checks whether that symbol is member of periodic
					 *         table. To check whether propositional symbol belongs to period table
					 *         we use <b>{@author pb556}</b> parser.
					 * 
					 */

					Element elementSymbol = PeriodicTable
							.getElementBySymbol(periodicTableSymbol);

					if (elementSymbol.getSymbol() == null) {

						addFieldError("term.name", "There is at least one propositional letter (" + periodicTableSymbol
								+ ") that is not member of periodic table.");

						return ERROR;

					} else {						
						setPeriodicTableElement(elementSymbol.getName());						
					}					
				}
			}

			/**
			 * 
			 * @author nk510 Checks whether input propositional sentence (query string) is
			 *         satisfiable. It si checked by using Davis–Putnam–Logemann–Loveland
			 *         (DPLL) procedure.
			 * 
			 */
			
			setSatisfiable(dpll.dpllSatisfiable(sentence));

			if (dpll.dpllSatisfiable(sentence)) {

				setFormula(getSearchTerm(term));
				
				try {			
					
					queryResultString = new HashSet<String>();
				
					Set<String> listTemp = QueryManager.performQuery(sentence);
					
					for(String mpp: listTemp) {
						
						queryResultString.add(mpp);
					}	
					
				}catch(RDF4JException e) {
					
					addFieldError("term.name", "Query result failed. Explanation: " + e.getMessage());

					return ERROR;
					
				}
				
				if(queryResultString.isEmpty()) {
					
					addFieldError("term.name", "There are not result for given query string. Please, try again.");
				}
				
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

			addFieldError("term.name", "Query string is not propositionally valid sentence. Please try again.");

			return ERROR;
		}

	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see com.opensymphony.xwork2.ActionSupport#validate()
	 */
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

	/**
	 * Gets the term.
	 *
	 * @return the term
	 */
	public Term getTerm() {

		return term;

	}

	/**
	 * Sets the term.
	 *
	 * @param term
	 *            the new term
	 */
	public void setTerm(Term term) {

		this.term = term;

	}

	/**
	 * Gets the formula.
	 *
	 * @return the formula
	 */
	public String getFormula() {
		return formula;
	}

	/**
	 * Sets the formula.
	 *
	 * @param formula
	 *            the new formula
	 */
	public void setFormula(String formula) {

		this.formula = formula;
	}

	/**
	 * Checks if is satisfiable.
	 *
	 * @return true, if is satisfiable
	 */
	public boolean isSatisfiable() {
		return satisfiable;
	}

	/**
	 * Sets the satisfiable.
	 *
	 * @param satisfiable
	 *            the new satisfiable
	 */
	public void setSatisfiable(boolean satisfiable) {
		this.satisfiable = satisfiable;
	}

	/**
	 * Gets the search term.
	 *
	 * @param term
	 *            the term
	 * @return the search term
	 */
	public String getSearchTerm(Term term) {

		String formula = "";

		/**
		 * @author nk510 Converts all letter into lower case.
		 */

		formula = term.getName().replaceAll("and", "&");
		formula = formula.replaceAll("or", "|");
		formula = formula.replaceAll("not", "~");
		formula = formula.replaceAll("implies", "=>");
		formula = formula.replaceAll("equals", "<=>");

		return formula;
	}

	/**
	 * Gets the periodic table element.
	 *
	 * @return the periodic table element
	 */
	public String getPeriodicTableElement() {
		return periodicTableElement;
	}

	/**
	 * Sets the periodic table element.
	 *
	 * @param periodicTableElement
	 *            the new periodic table element
	 */
	public void setPeriodicTableElement(String periodicTableElement) {
		this.periodicTableElement = periodicTableElement;
	}

	public Set<MoleculeProperty> getFinalSearchResultSet() {
		return finalSearchResultSet;
	}

	public void setFinalSearchResultSet(Set<MoleculeProperty> finalSearchResultSet) {
		this.finalSearchResultSet = finalSearchResultSet;
	}

	public Set<MoleculeProperty> getQueryResult() {
		return queryResult;
	}

	public void setQueryResult(Set<MoleculeProperty> queryResult) {
		this.queryResult = queryResult;
	}

	public Set<String> getQueryResultString() {
		return queryResultString;
	}

	public void setQueryResultString(Set<String> queryResultString) {
		this.queryResultString = queryResultString;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	



}