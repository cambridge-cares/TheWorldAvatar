package uk.ac.ceb.como.molhub.action;

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.log4j.Logger;
import org.apache.struts2.dispatcher.SessionMap;
import org.apache.struts2.interceptor.SessionAware;

import com.opensymphony.xwork2.ActionSupport;
import com.opensymphony.xwork2.ValidationAware;

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
 *
 * 
 *         <p>
 * 		Implements methods for querying RDF4J repository by using
 *         propositional logic formulas. Each literal in query string consists
 *         of atom name (as given in Periodic table) and number of atoms
 *         appearing in molecule name.
 *         </p>
 *         
 *  @author Nenad Krdzavac (caresssd@hermes.cam.ac.uk)
 *  @author Feroz Farazi (msff2@cam.ac.uk)
 */

public class TermValidationLogAction extends ActionSupport implements SessionAware, ValidationAware {
	
	
	/** The Constant logger. */
	final static Logger logger = Logger.getLogger(TermValidationLogAction.class.getName());

	/** The start time. */
	final long startTime = System.currentTimeMillis();

	/** The running time. */
	private String runningTime = null;

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1222255700658500383L;

	/** The term. Represents a query term that will be used in search box */
	private Term term;

	/** The formula. For example, Cl2. */
	private String formula;

	/** The satisfiable. Denotes whether input string added into search box is DPLL satisfiable or not.*/
	private boolean satisfiable;

	/** The periodic table element. */
	private String periodicTableElement;

	/** The final search result set. */
	private Set<MoleculeProperty> finalSearchResultSet = new HashSet<MoleculeProperty>();

	/** The query result string. */
	private Set<String> queryResultString;
	
	/** The results column. */
	List<String> resultsColumn = new ArrayList<String>();

	/** The session. */
	private Map<String, Object> session = new HashMap<String, Object>();
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see com.opensymphony.xwork2.ActionSupport#execute()
	 * 
	 */
	@Override
	public String execute() throws Exception {
		
        /**
         * Propositional logic parser used together with Philipp's parser to parse input string in search box 
         */
		PLParser parser = new PLParser();

		/**
		 * Calculates satisfiability of query added into search text.
		 */
		DPLL dpll = new DPLLSatisfiable();

		String periodicTableSymbol = null;

		resultsColumn.add("UUID:");
		resultsColumn.add("URL:");
		resultsColumn.add("Empirical Formula:");
		resultsColumn.add("Basis Set:");
		resultsColumn.add("Method: ");

		/**
		 * 
		 *         <p>
		 * 		This part of code is executing when a user presses button "Search Knowledge Graph".
		 *         </p>
		 */
		if ((term.getName().length() == 0) || (term == null)) {

			if (!session.isEmpty()) {

				for (Map.Entry<String, Object> mp : session.entrySet()) {

					session.remove(mp.getKey(), mp.getValue());
				}

			}

			addFieldError("term.name", "Query string is empty.");

			return ERROR;
		}

		if (!session.isEmpty()) {

			session.clear();
		}

		try {

			Sentence sentence = parser.parse(getSearchTerm(term));

			/**
			 * @author nk510
			 *         <p>
			 * 		Gets a set of all clauses.
			 *         </p>
			 */
			
			Set<Clause> clauseSet = SentenceManager.getClauseSet(sentence);

			/**
			 * 
			 * @author nk510
			 *         <p>
			 * 		Iterates over set of clauses. Validation of query string: 1. Checks
			 *         whether each clause starts with one or more letter and ends with one
			 *         or more digit. 2. Removes numbers at the end of each clause and
			 *         checks whether the literal belongs to periodic table.
			 *         </p>
			 * 
			 */

			for (Clause c : clauseSet) {

				Set<PropositionSymbol> ps = c.getSymbols();

				for (PropositionSymbol ppSymbol : ps) {

					/**
					 * @author nk510
					 *         <p>
					 * 		Checking whether propositional symbol matches regular expression of
					 *         periodic table elements.
					 *         </p>
					 */
					
					if (!ppSymbol.getSymbol().matches("[A-Z][a-z]{0,3}[0-9]+")) {

						addFieldError("term.name", "Propositional letter (" + ppSymbol.getSymbol()
								+ ") does not match the naming of input query string.");

						return ERROR;
						
					} else {

						/**
						 * @author nk510 Removes appearing all numbers at the end of propositional
						 *         symbol.
						 */
						
						periodicTableSymbol = ppSymbol.getSymbol().replaceAll("[0-9]+", "");
					}
					
					/**
					 * 
					 * @author nk510
					 *         <p>
					 * 		Extracts each propositional letter (propositional symbol) in each
					 *         clause and checks whether that symbol is member of periodic table. To
					 *         check whether propositional symbol belongs to period table we use
					 *         <b>{@author pb556}</b> parser.
					 *         </p>
					 * 
					 */

					Element elementSymbol = PeriodicTable.getElementBySymbol(periodicTableSymbol);

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
			 * @author nk510
			 *         <p>
			 * 		Checks whether input propositional sentence (query string) is
			 *         satisfiable. It is checked by using Davis-Putnam-Logemann-Loveland
			 *         (DPLL) procedure.
			 *         </p>
			 * 
			 */

			setSatisfiable(dpll.dpllSatisfiable(sentence));

			if (dpll.dpllSatisfiable(sentence)) {

				setFormula(getSearchTerm(term));

				try {

					queryResultString = new HashSet<String>();

					queryResultString =  QueryManager.performSPARQLQueryOnQueryString(sentence);

					for (String mpp : queryResultString) {
						
						logger.info("mpp: " + mpp);

						/**
						 * @author nk510
						 *         <p>
						 * 		Returns list of all molecule properties which will appear in query
						 *         result. It remembers also image file name (PNG file).
						 *         </p>
						 */

						Set<MoleculeProperty> setMoleculeProperty = new HashSet<MoleculeProperty>();

						setMoleculeProperty = QueryManager.performSPARQLForMoleculeName(mpp);
						
						for(MoleculeProperty mp: setMoleculeProperty) {
							
							logger.info("mp.getUuid(): " + mp.getUuid() + " mp.getLevelOfTheory(): " + mp.getLevelOfTheory()+ " mp.getBasisSet(): " + mp.getBasisSet() );
						}

						/**
						 * @author nk510
						 *         <p>
						 * 		Adds result in final search result set as Java Set of
						 *         MoleculeProperties.
						 *         </p>
						 */
						
						finalSearchResultSet.addAll(setMoleculeProperty);

					}

					/**
					 * 
					 * @author nk510
					 *         <p>
					 * 		Adding search results as 2-tuple (uuid, molecule name) into session
					 *         to be used by {@link uk.ac.ceb.como.molhub.action.CalculationAction}.
					 *         </p>
					 *         
					 */
					
					for (MoleculeProperty mp : finalSearchResultSet) {

						session.put(mp.getUuid(), mp.getMoleculeName());
						
						logger.info("mp.getUuid(): " + mp.getUuid() + "  mp.getMoleculeName(): " +  mp.getMoleculeName());

					}
					

					NumberFormat formatter = new DecimalFormat("#00.00");

					final long endTime = System.currentTimeMillis();

					runningTime = formatter.format((endTime - startTime) / 1000d) + " seconds";

				} catch (Exception e) {

					addFieldError("term.name", "Query result failed. Explanation: " + e.getMessage());

					return ERROR;
				}

				if (queryResultString.isEmpty()) {

					addFieldError("term.name", "There are no results for given query string. Please, try again.");
				}

				return SUCCESS;

			} else {

				addFieldError("term.name", "Query string is not Davis-Putnam-Logemann-Loveland (DPLL) satisfiable.");

				return ERROR;
			}

		} catch (Exception e) {

			/**
			 * 
			 * @author nk510
			 * 
			 *         <p>
			 * 		Checks whether input query string is propositionally valid. For
			 *         example "(P and not P" is not propositionally valid statement.
			 *         </p>
			 * 
			 */

			addFieldError("term.name", "Query string is not propositionally valid sentence. Please try again.");

			return ERROR;
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.opensymphony.xwork2.ActionSupport#input()
	 */
	@Override
	public String input() {

		getFinalSearchResultSet();

		return INPUT;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.opensymphony.xwork2.ActionSupport#validate()
	 */

	public void validate() {

		/**
		 * 
		 * @author nk510
		 *         <p>
		 * 		Checks whether input query string is empty.
		 *         </p>
		 * 
		 */
		if (term.getName().length() == 0) {

			addFieldError("term.name", "Query string is empty.");
		}
	}

	/**
	 * Gets the term.
	 *
	 * @return the term represents a query string as a propositional logic formula.
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
		 * @author nk510
		 *         <p>
		 * 		Converts all logical operations letter into lower case keywords. Both
		 *         notations can be used equally in search engine.
		 *         </p>
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

	/**
	 * Gets the final search result set.
	 *
	 * @return the final search result set
	 */
	public Set<MoleculeProperty> getFinalSearchResultSet() {
		return finalSearchResultSet;
	}

	/**
	 * Sets the final search result set.
	 *
	 * @param finalSearchResultSet
	 *            the new final search result set
	 */
	public void setFinalSearchResultSet(Set<MoleculeProperty> finalSearchResultSet) {
		this.finalSearchResultSet = finalSearchResultSet;
	}

	/**
	 * Gets the query result string.
	 *
	 * @return the query result string
	 */
	public Set<String> getQueryResultString() {
		return queryResultString;
	}

	/**
	 * Sets the query result string.
	 *
	 * @param queryResultString
	 *            the new query result string
	 */
	public void setQueryResultString(Set<String> queryResultString) {
		this.queryResultString = queryResultString;
	}

	/**
	 * Gets the serialversionuid.
	 *
	 * @return the serialversionuid
	 */
	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	/**
	 * Gets the results column.
	 *
	 * @return the results column
	 */
	public List<String> getResultsColumn() {
		return resultsColumn;
	}

	/**
	 * Sets the results column.
	 *
	 * @param resultsColumn
	 *            the new results column
	 */
	public void setResultsColumn(List<String> resultsColumn) {
		this.resultsColumn = resultsColumn;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.apache.struts2.interceptor.SessionAware#setSession(java.util.Map)
	 */
	@Override
	public void setSession(Map<String, Object> session) {
		
		this.session = (SessionMap<String, Object>) session;
	}

	/**
	 * Gets the session.
	 *
	 * @return the session
	 */
	public Map<String, Object> getSession() {

		return session;
	}

	/**
	 * Gets the running time.
	 *
	 * @return
	 *         <p>
	 * 		The running time needed to query JPS Knowledge Graph and show
	 *         results.
	 *         </p>
	 */
	public String getRunningTime() {
		return runningTime;
	}

	/**
	 * Sets the running time.
	 *
	 * @param runningTime
	 *            the new running time
	 */
	public void setRunningTime(String runningTime) {
		this.runningTime = runningTime;
	}
}