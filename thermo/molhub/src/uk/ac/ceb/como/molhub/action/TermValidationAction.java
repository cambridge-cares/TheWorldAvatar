package uk.ac.ceb.como.molhub.action;

import com.opensymphony.xwork2.ActionSupport;

import uk.ac.ceb.como.molhub.bean.Term;

public class TermValidationAction extends ActionSupport {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1222255700658500383L;
	
	private Term term;
	
	public String execute() throws Exception {
		
		return SUCCESS;
	}
	

	public void validate() {
		if(term.getName().length()==0) {
			addFieldError( "term.name", "Query term is empty." );
		}
		
	}
	public Term getTerm() {
		return term;
	}

	public void setTerm(Term term) {
		this.term = term;
	}
	
	
}
