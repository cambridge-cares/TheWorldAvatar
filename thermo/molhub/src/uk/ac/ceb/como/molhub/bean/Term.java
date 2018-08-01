package uk.ac.ceb.como.molhub.bean;

/**
 * 
 * @author nk510 Represents a query term that will be used in search box. For
 *         example: "(H2 and O) or Cl2"
 */
public class Term {

	/**
	 * Query string name
	 */
	private String name;

	/**
	 * Conjunctive normal form of input term (query string). For example: (not Cl2
	 * or Fe3) and (O2 or H2).
	 */
	private String cnf;

	public String getCnf() {
		return cnf;
	}

	public void setCnf(String cnf) {
		this.cnf = cnf;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String toString() {

		return getName();
	}

}