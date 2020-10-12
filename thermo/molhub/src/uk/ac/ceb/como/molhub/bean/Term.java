package uk.ac.ceb.como.molhub.bean;

// TODO: Auto-generated Javadoc
/**
 * The Class Term.
 *
 * <p>Represents a query term that will be used in search box. For
 *         example: "(H2 and O) or Cl2"</p>
 *         
 *  @author Nenad Krdzavac (caresssd@hermes.cam.ac.uk)
 *  @author Feroz Farazi (msff2@cam.ac.uk)
 */
public class Term {

	/** Query string name. For example ((not Cl2 or Fe3) and (O2 or H2)) */
	private String name;

	/**
	 * @author nk510
	 * <p>Conjunctive normal form of input term (query string name). For example: (not Cl2
	 * or Fe3) and (O2 or H2).</p>
	 */
	private String cnf;

	/**
	 * Gets the cnf (Conjunctive normal form).
	 *
	 * @return the cnf. <p> Represents conjunctive normal form of input query given as name in this class. </p>
	 */
	public String getCnf() {
		return cnf;
	}

	/**
	 * Sets the cnf.
	 *
	 * @param cnf the new cnf 
	 */
	public void setCnf(String cnf) {
		this.cnf = cnf;
	}

	/**
	 * Gets the name.
	 * @author nk510
	 * @return the name. <p> Represents input query as a String. </p>
	 */
	public String getName() {
		return name;
	}

	/**
	 * Sets the name.
	 *
	 * @param name the new name. <p> Represents input query as a String. </p>
	 */
	public void setName(String name) {
		this.name = name;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {

		return getName();
	}

}