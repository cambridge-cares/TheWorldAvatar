package uk.ac.ceb.como.action;

import com.opensymphony.xwork2.ActionSupport;

import uk.ac.ceb.como.query.QueryManager;
import uk.ac.ceb.como.query.QueryString;

/**
 * 
 * @author NK
 *
 *         Show a simple statistical data about number of Gaussian calculations,
 *         number of species, number of reactions, and number of reaction
 *         mechanisms.
 *
 */
public class StatisticsAction extends ActionSupport {

	private static final long serialVersionUID = 1L;

	private String numberOfCalculations;

	private String numberOfSpeciesInOntoSpecies;

	private String numberOfReactionMechanisms;

	private String numberOfSpeciesInOntoKin;

	private String numberOfChemicalReactions;

	public String getNumberOfChemicalReactions() {
		return numberOfChemicalReactions;
	}

	public void setNumberOfChemicalReactions(String numberOfChemicalReactions) {
		this.numberOfChemicalReactions = numberOfChemicalReactions;
	}

	public String getNumberOfSpeciesInOntoKin() {
		return numberOfSpeciesInOntoKin;
	}

	public void setNumberOfSpeciesInOntoKin(String numberOfSpeciesInOntoKin) {
		this.numberOfSpeciesInOntoKin = numberOfSpeciesInOntoKin;
	}

	public String getNumberOfReactionMechanisms() {
		return numberOfReactionMechanisms;
	}

	public void setNumberOfReactionMechanisms(String numberOfReactionMechanisms) {
		this.numberOfReactionMechanisms = numberOfReactionMechanisms;
	}

	public String getNumberOfSpeciesInOntoSpecies() {
		return numberOfSpeciesInOntoSpecies;
	}

	public void setNumberOfSpeciesInOntoSpecies(String numberOfSpeciesInOntoSpecies) {
		this.numberOfSpeciesInOntoSpecies = numberOfSpeciesInOntoSpecies;
	}

	public String getNumberOfCalculations() {
		return numberOfCalculations;
	}

	public void setNumberOfCalculations(String numberOfCalculations) {
		this.numberOfCalculations = numberOfCalculations;
	}

	@Override
	public String execute() {

		numberOfCalculations = new String(
				QueryManager.getQuery("http://localhost/rdf4j-server/repositories/ontocompchem",
						QueryString.getNumberOfGaussianCalculations()));

		numberOfSpeciesInOntoSpecies = new String(
				QueryManager.getQuery("http://localhost/rdf4j-server/repositories/ontospecieskb",
						QueryString.getNumberOfSpeciesInOntoSpecies()));

		numberOfReactionMechanisms = new String(QueryManager.getQuery(
				"http://localhost/rdf4j-server/repositories/ontokin", QueryString.getNumberOfReactionMechanisms()));

		numberOfSpeciesInOntoKin = new String(QueryManager.getQuery(
				"http://localhost/rdf4j-server/repositories/ontokin", QueryString.getNumberOfSpeciesInOntoKin()));

		numberOfChemicalReactions = new String(
				QueryManager.getQuery("http://localhost/rdf4j-server/repositories/ontokin",
						QueryString.getNumberOfChemicalReactionsInOntoKin()));

		return SUCCESS;
	}

}