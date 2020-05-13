package uk.ac.ceb.como.action;

import java.util.Properties;

import com.opensymphony.xwork2.ActionSupport;

import uk.ac.ceb.como.properties.PropertiesManager;
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
	
	Properties kbProperties = PropertiesManager.loadProperties(StatisticsAction.class.getClassLoader().getResourceAsStream("kb.properties"));
	
	private String numberOfCalculations;

	private String numberOfSpeciesInOntoSpecies;

	private String numberOfReactionMechanisms;

	private String numberOfSpeciesInOntoKin;

	private String numberOfChemicalReactions;
	
	private String numberOfAgents;
	
	private String ontocompchemkb = kbProperties.getProperty("ontocompchem.kb.local.rdf4j.server.url");
	private String ontokinkb = kbProperties.getProperty("ontokin.kb.local.rdf4j.server.url");
	private String ontospecieskb = kbProperties.getProperty("ontospecies.kb.local.rdf4j.server.url");

	public String getNumberOfAgents() {
		return numberOfAgents;
	}

	public void setNumberOfAgents(String numberOfAgents) {
		this.numberOfAgents = numberOfAgents;
	}

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

		numberOfCalculations = new String(QueryManager.getQuery(ontocompchemkb,QueryString.getNumberOfGaussianCalculations()));

		numberOfSpeciesInOntoSpecies = new String(QueryManager.getQuery(ontospecieskb,QueryString.getNumberOfSpeciesInOntoSpecies()));

		numberOfReactionMechanisms = new String(QueryManager.getQuery(ontokinkb, QueryString.getNumberOfReactionMechanisms()));

		numberOfSpeciesInOntoKin = new String(QueryManager.getQuery(ontokinkb, QueryString.getNumberOfSpeciesInOntoKin()));

		numberOfChemicalReactions = new String(QueryManager.getQuery(ontokinkb,QueryString.getNumberOfChemicalReactionsInOntoKin()));

		return SUCCESS;
	}

}