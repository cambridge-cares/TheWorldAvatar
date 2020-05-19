package uk.ac.ceb.como.action;

import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.apache.log4j.Logger;

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
	
	final static Logger logger = Logger.getLogger(StatisticsAction.class.getName());
	
	static Properties kbProperties = PropertiesManager.loadProperties(StatisticsAction.class.getClassLoader().getResourceAsStream("kb.properties"));	
	
	private static String ontocompchemkb = kbProperties.getProperty("ontocompchem.kb.local.rdf4j.server.url");
	private static String ontokinkb = kbProperties.getProperty("ontokin.kb.local.rdf4j.server.url");
	private static String ontospecieskb = kbProperties.getProperty("ontospecies.kb.local.rdf4j.server.url");
	
	private String numberOfCalculations;

	private String numberOfSpeciesInOntoSpecies ;

	private String numberOfReactionMechanisms ;

	private String numberOfSpeciesInOntoKin ;

	private String numberOfChemicalReactions;
	
//	private String numberOfAgents;
	
	private String numberOfSynonyms;
	
	private String numberOfCabronAndHydrogenSpecies;
	
	private String numberOfCabronAndHydrogenAndOxygenSpecies;
	
	private String numberOfNitrogenSpeciesInOntoKin;
	
	private String numberOfReactionsThatInvolveOxygenHydrocarbonSpecies;
	
	private String numberOfReactionsThatInvolveNitrogenSpecies;
	
	private String numberOfReactionsHydrocarbonSpecies;
	
	private LinkedList<String> labelList = new LinkedList<String>();	
	
	private LinkedList<String> ontoCompChemDataSetList = new LinkedList<String>();
	
	private LinkedList<String> ontoKinDataSetList = new LinkedList<String>();
	
	public List<String> getOntoKinDataSetList() {
		return ontoKinDataSetList;
	}

	public void setOntoKinDataSetList(LinkedList<String> ontoKinDataSetList) {
		this.ontoKinDataSetList = ontoKinDataSetList;
	}

	public LinkedList<String> getOntoCompChemDataSetList() {
		return ontoCompChemDataSetList;
	}

	public void setOntoCompChemDataSetList(LinkedList<String> ontoCompChemDataSetList) {
		this.ontoCompChemDataSetList = ontoCompChemDataSetList;
	}

	public LinkedList<String> getLabelList() {
		return labelList;
	}

	public void setLabelList(LinkedList<String> labelList) {
		this.labelList = labelList;
	}

	public String getNumberOfReactionsHydrocarbonSpecies() {
		return numberOfReactionsHydrocarbonSpecies;
	}

	public void setNumberOfReactionsHydrocarbonSpecies(String numberOfReactionsHydrocarbonSpecies) {
		this.numberOfReactionsHydrocarbonSpecies = numberOfReactionsHydrocarbonSpecies;
	}

	public String getNumberOfReactionsThatInvolveNitrogenSpecies() {
		return numberOfReactionsThatInvolveNitrogenSpecies;
	}

	public void setNumberOfReactionsThatInvolveNitrogenSpecies(String numberOfReactionsThatInvolveNitrogenSpecies) {
		this.numberOfReactionsThatInvolveNitrogenSpecies = numberOfReactionsThatInvolveNitrogenSpecies;
	}

	public String getNumberOfReactionsThatInvolveOxygenHydrocarbonSpecies() {
		return numberOfReactionsThatInvolveOxygenHydrocarbonSpecies;
	}

	public void setNumberOfReactionsThatInvolveOxygenHydrocarbonSpecies(
			String numberOfReactionsThatInvolveOxygenHydrocarbonSpecies) {
		this.numberOfReactionsThatInvolveOxygenHydrocarbonSpecies = numberOfReactionsThatInvolveOxygenHydrocarbonSpecies;
	}
	
	public String getNumberOfNitrogenSpeciesInOntoKin() {
		return numberOfNitrogenSpeciesInOntoKin;
	}

	public void setNumberOfNitrogenSpeciesInOntoKin(String numberOfNitrogenSpeciesInOntoKin) {
		this.numberOfNitrogenSpeciesInOntoKin = numberOfNitrogenSpeciesInOntoKin;
	}

	public String getNumberOfCabronAndHydrogenAndOxygenSpecies() {
		return numberOfCabronAndHydrogenAndOxygenSpecies;
	}

	public void setNumberOfCabronAndHydrogenAndOxygenSpecies(String numberOfCabronAndHydrogenAndOxygenSpecies) {
		this.numberOfCabronAndHydrogenAndOxygenSpecies = numberOfCabronAndHydrogenAndOxygenSpecies;
	}

	public String getNumberOfCabronAndHydrogenSpecies() {
		return numberOfCabronAndHydrogenSpecies;
	}

	public void setNumberOfCabronAndHydrogenSpecies(String numberOfCabronAndHydrogenSpecies) {
		this.numberOfCabronAndHydrogenSpecies = numberOfCabronAndHydrogenSpecies;
	}

	public String getNumberOfSynonyms() {
		return numberOfSynonyms;
	}

	public void setNumberOfSynonyms(String numberOfSynonyms) {
		this.numberOfSynonyms = numberOfSynonyms;
	}

//	public String getNumberOfAgents() {
//		return numberOfAgents;
//	}
//
//	public void setNumberOfAgents(String numberOfAgents) {
//		this.numberOfAgents = numberOfAgents;
//	}

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
	public String execute() throws IOException {		

		PropertiesManager propertiesManager = new PropertiesManager();
		
		LinkedHashMap<String,String> ontoCompChemMap = new LinkedHashMap<String,String>();
		
		ontoCompChemMap.putAll(propertiesManager.getFrequencyOfSpeciesPerDate(ontocompchemkb,QueryString.getSpeciesIRIFromOntoCompChem()));
		
		LinkedHashMap<String,String> ontoKinMap = new LinkedHashMap<String,String>();
		
		ontoKinMap.putAll(propertiesManager.getFrequencyOfSpeciesPerDate(ontokinkb,QueryString.getSpeciesIRIFromOntoKin()));
		
		LinkedHashMap<String,String> updatedOntoCompChemMap = new LinkedHashMap<String,String>();
		
		updatedOntoCompChemMap.putAll(new PropertiesManager().updateFrequenciesMapData(ontoKinMap, ontoCompChemMap));
		
		LinkedHashMap<String,String> updatedOntoKinMap = new LinkedHashMap<String,String>();
		
		updatedOntoKinMap.putAll(new PropertiesManager().updateFrequenciesMapData(updatedOntoCompChemMap,ontoKinMap));
		
		for(Map.Entry<String, String> map: updatedOntoKinMap.entrySet()) {
			
			logger.info("key: " + map.getKey() + " value: " +map.getValue());
		}
		
		LinkedList<String> labelListTemp = new LinkedList<String>();
		LinkedList<String> ontoCompChemDataSetListTemp = new LinkedList<String>();
		
		/**
		 * @author NK510 (caresssd@hermes.cam.ac.uk)
		 * Updated onto compchem data
		 * 
		 */
		for(Map.Entry<String, String> compChemMap: updatedOntoCompChemMap.entrySet()) {
			
			labelListTemp.add(compChemMap.getKey());
			ontoCompChemDataSetListTemp.add(compChemMap.getValue());
		}
		
		LinkedList<String> ontoKinDataSetListTemp = new LinkedList<String>();

		/**
		 * 
		 * @author NK510 (caresssd@hermes.cam.ac.uk)
		 * Updated onto kin data.
		 * 
		 */
		for(Map.Entry<String, String> m: updatedOntoKinMap.entrySet()) {
			
			ontoKinDataSetListTemp.add(m.getValue());
		}
		
		labelList.addAll(labelListTemp);
		ontoCompChemDataSetList.addAll(ontoCompChemDataSetListTemp);
		ontoKinDataSetList.addAll(ontoKinDataSetListTemp);
		
		logger.info("OntoCompChem data set list:");		
		for(String s: ontoCompChemDataSetList) {
			
			logger.info(s);
		}
		

		logger.info("OntoKin data set list:");
		for(String s: ontoKinDataSetList) {
			
			logger.info(s);
		}
		
		
		/**
		 * 
		 * @author NK510 (caresssd@hermes.cam.ac.uk)
		 * These properties are shown on second table.
		 * 
		 */
		
		numberOfReactionsHydrocarbonSpecies = new QueryManager().getQuery(ontokinkb, QueryString.getNumberOfReactionsThatInvolveHydrocarbonSpecies());
		
		numberOfReactionsThatInvolveNitrogenSpecies=new QueryManager().getQuery(ontokinkb, QueryString.getNumberOfReactionsThatInvolveNitrogenSpecies());
			
		numberOfReactionsThatInvolveOxygenHydrocarbonSpecies =new QueryManager().getQuery(ontokinkb, QueryString.getNumberOfReactionsThatInvolveOxygenHydrocarbonSpecies());
		
		/**
		 * 
		 * @author NK510 (caresssd@hermes.cam.ac.uk)
		 * These properties are shown on first table.
		 * 
		 */
		
		numberOfCalculations = new QueryManager().getQuery(ontocompchemkb,QueryString.getNumberOfGaussianCalculations());
		
		numberOfSpeciesInOntoSpecies = new QueryManager().getQuery(ontospecieskb,QueryString.getNumberOfSpeciesInOntoSpecies());

		numberOfReactionMechanisms = new QueryManager().getQuery(ontokinkb, QueryString.getNumberOfReactionMechanisms());

		numberOfSpeciesInOntoKin = new QueryManager().getQuery(ontokinkb, QueryString.getNumberOfSpeciesInOntoKin());

		numberOfChemicalReactions = new QueryManager().getQuery(ontokinkb,QueryString.getNumberOfChemicalReactionsInOntoKin());
		
		numberOfSynonyms = new QueryManager().getQuery(ontospecieskb,QueryString.getNumberOfSynonymsInOntoSpecies());
		
		/**
		 * 
		 * @author NK510 (caresssd@hermes.cam.ac.uk)
		 * Line below is implemented by Dr Feroz Farazi (msff2@cam.ac.uk). He contributed in implementation of reading "value" in sparql query result as JSONObject.  
		 * 
		 */
//		numberOfAgents = JsonPath.read(QueryManager.getNumberOfAgents().toString(), "$.results.bindings[0].sum.value");
		
		/**
		 * 
		 * @author NK510 (caresssd@hermes.cam.ac.uk)
		 * These properties are shown on second table.
		 * 
		 */
		
		numberOfCabronAndHydrogenSpecies = new QueryManager().getQuery(ontokinkb, QueryString.getCabronHydrogenSpeciesInOntoKin());
		
		numberOfCabronAndHydrogenAndOxygenSpecies = new QueryManager().getQuery(ontokinkb, QueryString.getCabronHydrogenOxygenSpeciesInOntoKin());
		
		numberOfNitrogenSpeciesInOntoKin = new QueryManager().getQuery(ontokinkb, QueryString.getNumberNitrogenSpeciesInOntoKin());
		
		
		
		return SUCCESS;
	}

}