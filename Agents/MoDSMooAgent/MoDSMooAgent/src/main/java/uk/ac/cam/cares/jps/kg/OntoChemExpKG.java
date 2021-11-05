package uk.ac.cam.cares.jps.kg;

import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;

import uk.ac.cam.cares.jps.agent.configuration.MoDSMooAgentProperty;
import uk.ac.cam.cares.jps.agent.mechanism.moo.MoDSMooAgentException;
import uk.ac.cam.cares.jps.agent.mechanism.moo.Property;

public class OntoChemExpKG {
	Logger logger = Logger.getLogger(OntoChemExpKG.class);
	private MoDSMooAgentProperty MoDSMooAgentProperty;
	
	public OntoChemExpKG(MoDSMooAgentProperty MoDSMooAgentProperty) {
		this.MoDSMooAgentProperty = MoDSMooAgentProperty;
	}
	
	/**
	 * DataTable class that records the queried experimental data. 
	 * 
	 * @author jb2197
	 *
	 */
	public final class DataTable {
	    private final List<String> header;
	    private final List<List<String>> data;

	    public DataTable(List<String> header, List<List<String>> data) {
	        this.header = header;
	        this.data = data;
	    }

	    public List<String> getTableHeader() {
	        return header;
	    }

	    public List<List<String>> getTableData() {
	        return data;
	    }
	}
	
	/**
	 * Form a data table given an IRI of experiment that measures laminar flame speed. 
	 * 
	 * @param experimentIRI
	 * @return
	 * @throws MoDSMooAgentException
	 */
	public DataTable formatFlameSpeedExpDataTable(String experimentIRI) throws MoDSMooAgentException {
		// TODO
		List<String> columnTitles = new ArrayList<String>();
		List<List<String>> experimentData = new ArrayList<List<String>>();
		
		List<List<String>> flameSpeedData = queryFlameSpeedExpData(experimentIRI);
		columnTitles.addAll(flameSpeedData.get(0));
		
		for (int i = 1; i < flameSpeedData.size(); i++) {
			List<String> dataLine = new ArrayList<>();
			dataLine.addAll(flameSpeedData.get(i));
			experimentData.add(dataLine);
		}
		
		return new DataTable(columnTitles, experimentData);
	}
	
	/**
	 * Form a data table given an IRI of experiment that measures ignition delay times. 
	 * 
	 * @param experimentIRI
	 * @return
	 * @throws MoDSMooAgentException
	 */
	public DataTable formatExperimentDataTable(String experimentIRI) throws MoDSMooAgentException {
		List<String> columnTitles = new ArrayList<String>();
		List<List<String>> experimentData = new ArrayList<List<String>>();
		
		List<List<String>> initialConc = queryConcentration(experimentIRI);
		List<List<String>> phi = queryEquivalenceRatio(experimentIRI);
		List<List<String>> expCond = queryExperimentData(experimentIRI);
		
		if (initialConc.get(0).get(0).equalsIgnoreCase("molecule")) {
			for (int i = 1; i < initialConc.size(); i++) {
				columnTitles.add(initialConc.get(i).get(0));
			}
		} else {
			logger.error("The query results should be molecule concentrations.");
		}
		columnTitles.add(phi.get(0).get(0));
		columnTitles.addAll(expCond.get(0));
		
		for (int i = 1; i < expCond.size(); i++) {
			List<String> dataLine = new ArrayList<>();
			// add molecular concentrations to dataLine
			dataLine.add(initialConc.get(1).get(1));
			dataLine.add(initialConc.get(2).get(1));
			dataLine.add(initialConc.get(3).get(1));
			// add equivalence ratio to dataLine
			dataLine.add(phi.get(1).get(0));
			// add temperature, pressure, and ignition delay times to dataLine
			dataLine.addAll(expCond.get(i));
			experimentData.add(dataLine);
		}
		
		return new DataTable(columnTitles, experimentData);
	}
	
	/**
	 * Query the concentration of a chemical mixture. 
	 * 
	 * @param experimentIRI
	 * @return
	 * @throws MoDSMooAgentException
	 */
	public List<List<String>> queryConcentration(String experimentIRI) throws MoDSMooAgentException {
		if(!experimentIRI.trim().startsWith("<") && !experimentIRI.trim().endsWith(">")){
			experimentIRI = "<".concat(experimentIRI).concat(">");
		}
		String queryString = formConcentrationQuery(experimentIRI);
		List<List<String>> testResults = RepositoryManager.queryRepository(MoDSMooAgentProperty.getRdf4jServerURL(), 
				MoDSMooAgentProperty.getRdf4jRepositoryOntoChemExp(), queryString);
		return testResults;
	}
	
	/**
	 * Query the equivalence ratio of a fuel-oxidiser mixture. 
	 * 
	 * @param experimentIRI
	 * @return
	 * @throws MoDSMooAgentException
	 */
	public List<List<String>> queryEquivalenceRatio(String experimentIRI) throws MoDSMooAgentException {
		if(!experimentIRI.trim().startsWith("<") && !experimentIRI.trim().endsWith(">")){
			experimentIRI = "<".concat(experimentIRI).concat(">");
		}
		String queryString = formEquivalenceRatioQuery(experimentIRI);
		List<List<String>> testResults = RepositoryManager.queryRepository(MoDSMooAgentProperty.getRdf4jServerURL(), 
				MoDSMooAgentProperty.getRdf4jRepositoryOntoChemExp(), queryString);
		return testResults;
	}
	
	/**
	 * Query the ignition delay times experimental data. 
	 * 
	 * @param experimentIRI
	 * @return
	 * @throws MoDSMooAgentException
	 */
	public List<List<String>> queryExperimentData(String experimentIRI) throws MoDSMooAgentException {
		if(!experimentIRI.trim().startsWith("<") && !experimentIRI.trim().endsWith(">")){
			experimentIRI = "<".concat(experimentIRI).concat(">");
		}
		String queryString = formExperimentDataQuery(experimentIRI);
		List<List<String>> testResults = RepositoryManager.queryRepository(MoDSMooAgentProperty.getRdf4jServerURL(), 
				MoDSMooAgentProperty.getRdf4jRepositoryOntoChemExp(), queryString);
		return testResults;
	}
	
	/**
	 * Query the laminar flame speed experimental data. 
	 * @param experimentIRI
	 * @return
	 * @throws MoDSMooAgentException
	 */
	public List<List<String>> queryFlameSpeedExpData(String experimentIRI) throws MoDSMooAgentException {
		if(!experimentIRI.trim().startsWith("<") && !experimentIRI.trim().endsWith(">")){
			experimentIRI = "<".concat(experimentIRI).concat(">");
		}
		String queryString = formFlameSpeedExpDataQuery(experimentIRI);
		List<List<String>> testResults = RepositoryManager.queryRepository(MoDSMooAgentProperty.getRdf4jServerURL(), 
				MoDSMooAgentProperty.getRdf4jRepositoryOntoChemExp(), queryString);
		return testResults;
	}
	
	/**
	 * Form the query string of chemical mixture concentration. 
	 * 
	 * @param experimentIRI
	 * @return
	 */
	private String formConcentrationQuery(String experimentIRI) {
		String queryString = Property.PREFIX_BINDING_ONTOCHEMEXP.getPropertyName();
		queryString = queryString.concat("SELECT ?molecule ?composition \n");
		queryString = queryString.concat("WHERE { \n");
		queryString = queryString.concat("    ").concat(experimentIRI).concat(" OntoChemExp:hasCommonProperties ?commonProperties . \n");
		queryString = queryString.concat("    ?commonProperties OntoChemExp:hasProperty ?property . \n");
		queryString = queryString.concat("    ?property OntoChemExp:hasComponent ?component . \n");
		queryString = queryString.concat("    ?component OntoChemExp:hasSpeciesLink ?speciesLink . \n");
		queryString = queryString.concat("    ?speciesLink OntoChemExp:hasDatPreferredKey ?molecule . \n");
		queryString = queryString.concat("    ?component OntoChemExp:hasAmount ?amount . \n");
		queryString = queryString.concat("    ?amount OntoChemExp:hasVal ?composition \n");
		queryString = queryString.concat("}");
		return queryString;
	}
	
	/**
	 * Form the query string of equivalence ratio of fuel-oxidiser mixture. 
	 * 
	 * @param experimentIRI
	 * @return
	 */
	private String formEquivalenceRatioQuery(String experimentIRI) {
		String queryString = Property.PREFIX_BINDING_ONTOCHEMEXP.getPropertyName();
		queryString = queryString.concat("SELECT ?Phi \n");
		queryString = queryString.concat("WHERE { \n");
		queryString = queryString.concat("    ").concat(experimentIRI).concat(" OntoChemExp:hasCommonProperties ?commonProperties . \n");
		queryString = queryString.concat("    ?commonProperties OntoChemExp:hasProperty ?property . \n");
		queryString = queryString.concat("    ?property OntoChemExp:hasName ?name . \n");
		queryString = queryString.concat("    ?property OntoChemExp:hasValue ?value . \n");
		queryString = queryString.concat("    ?value OntoChemExp:hasVal ?Phi  \n");
		queryString = queryString.concat("    FILTER regex(str(?name), \"equivalence ratio\", \"i\") \n");
		queryString = queryString.concat("}");
		return queryString;
	}
	
	/**
	 * Form the query string of ignition delay times experimental data. 
	 * 
	 * @param experimentIRI
	 * @return
	 */
	private String formExperimentDataQuery(String experimentIRI) {
		String queryString = Property.PREFIX_BINDING_ONTOCHEMEXP.getPropertyName();
		queryString = queryString.concat(Property.PREFIX_BINDING_RDF.getPropertyName());
		queryString = queryString.concat("SELECT ?Temperature ?UnitTemp ?Pressure ?UnitPres ?IgnitionDelay ?UnitIgni \n");
		queryString = queryString.concat("WHERE { \n");
		queryString = queryString.concat("    ").concat(experimentIRI).concat(" OntoChemExp:hasDataGroup ?dataGroup . \n");
		queryString = queryString.concat("    ?dataGroup OntoChemExp:hasDataPoint ?dataPoint . \n");
		queryString = queryString.concat("    ?dataPoint OntoChemExp:hasID ?dataPointID . \n");
		queryString = queryString.concat("    ?dataGroup OntoChemExp:hasProperty ?propertyTemp . \n");
		queryString = queryString.concat("    ?propertyTemp OntoChemExp:hasName ?nameTemp . \n");
		queryString = queryString.concat("    ?propertyTemp OntoChemExp:hasID ?idTemp . \n");
		queryString = queryString.concat("    ?propertyTemp OntoChemExp:hasUnits ?UnitTemp . \n");
		queryString = queryString.concat("    FILTER regex(str(?nameTemp), \"temperature\", \"i\") \n");
		queryString = queryString.concat("    ?dataPoint OntoChemExp:hasDataPointX ?dataPointTemp . \n");
		queryString = queryString.concat("    ?dataPointTemp rdf:type ?typeTemp . \n");
		queryString = queryString.concat("    FILTER regex(str(?typeTemp), str(?idTemp), \"i\") \n");
		queryString = queryString.concat("    ?dataPointTemp OntoChemExp:hasVal ?Temperature . \n");
		queryString = queryString.concat("    ?dataGroup OntoChemExp:hasProperty ?propertyPres . \n");
		queryString = queryString.concat("    ?propertyPres OntoChemExp:hasName ?namePres . \n");
		queryString = queryString.concat("    ?propertyPres OntoChemExp:hasID ?idPres . \n");
		queryString = queryString.concat("    ?propertyPres OntoChemExp:hasUnits ?UnitPres . \n");
		queryString = queryString.concat("    FILTER regex(str(?namePres), \"pressure\", \"i\") \n");
		queryString = queryString.concat("    ?dataPoint OntoChemExp:hasDataPointX ?dataPointPres . \n");
		queryString = queryString.concat("    ?dataPointPres rdf:type ?typePres . \n");
		queryString = queryString.concat("    FILTER regex(str(?typePres), str(?idPres), \"i\") \n");
		queryString = queryString.concat("    ?dataPointPres OntoChemExp:hasVal ?Pressure . \n");
		queryString = queryString.concat("    ?dataGroup OntoChemExp:hasProperty ?propertyIgni . \n");
		queryString = queryString.concat("    ?propertyIgni OntoChemExp:hasName ?nameIgni . \n");
		queryString = queryString.concat("    ?propertyIgni OntoChemExp:hasID ?idIgni . \n");
		queryString = queryString.concat("    ?propertyIgni OntoChemExp:hasUnits ?UnitIgni . \n");
		queryString = queryString.concat("    FILTER regex(str(?nameIgni), \"ignition delay\", \"i\") \n");
		queryString = queryString.concat("    ?dataPoint OntoChemExp:hasDataPointX ?dataPointIgni . \n");
		queryString = queryString.concat("    ?dataPointIgni rdf:type ?typeIgni . \n");
		queryString = queryString.concat("    FILTER regex(str(?typeIgni), str(?idIgni), \"i\") \n");
		queryString = queryString.concat("    ?dataPointIgni OntoChemExp:hasVal ?IgnitionDelay . \n");
		queryString = queryString.concat("}");
		return queryString;
	}
	
	/**
	 * Form the query string of laminar flame speed experimental data. 
	 * 
	 * @param experimentIRI
	 * @return
	 */
	private String formFlameSpeedExpDataQuery(String experimentIRI) {
		String queryString = Property.PREFIX_BINDING_ONTOCHEMEXP.getPropertyName();
		queryString = queryString.concat(Property.PREFIX_BINDING_RDF.getPropertyName());
		queryString = queryString.concat("SELECT ?Fuel ?Oxidizer ?Phi ?Temperature ?UnitTemp ?Pressure ?UnitPres ?LaminarFlameSpeed ?UnitLFS ?LFSErrors \n");
		queryString = queryString.concat("WHERE { \n");
		queryString = queryString.concat("    ").concat(experimentIRI).concat(" OntoChemExp:hasCommonProperties ?commonProperties . \n");
		queryString = queryString.concat("    ?commonProperties OntoChemExp:hasProperty ?propertyFuel . \n");
		queryString = queryString.concat("    ?propertyFuel OntoChemExp:hasName \"fuel\" . \n");
		queryString = queryString.concat("    ?propertyFuel OntoChemExp:hasValue ?valueFuel . \n");
		queryString = queryString.concat("    ?valueFuel OntoChemExp:hasVal ?Fuel . \n");
		
		queryString = queryString.concat("    ?commonProperties OntoChemExp:hasProperty ?propertyOxi . \n");
		queryString = queryString.concat("    ?propertyOxi OntoChemExp:hasName \"oxidizer\" . \n");
		queryString = queryString.concat("    ?propertyOxi OntoChemExp:hasValue ?valueOxi . \n");
		queryString = queryString.concat("    ?valueOxi OntoChemExp:hasVal ?Oxidizer . \n");
		
		queryString = queryString.concat("    ?commonProperties OntoChemExp:hasProperty ?propertyTemp . \n");
		queryString = queryString.concat("    ?propertyTemp OntoChemExp:hasName \"temperature\" . \n");
		queryString = queryString.concat("    ?propertyTemp OntoChemExp:hasValue ?valueTemp . \n");
		queryString = queryString.concat("    ?valueTemp OntoChemExp:hasVal ?Temperature . \n");
		queryString = queryString.concat("    ?propertyTemp OntoChemExp:hasUnits ?UnitTemp . \n");
		
		queryString = queryString.concat("    ?commonProperties OntoChemExp:hasProperty ?propertyPres . \n");
		queryString = queryString.concat("    ?propertyPres OntoChemExp:hasName \"pressure\" . \n");
		queryString = queryString.concat("    ?propertyPres OntoChemExp:hasValue ?valuePres .  \n");
		queryString = queryString.concat("    ?valuePres OntoChemExp:hasVal ?Pressure . \n");
		queryString = queryString.concat("    ?propertyPres OntoChemExp:hasUnits ?UnitPres . \n");
		
		queryString = queryString.concat("    ").concat(experimentIRI).concat(" OntoChemExp:hasDataGroup ?dataGroup . \n");
		queryString = queryString.concat("    ?dataGroup OntoChemExp:hasID ?dgID . \n");
		queryString = queryString.concat("    ?dataGroup OntoChemExp:hasDataPoint ?dataPoint . \n");
		queryString = queryString.concat("    ?dataPoint OntoChemExp:hasID ?dpID . \n");
		
		queryString = queryString.concat("    ?dataGroup OntoChemExp:hasProperty ?propertyPhi . \n");
		queryString = queryString.concat("    ?propertyPhi OntoChemExp:hasName ?namePhi . \n");
		queryString = queryString.concat("    ?propertyPhi OntoChemExp:hasID ?idPhi . \n");
		queryString = queryString.concat("    FILTER regex(str(?namePhi), \"equivalence ratio\", \"i\") \n");
		queryString = queryString.concat("    FILTER NOT EXISTS { \n");
		queryString = queryString.concat("       FILTER (regex(?namePhi, \"error\", \"i\")) . \n");
		queryString = queryString.concat("    }\n");
		queryString = queryString.concat("    ?dataPoint OntoChemExp:hasDataPointX ?dpPhi . \n");
		queryString = queryString.concat("    ?dpPhi rdf:type ?typePhi . \n");
		queryString = queryString.concat("    ?dpPhi OntoChemExp:hasVal ?Phi . \n");
		queryString = queryString.concat("    FILTER regex(str(?typePhi), str(?idPhi), \"i\") \n");
		
		queryString = queryString.concat("    ?dataGroup OntoChemExp:hasProperty ?propertyLFS . \n");
		queryString = queryString.concat("    ?propertyLFS OntoChemExp:hasName ?nameLFS . \n");
		queryString = queryString.concat("    ?propertyLFS OntoChemExp:hasID ?idLFS . \n");
		queryString = queryString.concat("    ?propertyLFS OntoChemExp:hasUnits ?UnitLFS . \n");
		queryString = queryString.concat("    FILTER regex(str(?nameLFS), \"flame speed\", \"i\") \n");
		queryString = queryString.concat("    FILTER NOT EXISTS { \n");
		queryString = queryString.concat("        FILTER (regex(?nameLFS, \"error\", \"i\")) . \n");
		queryString = queryString.concat("    }\n");
		queryString = queryString.concat("    ?dataPoint OntoChemExp:hasDataPointX ?dpLFS . \n");
		queryString = queryString.concat("    ?dpLFS rdf:type ?typeLFS . \n");
		queryString = queryString.concat("    ?dpLFS OntoChemExp:hasVal ?LaminarFlameSpeed . \n");
		queryString = queryString.concat("    FILTER regex(str(?typeLFS), str(?idLFS), \"i\") \n");
		
		queryString = queryString.concat("    ?dataGroup OntoChemExp:hasProperty ?propertyLFSError . \n");
		queryString = queryString.concat("    ?propertyLFSError OntoChemExp:hasName ?nameLFSError . \n");
		queryString = queryString.concat("    ?propertyLFSError OntoChemExp:hasID ?idLFSError . \n");
		queryString = queryString.concat("    ?propertyLFSError OntoChemExp:hasUnits ?UnitLFSError . \n");
		queryString = queryString.concat("    FILTER regex(str(?nameLFSError), \"flame speed error\", \"i\") \n");
		queryString = queryString.concat("    ?dataPoint OntoChemExp:hasDataPointX ?dpLFSError . \n");
		queryString = queryString.concat("    ?dpLFSError rdf:type ?typeLFSError . \n");
		queryString = queryString.concat("    ?dpLFSError OntoChemExp:hasVal ?LFSErrors . \n");
		queryString = queryString.concat("    FILTER regex(str(?typeLFSError), str(?idLFSError), \"i\") \n");
		queryString = queryString.concat("}");
		return queryString;
	}
}
