package com.cmclinnovations.ontochem.model.converter.owl.query.sparql;

import java.util.ArrayList;
import java.util.Collections;

import org.slf4j.Logger;

import com.cmclinnovations.ontochem.model.converter.owl.OwlConverter;
import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.ElementArray;
import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.Kinetics;
import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.Phase;
import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.ReactionArray;
import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.SpeciesArray;
import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.State;
import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.Thermo;
import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.Transport;
import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.thermo.SiteDensity;

/**
 * Implements the methods that are used to perform SPARQL queries on top of
 * the OWL representation of a chemical mechanism ontology to extract
 * data and metadata about a phase (e.g. Gas Phase) to codify them in CTML.  
 * 
 * @author msff2
 *
 */
public class PhaseQuery extends OwlConverter implements IPhaseQuery{
	
	static Logger logger = org.slf4j.LoggerFactory.getLogger(PhaseQuery.class);
	
	public void query(){
		// The following queries store result in a global variable
		// called queryResult
		queryInstance(appConfigOntokin.getClassGasPhase());
		queryInstance(appConfigOntokin.getClassSitePhase());
		queryInstance(appConfigOntokin.getClassBulkPhase());
		if (queryResult != null && !queryResult.isEmpty()) {
			// The value of the variable queryResult has been sorted to 
			// maintain the order of the phases as it was in the source file.
			Collections.sort(queryResult);
			phaseOwlIds = queryResult;
			queryResult = new ArrayList<String>();
			for (String instance : phaseOwlIds) {
				queryAllProperties(instance);
				String sourceComment = performQuery(formQueryWithBaseURL
						(instance, appConfigOntokin.getSourceComment()), 1);
				if (sourceComment != null) {
					objectVsSourceComment.put("phase".concat(Integer.toString(++phaseSequence)), sourceComment);
				}
			}
			// Sets the properties of all the phases of a mechanism
			ctmlMD.setPhase(queriedPhaseList);
		}
	}
	
	private void queryAllProperties(String instance){
		// Creates a phase instance for all the properties of a phase.
		Phase phase = new Phase();
		queriedPhaseList.add(phase);
		// Passes the already created phase instance to all the methods
		// in order to assign the values of the corresponding properties. 
		queryDimension(instance, phase);
		queryId(instance, phase);
		queryIfMaterialPhase(instance, phase);
		queryComment(instance, phase);
		queryElementArray(instance, phase);
		querySpeciesArray(instance, phase);
		queryReactionArray(instance, phase);
		queryState(instance, phase);
		queryThermo(instance, phase);
		queryKinetics(instance, phase);
		queryTransport(instance, phase);
		queryPhaseArray(instance, phase);
	}
	
	/**
	 * If the current phase is a material phase, it assigns the material 
	 * name to the phase.
	 * 
	 * @param instance
	 * @param phase
	 */
	private void queryIfMaterialPhase(String instance, Phase phase) {
		performMultilineAnswerQuery(formTypeQueryWithAnInstance(instance), 2);
		ArrayList<String> phaseClasses = queryResult;
		queryResult = new ArrayList<String>();
		if (phaseClasses.contains(appConfigOntokin.getClassSitePhase())
				|| phaseClasses.contains(appConfigOntokin.getClassBulkPhase())) {
			queryMaterial(instance, phase);
		}
	}
	
	/**
	 * Queries and then sets the dimension attached to the instance of
	 * a phase in the mechanism OWL file being processed.
	 */
	private void queryDimension(String instance, Phase phase){
		phase.setDimension(readDimension(instance));
	}
	
	/**
	 * Queries and then sets the id attached to the instance of
	 * a phase in the mechanism OWL file being processed.
	 */
	private void queryId(String instance, Phase phase){
		phase.setId(readId(instance));
	}
	
	/**
	 * Queries and then sets the material attached to the instance of
	 * a site or bulk phase in the mechanism OWL file being processed.
	 */
	private void queryMaterial(String instance, Phase phase){
		String q = formQueryWithBaseURL(instance, appConfigOntokin.getObjectPropertyExistsIn());
		instance = performQuery(q, 1);
		q = formQueryWithAStandardVocabulary(RDFS, RDFS_URL, instance, RDFS_LABEL);
		phase.setMaterial(performQuery(q, 1));
	}
	
	/**
	 * Queries the comment attached to the instance of a phase in the
	 * mechanism OWL file being processed.
	 */
	private void queryComment(String instance, Phase phase){
		String comment = readComment(instance);
		if(comment==null){
		}else{
			if(comment.contains("\\\\")){
				phase.setComment(comment.replace("\\\\", "\\"));
			} else if(comment.contains("\\\"")){
				phase.setComment(comment.replace("\\\"", "\""));
			} else{
				phase.setComment(comment);
			}
		}
	}
	
	/**
	 * Queries and then sets the elementArray data source attached to the
	 * instance of a phase in the mechanism OWL file being processed.
	 */
	private void queryElementArray(String instance, Phase phase){
		ElementArray ea = new ElementArray();
		phase.setElementArray(ea);
		String q = formQueryWithBaseURL(instance, appConfigOntokin.getElementDataSource());		
		ea.setDatasrc(performQuery(q, 1));
		queryElement(instance, phase, ea);
	}
	
	/**
	 * Queries and then sets the elements attached to the instance of
	 * a phase in the mechanism OWL file being processed.
	 */
	private void queryElement(String instance, Phase phase, ElementArray ea){
		try{
		String q = formQueryWithBaseURL(instance, appConfigOntokin.getObjectPropertyHasElement());
		performMultilineAnswerQuery(q, 2);
		}catch(Exception e){
			e.printStackTrace();
		}
		Collections.sort(queryResult);
		// Calls the setElementArray method to extract element names of 
		// a phase from OWL and assign them the element array of a phase
		setElementArray(ea);
		queryResult = new ArrayList<String>();
	}
	
	/**
	 * Queries and then sets the speciesArray data source attached to the
	 * instance of a phase in the mechanism OWL file being processed.
	 */
	private void querySpeciesArray(String instance, Phase phase){
		SpeciesArray sa = new SpeciesArray();
		String q = formQueryWithBaseURL(instance, appConfigOntokin.getSpeciesDataSource());		
		sa.setDatasrc(performQuery(q, 1));
		phase.setSpeciesArray(sa);
		querySpecies(instance, phase, sa);
	}

	/**
	 * Queries and then sets species attached to the instance of
	 * a phase in the mechanism OWL file being processed.
	 */
	private void querySpecies(String instance, Phase phase, SpeciesArray sa){
		String q = formQueryWithBaseURL(instance, appConfigOntokin.getPhaseSpeciesArray());
		sa.setValue(performQuery(q, 1));
	}
	
	/**
	 * Queries and then sets the ReactionArray data source attached to the
	 * instance of a phase in the mechanism OWL file being processed.
	 */
	private void queryReactionArray(String instance, Phase phase){
		ReactionArray ra = new ReactionArray();
		String q = formQueryWithBaseURL(instance, appConfigOntokin.getReactionDataSource());		
		ra.setDatasrc(performQuery(q, 1));
		phase.setReactionArray(ra);
	}

	/**
	 * Queries the state attached to the instance of a phase in the mechanism 
	 * OWL file being processed and then sets it to CTML
	 */
	private void queryState(String instance, Phase phase){
		State state = new State();
		String q = formQueryWithBaseURL(instance, appConfigOntokin.getPhaseState());		
		String result = performQuery(q, 1);
		// PerformQuery returns null when the state element is not present 
		// at all in an OWL representation. If it is present and the value
		// is empty it returns empty value.
		if(result == null){
		} else if(result.trim().equals(EMPTY)){
			phase.setState(state);
		}
	}
	
	/**
	 * Queries and then sets the thermo data attached to the
	 * instance of a phase in the mechanism OWL file being processed.
	 */
	private void queryThermo(String instance, Phase phase){
		Thermo thermo = new Thermo();
		String q = formQueryWithBaseURL(instance, appConfigOntokin.getThermoModel());		
		thermo.setModel(performQuery(q, 1));
		phase.setThermo(thermo);
		querySiteDensity(instance, thermo);
	}
	
	/**
	 * Queries and then sets the kinetics data source attached to the
	 * instance of a phase in the mechanism OWL file being processed.
	 */
	private void queryKinetics(String instance, Phase phase){
		Kinetics kinetics = new Kinetics();
		String q = formQueryWithBaseURL(instance, appConfigOntokin.getKineticsModel());		
		kinetics.setModel(performQuery(q, 1));
		phase.setKinetics(kinetics);
	}
	
	/**
	 * Queries and then sets the transport data source attached to the
	 * instance of a phase in the mechanism OWL file being processed.
	 */
	private void queryTransport(String instance, Phase phase){
		Transport transport = new Transport();
		String q = formQueryWithBaseURL(instance, appConfigOntokin.getTransportModel());		
		transport.setModel(performQuery(q, 1));
		phase.setTransport(transport);
	}
	
	/**
	 * Queries and then sets the phaseArray attached to the
	 * instance of a phase in the mechanism OWL file being processed.
	 */
	private void queryPhaseArray(String instance, Phase phase){
		String q = formQueryWithBaseURL(instance, appConfigOntokin.getPhaseArray());		
		phase.setPhaseArray(performQuery(q, 1));
	}
	
	/**
	 * Queries and then sets the site density value and units attached to the
	 * instance of a phase in the mechanism OWL file being processed.
	 */
	private void querySiteDensity(String instance, Thermo thermo){
		SiteDensity site_density = new SiteDensity();
		String q = formQueryWithBaseURL(instance, appConfigOntokin.getSiteDensity());		
		String result = performQuery(q, 1);
		if(result!=null && !result.trim().equals(EMPTY)){
			site_density.setValue(result);
			q = formQueryWithBaseURL(instance, appConfigOntokin.getSiteDensityUnits());
			site_density.setUnits(performQuery(q, 1));
			thermo.setSite_density(site_density);
		}
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the dimension of a phase. 
	 * 
	 * @return the dimension of a phase
	 */
	private String readDimension(String phaseInstance) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithAStandardVocabulary(GEOSPARQL, 
				GEOSPARQL_URL, phaseInstance,
				GEOSPARQL_DIMENSION);
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the id of a phase. 
	 * 
	 * @return the the id of a phase
	 */
	private String readId(String phaseInstance) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithAStandardVocabulary(DUBLIN_CORE, 
				DUBLIN_CORE_URL, phaseInstance,
				DUBLIN_CORE_ID);
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Deals with processing OWL chemical element names and convert them to CTML
	 * compatible names and finally assigns the elements names to element array. 
	 * 
	 * @param ea
	 */
	private void setElementArray(ElementArray ea){
		// Declared the elementsExtraced variable to save an array of 
		// elements extracted from a phase.
		String elementsExtraced = EMPTY;
		for(String element:queryResult){
			if(element.contains(UNDERSCORE)){
				String[] tokens = element.split(UNDERSCORE);
				if(tokens.length>1){
					element = tokens[1];
				}
			} else{
				logger.error("The following chemical element name in OWL does not contain a underscore(_):"+element);
			}
			elementsExtraced = elementsExtraced.concat(element).concat(SPACE);
		}
		ea.setValue(elementsExtraced.trim());
	}
}
