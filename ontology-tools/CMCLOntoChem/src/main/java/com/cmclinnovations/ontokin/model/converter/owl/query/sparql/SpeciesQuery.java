package com.cmclinnovations.ontokin.model.converter.owl.query.sparql;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.slf4j.Logger;

import com.cmclinnovations.ontokin.model.converter.owl.OwlConverter;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.Density;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.DipoleMoment;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.FloatArray;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.LJDiameter;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.LJWellDepth;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.NASA;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.Polarizability;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.RotRelax;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.STRING;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.Size;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.Species;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.SpeciesComment;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.SpeciesData;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.SpeciesThermo;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.SpeciesTransport;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.TransportComment;

/**
 * Implements the methods that are used to perform SPARQL queries on top of
 * the OWL representation of a chemical mechanism ontology to extract
 * data and metadata about species to codify them in CTML.  
 * 
 * @author msff2
 *
 */
public class SpeciesQuery extends OwlConverter implements ISpeciesQuery{
	static Logger logger = org.slf4j.LoggerFactory.getLogger(PhaseQuery.class);
	/**
	 * Queries the two metadata (data block id and case sensitivity) about</br>
	 * species. These metadata are used to divide species into a number of</br>
	 * groups. The groups can be seen as, e.g., GAS_species_data and</br> 
	 * MATERIAL1_species_data.
	 */
	public void query(){
		queriedSpeciesDataList = new ArrayList<SpeciesData>();
		ctmlMD.setSpeciesData(queriedSpeciesDataList);
		// Queries species for each phase
		for (String phaseOwlId : phaseOwlIds) {
			// Queries and retrieves IRIs of species which belong to a 
			// specific phase contained in the phaseOwlId variable 
			querySpecies(phaseOwlId);
			if (queryResult != null && !queryResult.isEmpty()) {
				// To maintain the order of species codification in CTML it
				// sorts
				// the list of species ids
				Collections.sort(queryResult);
				ArrayList<String> speciesIds = queryResult;
				// Frees up the queryResult list for next use
				queryResult = new ArrayList<String>();
				// Queries species metadata and all the properties of a group of
				// species
				queryAllProperties(speciesIds, phaseOwlId);
			}
		}
	}
	
	/**
	 * Queries all the metadata and properties of all the species.
	 * 
	 * @param speciesIds
	 * @param phaswOwlId
	 */
	private void queryAllProperties(ArrayList<String> speciesIds, String phaswOwlId) {
		String metadataInstanceId = querySpeciesMetadata(speciesIds);
		String speciesMetadataId = readSpeciesMetaDataId(metadataInstanceId);
		if(!previousSpeciesMetadataId.equalsIgnoreCase(speciesMetadataId)){
			previousSpeciesMetadataId = speciesMetadataId;
			speciesDataInOwl = new SpeciesData();
			// Setting species metadata in the following method allows to reduce
			// the amount of code from this method
			setSpeciesMetadata(speciesDataInOwl, speciesMetadataId, metadataInstanceId);
			speciesInOwlList = new ArrayList<Species>();
			queriedSpeciesDataList.add(speciesDataInOwl);
			speciesDataInOwl.setSpecies(speciesInOwlList);
		}
		queryAllProperties(speciesIds);
	}
	
	private void queryAllProperties(ArrayList<String> speciesIds){
		for(String speciesInstanceId: speciesIds){
			queryAllProperties(speciesInstanceId);
			String sourceComment = performQuery(formQueryWithBaseURL(speciesInstanceId, appConfigOntokin.getSourceComment()), 1);
			if (sourceComment != null) {
				objectVsSourceComment.put("species".concat(Integer.toString(++speciesSequence)), sourceComment);
			}
		}
	}
	
	private void setSpeciesMetadata(SpeciesData speciesData, String speciesMetadataId, String metadataInstanceId){
		String caseSensitivity = readSpeciesDataCaseSensitivity(metadataInstanceId);
		speciesData.setId(speciesMetadataId);
		speciesData.setCaseSensitive(caseSensitivity);
		String sourceComment = performQuery(formQueryWithBaseURL(metadataInstanceId, appConfigOntokin.getSourceComment()), 1);
		if (sourceComment != null) {
			objectVsSourceComment.put("speciesData".concat(Integer.toString(++speciesDataSequence)), sourceComment);
		}
	}
	
	/**
	 * Queries and return the metadata instance instance id of a species.
	 * 
	 * @param speciesId
	 * @param speciesData
	 */
	private String querySpeciesMetadata(List<String> speciesId){
		if(speciesId==null || speciesId.size()<1){
			logger.error("No species found in this mechanism OWL ontology.");
		}
		// Forms query to retrieve the metadata instance id of element data
		String q = formQueryWithBaseURL(speciesId.get(0), appConfigOntokin.getSpeciesMetadataProperty());
		// Retrieved metadata instance id
		return performQuery(q, 2);
	}
	
	/**
	 * Queries a species using its phase instance id.
	 * 
	 * @param phaseOwlId the instance id of a phase in the OWL ontology 
	 * being processed
	 */
	private void querySpecies(String phaseOwlId){
		String q = formSubjectRetrievalQuery(phaseOwlId, appConfigOntokin.getOntokinSpeciesBelongsTo());
		// This method call will store result in a global variable called
		// queryResult, therefore, the line following the call will extract
		// results from it. 
		performMultilineAnswerQuery(q, 2);
	}
	
	/**
	 * Reads species metadata id (e.g. GAS_species_data and MATERIAL1_species_data)
	 * 
	 * @param metaDataInstance
	 * @return
	 */
	private String readSpeciesMetaDataId(String metaDataInstance){
		String q = formQueryWithAStandardVocabulary(DUBLIN_CORE, DUBLIN_CORE_URL, metaDataInstance, DUBLIN_CORE_ID);
		return performQuery(q, 1);
	}
	
	/**
	 * Reads information about if species data is case sensitive or not
	 * 
	 * @param metaDataInstance
	 * @return
	 */
	private String readSpeciesDataCaseSensitivity(String metaDataInstance){
		String q = formQueryWithBaseURL(metaDataInstance, appConfigOntokin.getSpeciesDataCaseSensitivity());
		return performQuery(q, 1);
	}
	
	/**
	 * Queries all the properties of species.
	 * 
	 * @param instance
	 */
	private void queryAllProperties(String instance){
		// Creates a species instance with all the properties found in OWL.
		Species species = new Species();
		speciesInOwlList.add(species);
		// Passes the already created species instance to all the methods
		// in order to assign the values of the corresponding properties. 
		queryName(instance, species);
		queryPhase(instance, species);
		queryComment(instance, species);
		queryNote(instance, species);
		queryAtomArray(instance, species);
		querySize(instance, species);
		queryDensity(instance, species);
		queryThermoData(instance, species);
		queryTransportData(instance, species);
	}
	
	/**
	 * Queries the name attached to the instance of a species in the
	 * mechanism OWL file being processed.
	 */
	private void queryName(String instance, Species species){
		String name = readLabel(instance);
		species.setName(name);
	}
	
	/**
	 * Queries the phase attached to the instance of a species in the
	 * mechanism OWL file being processed.
	 */
	private void queryPhase(String instance, Species species){
		species.setPhase(readPhase(instance));
	}
	
	/**
	 * Queries the comment attached to the instance of a species in the
	 * mechanism OWL file being processed.
	 */
	private void queryComment(String instance, Species species){
		String comment = readComment(instance);
		if(comment==null){
		} else if(comment.equals(EMPTY)){
			SpeciesComment speciesComment = new SpeciesComment();
			speciesComment.setValue(EMPTY);
			species.setComment(speciesComment);
		}
		else{
			SpeciesComment speciesComment = new SpeciesComment();
			species.setComment(speciesComment);
			if(comment.contains("\\\\")){
				comment = comment.replace("\\\\", "\\");
			} else if(comment.contains("\\\"")){
				comment = comment.replace("\\\"", "\"");
			}
			speciesComment.setValue(comment);
		}
	}
	
	/**
	 * Queries the thermo data of a species in the mechanism OWL file 
	 * being processed.
	 */
	private void queryThermoData(String instance, Species species){
		SpeciesThermo  thmo = new SpeciesThermo(); 
		species.setThermo(thmo);
		queryThermoComment(instance, thmo);
		queryThermoModels(instance, thmo);
	}
	
	/**
	 * Queries the transport data of a species in the mechanism OWL file 
	 * being processed.
	 */
	private void queryTransportData(String instance, Species species){
		queryTransportModel(instance, species);
	}
	
	/**
	 * Queries the comment about thermo model attached to the instance of 
	 * a species in the mechanism OWL file being processed.
	 */
	private void queryThermoComment(String instance, SpeciesThermo thmo){
		String comment = readThermoComment(instance);
		if(comment==null){
		}else if (comment.isEmpty()){
			thmo.setComment(comment);
		} else{
			if(comment.contains("\\\\")){
				thmo.setComment(comment.replace("\\\\", "\\"));
			} else if(comment.contains("\\\"")){
				thmo.setComment(comment.replace("\\\"", "\""));
			}
			else{
				thmo.setComment(comment);
			}
		}
	}
	
	/**
	 * Queries the note (aka short comment) attached to the instance of a 
	 * species in the mechanism OWL file being processed.
	 */
	private void queryNote(String instance, Species species){
		String note = readNote(instance);
		if (note == null) {
		} else if (note.trim().equals(EMPTY)) {
			// When fulfils the above condition, using empty allows us
			// to have proper syntactic alignment with source CTML.
			// However, this is more to do with aesthetic, nothing to
			// really important from semantic perspective.
			species.setNote(EMPTY);
		} else {
			species.setNote(note);
		}
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the size of a bulk 
	 * phase species being queried and retrieved from OWL.
	 * 
	 * @param speciesInstance the OWL instance of the species being queried
	 * @param species an instance of the species class
	 */
	private void querySize(String speciesInstance, Species species) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(speciesInstance, appConfigOntokin.getOntokinSpeciesSize());
		// Performs the query q and returns the result
		Size size = new Size();
		String result = performQuery(q, 1);
		if(result == null){
		} else{
			species.setSize(size);
			if(!result.isEmpty()){
				size.setValue(result);
			}
			querySizeUnits(speciesInstance, size);
		}
	}
	
	/**
	 * Forms and then performs a SPARQL query to read size of a bulk 
	 * phase species.
	 * 
	 * @param speciesInstance the OWL instance of the species being queried
	 * @param density an instance of the Density class 
	 * class
	 */
	private void querySizeUnits(String speciesInstance, Size size) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(speciesInstance, appConfigOntokin.getOntokinSpeciesSizeUnits());
		// Performs the query q and returns the result
		String units = performQuery(q, 1);
		if(units == null){
		} else{
			size.setUnits(units);
		}
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the density of a bulk 
	 * phase species being queried and retrieved from OWL.
	 * 
	 * @param speciesInstance the OWL instance of the species being queried
	 * @param species an instance of the species class
	 */
	private void queryDensity(String speciesInstance, Species species) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(speciesInstance, appConfigOntokin.getOntokinSpeciesDensity());
		// Performs the query q and returns the result
		Density density = new Density();
		String result = performQuery(q, 1);
		if(result == null){
		} else{
			species.setDensity(density);
			if(!result.isEmpty()){
				density.setValue(result);
			}
			queryDensityUnits(speciesInstance, density);
		}
	}
	
	/**
	 * Forms and then performs a SPARQL query to read density of a bulk 
	 * phase species.
	 * 
	 * @param speciesInstance the OWL instance of the species being queried
	 * @param density an instance of the Density class 
	 * class
	 */
	private void queryDensityUnits(String speciesInstance, Density density) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(speciesInstance, appConfigOntokin.getOntokinSpeciesDensityUnits());
		// Performs the query q and returns the result
		String units = performQuery(q, 1);
		if(units == null){
		} else{
			density.setUnits(units);
		}
	}

	
	/**
	 * Queries the atom array attached to the instance of a species in the
	 * mechanism OWL file being processed.
	 */
	private void queryAtomArray(String instance, Species species){
		try {
			String q = formQueryWithBaseURL(instance, appConfigOntokin.getObjectPropertyElementSpecification());
			performMultilineAnswerQuery(q, 2);
		} catch (Exception e) {
			e.printStackTrace();
		}
		Collections.sort(queryResult);
		// Calls the setElementArray method to extract element names of 
		// a phase from OWL and assign them the element array of a phase
		getAtomSpecificationArray(queryResult, species);
		queryResult = new ArrayList<String>();
	}
	
	/**
	 * Queries the thermo model attached to the instance of a species in the
	 * mechanism OWL file being processed.
	 */
	private void queryThermoModels(String instance, SpeciesThermo thermo){
		try {
			String q = formQueryWithBaseURL(instance, appConfigOntokin.getObjectPropertyHasThermoModel());
			performMultilineAnswerQuery(q, 2);
		} catch (Exception e) {
			e.printStackTrace();
		}
		Collections.sort(queryResult);
		// Calls the setElementArray method to extract element names of 
		// a phase from OWL and assign them the element array of a phase
		queryThermoModels(queryResult, thermo);
		queryResult = new ArrayList<String>();
	}
	
	/**
	 * Queries the transport (tx) model attached to a species in the mechanism
	 * OWL file being processed.
	 * 
	 * @param instance
	 * @param species
	 */
	private void queryTransportModel(String instance, Species species){
		try {
			String q = formQueryWithBaseURL(instance, appConfigOntokin.getOntokinHasTransportParameter());
			String txModelInstance = performQuery(q, 1);
			if(txModelInstance == null){
				// It means no transport model is available so it does nothing. 
			} else if(!txModelInstance.isEmpty()){
				// As the transport model is available in the OWL 
				// representation, there is a need to create an instance of it.
				SpeciesTransport  transport = new SpeciesTransport(); 
				species.setTransport(transport);
				readTransportModel(txModelInstance, transport);
				queryTxportComment(txModelInstance, transport);
				readString(txModelInstance, transport);
				readLJWelldepth(txModelInstance, transport);
				readLJDiameter(txModelInstance, transport);
				readDipoleMoment(txModelInstance, transport);
				readPolarizability(txModelInstance, transport);
				readRotRelax(txModelInstance, transport);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Forms and then performs a SPARQL query to read the rotational 
	 * relaxation collision number of a species.
	 * 
	 * @param txModelInstance the transport model instance
	 * @param transport an instance of the transport class
	 */
	private void readRotRelax(String txModelInstance, SpeciesTransport transport) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(txModelInstance, appConfigOntokin.getOntokinTransportRotRelax());
		// Performs the query q and returns the result
		RotRelax rotRelax = new RotRelax();
		String result = performQuery(q, 1);
		transport.setRotRelax(rotRelax);
		if(result!=null && !result.isEmpty()){
			rotRelax.setValue(result);
		}
		readRotRelaxUnits(txModelInstance, rotRelax);
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the units of the 
	 * rotational relaxation collision number of a species.
	 * 
	 * @param txModelInstance the transport model instance
	 * @param rotRelax an instance of the RotRelax class 
	 * class
	 */
	private void readRotRelaxUnits(String txModelInstance, RotRelax rotRelax) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(txModelInstance, appConfigOntokin.getOntokinTransportRotRelaxUnits());
		// Performs the query q and returns the result
		String units = performQuery(q, 1);
		if(units!=null && !units.isEmpty()){
			rotRelax.setUnits(units);
		}
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the polarizability 
	 * of species.
	 * 
	 * @param txModelInstance the transport model instance
	 * @param transport an instance of the transport class
	 */
	private void readPolarizability(String txModelInstance, SpeciesTransport transport) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(txModelInstance, appConfigOntokin.getOntokinTransportPolarizability());
		// Performs the query q and returns the result
		Polarizability polarizability = new Polarizability();
		String result = performQuery(q, 1);
		transport.setPolarizability(polarizability);
		if(result!=null && !result.isEmpty()){
			polarizability.setValue(result);
		}
		readPolarizabilityUnits(txModelInstance, polarizability);
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the units of the 
	 * polarizability of a species.
	 * 
	 * @param txModelInstance the transport model instance
	 * @param polarizability an instance of the Polarizability class 
	 * class
	 */
	private void readPolarizabilityUnits(String txModelInstance, Polarizability polarizability) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(txModelInstance, appConfigOntokin.getOntokinTransportPolarizabilityUnits());
		// Performs the query q and returns the result
		String units = performQuery(q, 1);
		if(units!=null && !units.isEmpty()){
			polarizability.setUnits(units);
		}
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the dipole moment 
	 * of species.
	 * 
	 * @param txModelInstance the transport model instance
	 * @param transport an instance of the transport class
	 */
	private void readDipoleMoment(String txModelInstance, SpeciesTransport transport) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(txModelInstance, appConfigOntokin.getOntokinTransportDipoleMoment());
		// Performs the query q and returns the result
		DipoleMoment dipoleMoment = new DipoleMoment();
		String result = performQuery(q, 1);
		transport.setDipoleMoment(dipoleMoment);
		if(result!=null && !result.isEmpty()){
			dipoleMoment.setValue(result);
		}
		readDipoleMomentUnits(txModelInstance, dipoleMoment);
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the units of the 
	 * dipole moment of a species.
	 * 
	 * @param txModelInstance the transport model instance
	 * @param dipoleMoment an instance of the Dipole Moment class 
	 * class
	 */
	private void readDipoleMomentUnits(String txModelInstance, DipoleMoment dipoleMoment) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(txModelInstance, appConfigOntokin.getOntokinTransportDipoleMomentUnits());
		// Performs the query q and returns the result
		String units = performQuery(q, 1);
		if(units!=null && !units.isEmpty()){
			dipoleMoment.setUnits(units);
		}
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the Lennard-Jones
	 * collision diameter of a species. 
	 * 
	 * @param txModelInstance the transport model instance
	 * @param transport an instance of the transport class
	 */
	private void readLJDiameter(String txModelInstance, SpeciesTransport transport) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(txModelInstance, appConfigOntokin.getOntokinTransportLJDiameter());
		// Performs the query q and returns the result
		LJDiameter lJDiameter = new LJDiameter();
		String result = performQuery(q, 1);
		transport.setLJ_diameter(lJDiameter);
		if(result!=null && !result.isEmpty()){
			lJDiameter.setValue(result);
		}
		readLJDiameterUnits(txModelInstance, lJDiameter);
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the units of the 
	 * Lennard-Jones collision diameter of a species.
	 * 
	 * @param txModelInstance the transport model instance
	 * @param ljDiameter an instance of the Lennard-Jones Collision Diameter 
	 * class
	 */
	private void readLJDiameterUnits(String txModelInstance, LJDiameter ljDiameter) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(txModelInstance, appConfigOntokin.getOntokinTransportLJDiameterUnits());
		// Performs the query q and returns the result
		String units = performQuery(q, 1);
		if(units!=null && !units.isEmpty()){
			ljDiameter.setUnits(units);
		}
	}

	
	/**
	 * Forms and then performs a SPARQL query to read the Lennard-Jones 
	 * potential well depth of a species. 
	 * 
	 * @param txModelInstance the transport model instance
	 * @param transport an instance of the transport class
	 */
	private void readLJWelldepth(String txModelInstance, SpeciesTransport transport) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(txModelInstance, appConfigOntokin.getOntokinTransportLJWellDepth());
		// Performs the query q and returns the result
		LJWellDepth lJWellDepth = new LJWellDepth();
		String result = performQuery(q, 1);
		transport.setLJ_welldepth(lJWellDepth);
		if(result!=null && !result.isEmpty()){
			lJWellDepth.setValue(result);
		}
		readLJWelldepthUnits(txModelInstance, lJWellDepth);
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the units of the Lennard-Jones 
	 * potential well depth of a species.
	 * 
	 * @param txModelInstance the transport model instance
	 * @param lJWellDepth an instance of the Lennard-Jones Well Depth class
	 */
	private void readLJWelldepthUnits(String txModelInstance, LJWellDepth lJWellDepth) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(txModelInstance, appConfigOntokin.getOntokinTransportLJWellDepthUnits());
		// Performs the query q and returns the result
		String units = performQuery(q, 1);
		if(units!=null && !units.isEmpty()){
			lJWellDepth.setUnits(units);
		}
	}
	
	/**
	 * Queries the comment about transport model attached to the instance of 
	 * a species in the mechanism OWL file being processed.
	 *  
	 * @param instance 
	 * @param txport
	 */
	private void queryTxportComment(String instance, SpeciesTransport txport){
		// Called the common method to retrieve the transport comment, 
		// if available.
		String comment = readComment(instance);
		if(comment==null){
		} else if(comment.equals(EMPTY)){
			TransportComment txComment = new TransportComment();
			txComment.setValue(EMPTY);
			txport.setComment(txComment);
		}
		else{
			TransportComment txportComment = new TransportComment();
			txport.setComment(txportComment);
			if(comment.contains("\\\\")){
				comment = comment.replace("\\\\", "\\");
			} else if(comment.contains("\\\"")){
				comment = comment.replace("\\\"", "\"");
			}
			txportComment.setValue(comment);
		}
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the name of the 
	 * transport model attached to a species. 
	 * 
	 * @param txModelInstance the transport model instance
	 * @param transport
	 */
	private void readTransportModel(String txModelInstance, SpeciesTransport transport) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(txModelInstance, appConfigOntokin.getTransportModel());
		// Performs the query q and returns the result
		String result = performQuery(q, 1);
		if(result!=null && !result.isEmpty()){
			transport.setModel(result);
		}
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the string (or species geometry type). 
	 * 
	 * @param txModelInstance the transport model instance
	 * @param transport
	 */
	private void readString(String txModelInstance, SpeciesTransport transport) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(txModelInstance, appConfigOntokin.getOntokinTransportString());
		// Performs the query q and returns the result
		STRING string = new STRING();
		String result = performQuery(q, 1);
		transport.setString(string);
		if(result!=null && !result.isEmpty()){
			string.setValue(result);
		}
		readStringTitle(txModelInstance, string);
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the title of the 
	 * string (or species geometry type).
	 * 
	 * @param txModelInstance the transport model instance
	 * @param string an instance of the species geometry type
	 */
	private void readStringTitle(String txModelInstance, STRING string) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(txModelInstance, appConfigOntokin.getOntokinTransportStringTitle());
		// Performs the query q and returns the result
		String result = performQuery(q, 1);
		if(result!=null && !result.isEmpty()){
			string.setTitle(result);
		}
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the expected phase of a species. 
	 * 
	 * @return the phase of a species
	 */
	private String readPhase(String speciesInstance) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(speciesInstance, appConfigOntokin.getOntokinSpeciesPhase());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the comment about thermo
	 * data of a species. 
	 * 
	 * @return the comment about a species
	 */
	private String readThermoComment(String speciesInstance) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(speciesInstance, appConfigOntokin.getOntokinThermoComment());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the note expressing
	 * a short comment about a species. 
	 * 
	 * @return the note attached to a species
	 */
	private String readNote(String speciesInstance) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(speciesInstance, appConfigOntokin.getOntokinNote());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Deals with processing OWL chemical element names of species and convert 
	 * them to CTML compatible names. 
	 * 
	 * 
	 * @param queryResult
	 * @param species
	 */
	private void getAtomSpecificationArray(List<String> queryResult, Species species){
		// Declared the elementsExtraced variable to save an array of 
		// elements extracted from a phase.
		String atomsExtraced = EMPTY;
		for(String elementSpecificationInstance:queryResult){
			atomsExtraced = atomsExtraced.concat(getAtom(elementSpecificationInstance));
			atomsExtraced = atomsExtraced.concat(COLON)
					.concat(getNumberOfAtoms(elementSpecificationInstance)).concat(SPACE);
		}
		species.setAtomArray(atomsExtraced.trim());
	}
	

	/**
	 * Deals with querying thermo models attached to species represented in 
	 * OWL and convert them to CTML compatible names.
	 * 
	 * @param queryResult
	 * @param thermo
	 */
	private void queryThermoModels(List<String> queryResult, SpeciesThermo thermo){
		// Created an instance of the NASA class to add all the thermo data of
		// the species being proceesed to it.
		ArrayList<NASA> nasaList = new ArrayList<NASA>();
		thermo.setNASA(nasaList);
		for(String thermoModel:queryResult){
			NASA nasa = new NASA();
			nasaList.add(nasa);
			readNASAParameters(thermoModel, nasa);
		}
	}
	/**
	 * Reads all NASA parameters.
	 * 
	 * @param instance
	 * @param nasa
	 */
	private void readNASAParameters(String thermoModelInstance, NASA nasa){
		readTmax(thermoModelInstance, nasa);
		readTmin(thermoModelInstance, nasa);
		readP0(thermoModelInstance, nasa);
		FloatArray floatArray = new FloatArray();
		nasa.setFloatArray(floatArray);
		readFloatArrayName(thermoModelInstance, floatArray);
		readFloatArraySize(thermoModelInstance, floatArray);
		readNASAParameterValues(thermoModelInstance, floatArray);
	}
	
	/**
	 * Reads the values of NASA (7 or 9) polynomials.
	 * 
	 * @param thermoModelInstance
	 * @param nasa
	 */
	private void readNASAParameterValues(String thermoModelInstance, FloatArray floatArray){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(thermoModelInstance, appConfigOntokin.getDataPropertyHasCoeffValues());
		// Performs the query q and returns the result
		floatArray.setValue(performQuery(q, 1));
	}
	
	/**
	 * Reads the size of float array.
	 * 
	 * @param thermoModelInstance
	 * @param nasa
	 */
	private void readFloatArraySize(String thermoModelInstance, FloatArray floatArray){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(thermoModelInstance, appConfigOntokin.getOntokinHasNumberOfCoefficients());
		// Performs the query q and returns the result
		floatArray.setSize(performQuery(q, 1));
	}
	
	/**
	 * Reads the name of float array.
	 * 
	 * @param thermoModelInstance
	 * @param nasa
	 */
	private void readFloatArrayName(String thermoModelInstance, FloatArray floatArray){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithAStandardVocabulary(RDFS, RDFS_URL, thermoModelInstance, RDFS_LABEL);
		// Performs the query q and returns the result
		floatArray.setName(performQuery(q, 1));
	}
	
	/**
	 * Reads the P0 NASA parameter.
	 * 
	 * @param thermoModelInstance
	 * @param nasa
	 */
	private void readP0(String thermoModelInstance, NASA nasa){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(thermoModelInstance, appConfigOntokin.getOntokinNASACoefficientP0());
		// Performs the query q and returns the result
		nasa.setP0(performQuery(q, 1));
	}
	
	/**
	 * Reads the Tmin NASA parameter.
	 * 
	 * @param thermoModelInstance
	 * @param nasa
	 */
	private void readTmin(String thermoModelInstance, NASA nasa){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(thermoModelInstance, appConfigOntokin.getOntokinNASACoefficientTmin());
		// Performs the query q and returns the result
		nasa.setTmin(performQuery(q, 1));
	}
	
	/**
	 * Reads the Tmax NASA parameter.
	 * 
	 * @param thermoModelInstance
	 * @param nasa
	 */
	private void readTmax(String thermoModelInstance, NASA nasa){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(thermoModelInstance, appConfigOntokin.getOntokinNASACoefficientTmax());
		// Performs the query q and returns the result
		nasa.setTmax(performQuery(q, 1));
	}
	
	/**
	 * Retrieves and return the name of a chemical element forming a species. 
	 * 
	 * @param elementSpecificationInstance
	 * @return
	 */
	private String getAtom(String elementSpecificationInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(elementSpecificationInstance, appConfigOntokin.getObjectPropertyHasAtom());
		// Performs the query q and returns the result
		String atomName = performQuery(q, 1);
		if(atomName.contains(UNDERSCORE)){
			String[] tokens = atomName.split(UNDERSCORE);
			if(tokens.length>1){
				if(tokens[1].contains("\n")){
					tokens[1] = tokens[1].replaceAll("\n", "");
				}
				return tokens[1];
			}
		} else{
			logger.error("The following chemical element name in OWL does not contain a underscore(_):"+atomName);
		}
		logger.error("Unexpected element name.");
		return atomName;
	}
	
	/**
	 * Retrieves and return the number of a chemical element forming a species. 
	 * 
	 * @param elementSpecificationInstance
	 * @return
	 */
	private String getNumberOfAtoms(String elementSpecificationInstance){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(elementSpecificationInstance, appConfigOntokin.getDataPropertyNumberOfElement());
		// Performs the query q and returns the result
		String numberOfAtoms = performQuery(q, 1);
		if(numberOfAtoms == null || numberOfAtoms.trim().equals(EMPTY)){
			logger.error("The number of specific type of chemical element in a species is empty");
		}else if(numberOfAtoms.contains("\n")){
			numberOfAtoms = numberOfAtoms.replaceAll("\n", "");
		}
		return numberOfAtoms;
	}
}
