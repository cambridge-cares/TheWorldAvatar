package com.cmclinnovations.ontochem.model.owl;

import org.xml.sax.SAXException;

import com.cmclinnovations.ontochem.model.converter.ctml.CtmlConverter;
import com.cmclinnovations.ontochem.model.exception.OntoException;

/**
 * Implements the method that forwards a call to those methods that
 * read species data and attributes from an in-memory temporary storage 
 * to pass them to the corresponding OWL writer methods.
 * 
 * @author msff2
 *
 */
public class SpeciesWriter extends CtmlConverter implements ISpeciesWriter{
	public void writer(char ch[], int start, int length) throws SAXException{
		writeCtmlSpeciesData(ch, start, length);
	}
	
	private void writeCtmlSpeciesData(char ch[], int start, int length) throws SAXException {
		if (speciesDataParseStatus.isSpeciesData()) {
			// Calls this method to write the species data
			// property values e.g. id
			writeSpeciesDataProperties();
			if (speciesParseStatus.isSpeciesDataSpecies()) {
				try{
				// Calls this method to write the species
				// properties of a mechanism e.g. name 
				writeSpeciesProperties(ch, start, length);
				}catch(Exception e){
					e.printStackTrace();
				}
			}
		}
	}
	
	private void writeSpeciesDataProperties() {
		if (speciesDataParseStatus.isSpeciesDataId()) {
			writeDataId();
			speciesDataParseStatus.setSpeciesDataId(false);
		}
		if (speciesDataParseStatus.isSpeciesDataCaseSensitive()) {
			writeCaseSensitiveInfo();
			speciesDataParseStatus.setSpeciesDataCaseSensitive(false);
		}
	}

	private void writeSpeciesProperties(char ch[], int start, int length) {
		if (speciesParseStatus.isSpeciesName()) {
			writeSpeciesName();
			// Connects a species to its phase using an object property.
			// It's done when at the time of creating a species 
			// individual in OWL. 
			connectsSpeciesToPhase();
			speciesParseStatus.setSpeciesName(false);
		}
		if (speciesParseStatus.isSpeciesPhase()) {
			writeSpeciesPhase();
			speciesParseStatus.setSpeciesPhase(false);
		}
		writeRestOfProperties(ch, start, length);
	}
	
	private void writeRestOfProperties(char ch[], int start, int length){
		writeComment(ch, start, length);
		writeNoteInfo(ch, start, length);
		writeAtomArrayInfo(ch, start, length);
		writeThermoProperties(ch, start, length);
		writeTransportProperties(ch, start, length);
	}
	
	private void writeComment(char ch[], int start, int length){
		if (speciesParseStatus.isSpeciesComment()) {
			// As this comment might be a multiline one, it is processed
			// at the CtmlConverter.endElement() method. Therefore,
			// speciesParseStatus.setSpeciesComment() is set false, in that method.
		}
	}

	private void writeNoteInfo(char ch[], int start, int length){
		if (speciesParseStatus.isSpeciesNote()) {
			speciesParseStatus.setSpeciesNote(false);
		}
	}
	
	private void writeAtomArrayInfo(char ch[], int start, int length){
		if (speciesParseStatus.isSpeciesAtomArray()) {
			speciesParseStatus.setSpeciesAtomArray(false);
		}
	}
	
	public void writeSizeInfo(String size){
		if(speciesSizeParseStatus.isSpeciesSize()){
			writeSize(size);
			if (speciesSizeParseStatus.isSpeciesSizeUnits()) {
				writeSizeUnits();
				speciesSizeParseStatus.setSpeciesSizeUnits(false);
			}
			speciesSizeParseStatus.setSpeciesSize(false);
		}
	}
	
	public void writeDensityInfo(String density){
		if(densityParseStatus.isSpeciesDensity()){
			writeDensity(density);
			if (densityParseStatus.isSpeciesDensityUnits()) {
				writeDensityUnits();
				densityParseStatus.setSpeciesDensityUnits(false);
			}
			densityParseStatus.setSpeciesDensity(false);
		}
	}
	
	private void writeThermoProperties(char ch[], int start, int length){
		if(nasaPolyParseStatus.isNASA()){
			if(nasaPolyParseStatus.isTmax()){
				nasaPolyParseStatus.setTmax(false);
			}
			if(nasaPolyParseStatus.isTmin()){
				nasaPolyParseStatus.setTmin(false);
			}
			if(nasaPolyParseStatus.isP0()){
				nasaPolyParseStatus.setP0(false);
			}
			if(coeffArrayParseStatus.isFloatArray()){
				if(coeffArrayParseStatus.isName()){
					coeffArrayParseStatus.setName(false);
				}
				if(coeffArrayParseStatus.isSize()){
					coeffArrayParseStatus.setSize(false);
				}
				coeffArrayParseStatus.setFloatArray(false);
			}
			nasaPolyParseStatus.setNASA(false);
		}
	}
	
	private void writeTransportProperties(char ch[], int start, int length){
		if(speciesTransportParseStatus.isTransport()){
			writeModelInfo(ch, start, length);
			writeTransportComment(ch, start, length);
			// The speciesTransportParseStatus.setTransport method
			// is set to false at the endElement method in 
			// the CtmlConverter class. The reason of doing so
			// is to make the parser robust.
		}
	}
	
	private void writeModelInfo(char ch[], int start, int length){
		if(speciesTransportParseStatus.isModel()){
			writeModel();
			speciesTransportParseStatus.setModel(false);
		}	
	}
	private void writeTransportComment(char ch[], int start, int length){
		if(speciesTransportParseStatus.isComment()){
		}	
	}
	
	public void writeStringInfo(String string){
		if(speciesTransportParseStatus.isString()){
			writeString(string);
			if(speciesTransportParseStatus.isTitle()){
				writeTitle();
				speciesTransportParseStatus.setTitle(false);
			}
			speciesTransportParseStatus.setString(false);
		}	
	}

	public void writeLJWellDepthInfo(String wellDepth){
		if(speciesTransportParseStatus.isLJWellDepth()){
			writeLJWellDepth(wellDepth);
			if(speciesTransportParseStatus.isLJWellDepthUnits()){
				writeLJWellDepthUnits();
				speciesTransportParseStatus.setLJWellDepthUnits(false);
			}
			speciesTransportParseStatus.setLJWellDepth(false);
		}	
	}

	public void writeLJDiameterInfo(String diameter){
		if(speciesTransportParseStatus.isLJDiameter()){
			writeLJDiameter(diameter);
			if(speciesTransportParseStatus.isLJDiameterUnits()){
				writeLJDiameterUnits();
				speciesTransportParseStatus.setLJDiameterUnits(false);
			}
			speciesTransportParseStatus.setLJDiameter(false);
		}	
	}
	
	public void writeDipoleMomentInfo(String dipoleMoment){
		if(speciesTransportParseStatus.isDipoleMoment()){
			writeDipoleMoment(dipoleMoment);
			if(speciesTransportParseStatus.isDipoleMomentUnits()){
				writeDipoleMomentUnits();
				speciesTransportParseStatus.setDipoleMomentUnits(false);
			}
			speciesTransportParseStatus.setDipoleMoment(false);
		}	
	}	

	public void writePolarizabilityInfo(String polarizability){
		if(speciesTransportParseStatus.isPolarizability()){
			writePolarizability(polarizability);
			if(speciesTransportParseStatus.isPolarizabilityUnits()){
				writePolarizabilityUnits();
				speciesTransportParseStatus.setPolarizabilityUnits(false);
			}
			speciesTransportParseStatus.setPolarizability(false);
		}	
	}
	
	public void writeRotRelaxInfo(String rotRelax){
		if(speciesTransportParseStatus.isRotRelax()){
			writeRotRelax(rotRelax);
			if(speciesTransportParseStatus.isRotRelaxUnits()){
				writeRotRelaxUnits();
				speciesTransportParseStatus.setRotRelaxUnits(false);
			}
			speciesTransportParseStatus.setRotRelax(false);
		}	
	}	


	/**
	 * Writes the size of a species
	 * 
	 * @param size number of surface sites occupied by a surface species
	 */
	private void writeSize(String size) {
		try {
			iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getOntokinSpeciesSize(), size,
					speciesId);
		} catch (OntoException e) {
			logger.error("The size of a specise could not be created.");
		}
	}
	
	/**
	 * Writes the size units of a species.
	 * Adds the size units property to the species. 
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	private void writeSizeUnits() {
		try {
			iOwlConstructWriter.addDataProperty(basePath, HASH,
					appConfigOntokin.getOntokinSpeciesSizeUnits(), speciesSize.getUnits(),speciesId);
		} catch (OntoException e) {
			logger.error("The size units of a species could not be created.");
		}
	}
	
	/**
	 * Writes the density of the current species if available.
	 * 
	 * @param density density of a bulk phase species
	 */
	private void writeDensity(String density) {
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getOntokinSpeciesDensity(), density, speciesId);
			} catch (OntoException e) {
				logger.error("The density of a specise could not be created.");
			}
	}
	
	/**
	 * Writes the density units of a species.
	 * Adds the density units property to the species. 
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	private void writeDensityUnits() {
		try {
			iOwlConstructWriter.addDataProperty(basePath, HASH,
					appConfigOntokin.getOntokinSpeciesDensityUnits(), speciesDensity.getUnits(),speciesId);
		} catch (OntoException e) {
			logger.error("The density units of a species could not be created.");
		}
	}


	/**
	 * Writes the id of a group of species relevant to a phase that appeared 
	 * in the mechanism being parsed.
	 */
	private void writeDataId() {
		try {
			iOwlConstructWriter.addSpeciesId(basePath, speciesData.getId());
		} catch (OntoException e) {
			logger.error("The id of the group of species could not be created.");
		}
	}
	
	/**
	 * Writes if the species data is case sensitive or not.
	 */
	private void writeCaseSensitiveInfo() {
		try {
			iOwlConstructWriter.addDataProperty(basePath, HASH,
					appConfigOntokin.getSpeciesDataCaseSensitivity(), speciesData.getCaseSensitive(),
					appConfigOntokin.getSpeciesMetadata(), speciesMetaDataInstanceId);
		} catch (OntoException e) {
			logger.error("The case sensitivity infomration of the species data could not be created.");
		}
	}

	/**
	 * Writes the name of a species in the mechanism OWL ontology 
	 * being generated.
	 */
	private void writeSpeciesName() {
		try {
			iOwlConstructWriter.addSpeciesName(basePath, species.getName());
			if(speciesData.getId()==null){
				logger.error("The id of a speciesData element is missing.");
			}
			speciesId = speciesUniqueIDMap.get(species.getName().concat(UNDERSCORE).concat(speciesData.getId()));
			if(speciesId == null){
				logger.error("The species id could not be generated.");
			}
		} catch (OntoException e) {
			logger.error("The name of a species could not be created.");
		}
	}
	
	/**
	 * Connects a species to its phase(s).
	 */
	private void connectsSpeciesToPhase() {
		String keyToSpeciesPhaseMap = species.getName().concat(UNDERSCORE).concat(speciesData.getId());
		if(!speciesVsPhaseClassMap.containsKey(keyToSpeciesPhaseMap)){
			logger.error("The species vs phase class map does not contain a species key.");
		}
		try {
			phaseType = speciesVsPhaseClassMap.get(keyToSpeciesPhaseMap);
			iOwlConstructWriter.addSpeciesToPhase(basePath, keyToSpeciesPhaseMap);
		} catch (OntoException e) {
			logger.error("The name of a species could not be created.");
		}
	}
	
	/**
	 * Writes the phase of a species in the mechanism being parsed
	 */
	private void writeSpeciesPhase() {
		try {
			iOwlConstructWriter.addDataProperty(basePath, HASH,
					appConfigOntokin.getOntokinSpeciesPhase(), species.getPhase(),
					speciesId);
		} catch (OntoException e) {
			logger.error("The case sensitivity infomration of the element data could not be created.");
		}
	}
	
	/**
	 * Writes the note attached to a species.
	 * 
	 * @param note occasional presence of note with a species
	 */
	public void writeNote(String note) {
		if(note!=null){
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getOntokinNote(), note, speciesId);
			} catch (OntoException e) {
				logger.error("Comment about a species could not be created.");
			}
		}
	}
	
	/**
	 * Writes the transport model of a species.
	 */
	private void writeModel() {
		iOwlConstructWriter.addTransportProperty();
		try {
			iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getOntokinTransportModel(),
					speciesTransport.getModel(), appConfigOntokin.getOntokinTransportParameter().concat(UNDERSCORE)
							.concat(Long.toString(idTransportParameter)));
		} catch (OntoException e) {
			logger.error("The units of the string title transport property could not be created.");
		}
	}
	
	/**
	 * Writes the value of the string transport property of a species.
	 * 
	 * @param string the species geometry type
	 */
	private void writeString(String string) {
		if(string!=null){
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getOntokinTransportString(), string, appConfigOntokin.getOntokinTransportParameter().concat(UNDERSCORE)
						.concat(Long.toString(idTransportParameter)));
			} catch (OntoException e) {
				logger.error("The string transport property of a species could not be created.");
			}
		}
	}

	/**
	 * Writes the string title transport property of a species.
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	private void writeTitle() {
		try {
			iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getOntokinTransportStringTitle(),
					speciesTxString.getTitle(), appConfigOntokin.getOntokinTransportParameter().concat(UNDERSCORE)
					.concat(Long.toString(idTransportParameter)));
		} catch (OntoException e) {
			logger.error("The units of the string title transport property could not be created.");
		}
	}

	/**
	 * Writes the value of the Lennard Jones well depth transport property of a species.
	 * 
	 * @param wellDepth the Lennard-Jones potential well depth of a species
	 */
	private void writeLJWellDepth(String wellDepth) {
		if (wellDepth != null) {
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getOntokinTransportLJWellDepth(),
						wellDepth, appConfigOntokin.getOntokinTransportParameter().concat(UNDERSCORE)
								.concat(Long.toString(idTransportParameter)));
			} catch (OntoException e) {
				logger.error("The Lennard Jones well depth transport property of a species could not be created.");
			}
		}
	}

	/**
	 * Writes the units of the string title transport property of a species.
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	private void writeLJWellDepthUnits() {
		try {
			iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getOntokinTransportLJWellDepthUnits(),
					speciesTxLJWellDepth.getUnits(), appConfigOntokin.getOntokinTransportParameter().concat(UNDERSCORE)
					.concat(Long.toString(idTransportParameter)));
		} catch (OntoException e) {
			logger.error("The units of the Lennard Jones well depth transport property could not be created.");
		}
	}

	/**
	 * Writes the value of the Lennard Jones collision diameter transport 
	 * property of a species.
	 * 
	 * @param diameter the Lennard-Jones collision diameter of a species.
	 */
	private void writeLJDiameter(String diameter) {
		if(diameter!=null){
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getOntokinTransportLJDiameter(), diameter, appConfigOntokin.getOntokinTransportParameter().concat(UNDERSCORE)
						.concat(Long.toString(idTransportParameter)));
			} catch (OntoException e) {
				logger.error("The Lennard Jones diameter transport property of a species could not be created.");
			}
		}
	}

	/**
	 * Writes the units of the Lennard Jones diameter transport property of a species.
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	private void writeLJDiameterUnits() {
		try {
			iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getOntokinTransportLJDiameterUnits(),
					speciesTxLJDiameter.getUnits(), appConfigOntokin.getOntokinTransportParameter().concat(UNDERSCORE)
					.concat(Long.toString(idTransportParameter)));
		} catch (OntoException e) {
			logger.error("The units of the Lennard Jones well depth transport property could not be created.");
		}
	}

	/**
	 * Writes the value of the dipolemoment transport property of a species.
	 * 
	 * @param dipoleMoment the dipole moment of a species
	 */
	private void writeDipoleMoment(String dipoleMoment) {
		if(dipoleMoment!=null){
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getOntokinTransportDipoleMoment(), dipoleMoment, appConfigOntokin.getOntokinTransportParameter().concat(UNDERSCORE)
						.concat(Long.toString(idTransportParameter)));
			} catch (OntoException e) {
				logger.error("The dipolemoment transport property of a species could not be created.");
			}
		}
	}

	/**
	 * Writes the units of the dipolemoment transport property of a species.
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	private void writeDipoleMomentUnits() {
		try {
			iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getOntokinTransportDipoleMomentUnits(),
					speciesTxDipoleMoment.getUnits(), appConfigOntokin.getOntokinTransportParameter().concat(UNDERSCORE)
					.concat(Long.toString(idTransportParameter)));
		} catch (OntoException e) {
			logger.error("The units of the dipolemoment transport property could not be created.");
		}
	}
	
	/**
	 * Writes the value of the polarizability transport property of a species.
	 * 
	 * @param polarizability the polarizability of a species
	 */
	private void writePolarizability(String polarizability) {
		if(polarizability!=null){
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getOntokinTransportPolarizability(), polarizability, appConfigOntokin.getOntokinTransportParameter().concat(UNDERSCORE)
						.concat(Long.toString(idTransportParameter)));
			} catch (OntoException e) {
				logger.error("The polarizability transport property of a species could not be created.");
			}
		}
	}

	/**
	 * Writes the units of the polarizability transport property of a species.
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	private void writePolarizabilityUnits() {
		try {
			iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getOntokinTransportPolarizabilityUnits(),
					speciesTxPolarizability.getUnits(), appConfigOntokin.getOntokinTransportParameter().concat(UNDERSCORE)
					.concat(Long.toString(idTransportParameter)));
		} catch (OntoException e) {
			logger.error("The units of the polarizability transport property could not be created.");
		}
	}
	
	/**
	 * Writes the value of the rotational relaxation number transport property of a species.
	 * 
	 * @param rotRelaxNumber the rotational relaxation collision number of a species
	 */
	private void writeRotRelax(String rotRelaxNumber) {
		if(rotRelaxNumber!=null){
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getOntokinTransportRotRelax(), rotRelaxNumber, appConfigOntokin.getOntokinTransportParameter().concat(UNDERSCORE)
						.concat(Long.toString(idTransportParameter)));
			} catch (OntoException e) {
				logger.error("The rotational relaxation transport property of a species could not be created.");
			}
		}
	}

	/**
	 * Writes the units of the rotational relaxation number transport property of a species.
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	private void writeRotRelaxUnits() {
		try {
			iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getOntokinTransportRotRelaxUnits(),
					speciesTxRotRelax.getUnits(), appConfigOntokin.getOntokinTransportParameter().concat(UNDERSCORE)
					.concat(Long.toString(idTransportParameter)));
		} catch (OntoException e) {
			logger.error("The units of the rotational relaxation number transport property could not be created.");
		}
	}

}