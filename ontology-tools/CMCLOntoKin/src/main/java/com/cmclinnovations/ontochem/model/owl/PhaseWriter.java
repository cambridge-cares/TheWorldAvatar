package com.cmclinnovations.ontochem.model.owl;

import org.xml.sax.SAXException;
import com.cmclinnovations.ontochem.model.converter.ctml.CtmlConverter;

/**
 * Implements the method that forwards a call to those methods that
 * read phase data and attributes from an in-memory temporary storage 
 * to pass them to the corresponding CTML to OWL conversion methods.
 * 
 * @author msff2
 *
 */
public class PhaseWriter extends CtmlConverter implements IPhaseWriter{
	
	public void writer(char ch[], int start, int length) throws SAXException{
		readCtmlPhase(ch, start, length);
	}
	
	private void readCtmlPhase(char ch[], int start, int length) throws SAXException {
		if (phaseParseStatus.isPhase()) {
			// Calls this method to read the phase
			// property values e.g. id and dimension
			readPhaseProperties();
			if (elementArrayParseStatus.isElementArray()) {
			}
			if (speciesArrayParseStatus.isSpeciesArray()) {
				// Calls this method to read the species
				// data from CTML
				readSpeciesArray(ch, start, length);
			}
			if (reactionArrayParseStatus.isReactionArray()) {
				// Calls this method to read the reaction
				// data source from CTML
				readReactionArray();
			}
			if (phaseStateParseStatus.isState()) {
				// Calls this method to read the state
				// of a phase from CTML
				readPhaseState(ch, start, length);
			}
			if (thermoParseStatus.isThermo()) {
				// Calls this method to read the thermo
				// property model of a phase from CTML
				readThermoProperty(ch, start, length);
				if (siteDensityParseStatus.isSiteDensity()) {
				}
			}
			if (kineticsParseStatus.isKinetics()) {
				// Calls this method to read the kinetics
				// of a phase and its model from CTML
				readKinetics();
			}
			if (transportParseStatus.isTransport()) {
				// Calls this method to read the transport
				// property of a phase and its model from CTML
				readTransport();
			}
			if (phaseArrayParseStatus.isPhaseArray()) {
			}
		}
	}
	
	/**
	 * Forwards the call to the methods that read dimension and
	 * id of a phase
	 */
	private void readPhaseProperties() {
		if (phaseParseStatus.isPhaseId()) {
			phaseParseStatus.setPhaseId(false);
		}
		if (phaseParseStatus.isPhaseDimension()) {
			phaseParseStatus.setPhaseDimension(false);
		}
		if (phaseParseStatus.isPhaseMaterial()) {
			phaseParseStatus.setPhaseMaterial(false);
		}
	}

	/**
	 * Forwards the call to the methods that read elements
	 * participating in a phase and their source
	 * 
	 * @param elementArrayData an array of elements
	 */
	public void readElementArray(String elementArrayData) {
		if (elementArrayParseStatus.isElementArray()) {
			readElementArrayData(elementArrayData);
			if (elementArrayParseStatus.isElementDataSrc()) {
				elementArrayParseStatus.setElementDataSrc(false);
			}
			elementArrayParseStatus.setElementArray(false);
		}
	}

	/**
	 * Reads elements from an element array
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	private void readElementArrayData(String elementArrayData) {
		elementArray.setValue(elementArrayData);
	}

	/**
	 * Forwards the call to the methods that read species
	 * participating in a phase and their source
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	private void readSpeciesArray(char ch[], int start, int length) {
		if (speciesArrayParseStatus.isSpeciesArray()) {
			readSpeciesArrayData(ch, start, length);
			if (speciesArrayParseStatus.isSpeciesDataSrc()) {
				speciesArrayParseStatus.setSpeciesDataSrc(false);
			}
		}
	}

	/**
	 * Reads all species from a species array. </br>
	 * Creates a HashMap with species-phase as key-value pairs
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	private void readSpeciesArrayData(char ch[], int start, int length) {
//		multiLineValue.append(ch, start, length);
	}

	/**
	 * Forwards the call to the method that reads the data
	 * source of the reactions of a phase
	 * 
	 */
	private void readReactionArray() {

		if (reactionArrayParseStatus.isReactionArray()) {
			if (reactionArrayParseStatus.isReactionDataSrc()) {
				reactionArrayParseStatus.setReactionDataSrc(false);
			}
			reactionArrayParseStatus.setReactionArray(false);
		}
	}
	

	/**
	 * Forwards the call to the method that reads the thermo
	 * property model of a phase
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	private void readThermoProperty(char ch[], int start, int length) {

		if (thermoParseStatus.isThermo()) {
			if (thermoParseStatus.isThermoModel()) {
				thermoParseStatus.setThermoModel(false);
			}
//			thermoParseStatus.setThermo(false);
		}
	}
	
	/**
	 * Forwards the call to the methods that read the state
	 * information of a phase
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	private void readPhaseState(char ch[], int start, int length) {
		if (phaseStateParseStatus.isState()) {
			readPhaseStateData(ch, start, length);
			phaseStateParseStatus.setState(false);
		}
	}

	/**
	 * Reads elements from an element array
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	private void readPhaseStateData(char ch[], int start, int length) {
		phaseState.setValue(new String(ch, start, length));
	}
	
	/**
	 * Forwards the call to the methods that read the site
	 *  density of the current phase.
	 * 
	 * @param siteDensityValue
	 */
	public void readSiteDensity(String siteDensityValue) {
		if (siteDensityParseStatus.isSiteDensity()) {
			readSiteDensityData(siteDensityValue);
			if (siteDensityParseStatus.isUnits()) {
				siteDensityParseStatus.setUnits(false);
			}
			siteDensityParseStatus.setSiteDensity(false);
		}
	}

	/**
	 * Reads the value of the site density in a phase
	 * 
	 * @param siteDensityValue
	 */
	private void readSiteDensityData(String siteDensityValue) {
		siteDensity.setValue(siteDensityValue);
	}
	
	/**
	 * Forwards the call to the method that reads the kinetics
	 * model of a phase
	 */
	private void readKinetics() {

		if (kineticsParseStatus.isKinetics()) {
			if (kineticsParseStatus.isKineticsModel()) {
				kineticsParseStatus.setKineticsModel(false);
			}
			kineticsParseStatus.setKinetics(false);
		}
	}
	
	/**
	 * Forwards the call to the method that reads the transport
	 * model of a phase
	 */
	private void readTransport() {
		if (transportParseStatus.isTransport()) {
			if (transportParseStatus.isTransportModel()) {
				transportParseStatus.setTransportModel(false);
			}
			transportParseStatus.setTransport(false);
		}
	}
	
	/**
	 * Forwards the call to the methods that read named phase
	 * array of a phase
	 * 
	 * @param phaseArrayData
	 */
	public void readPhaseArray(String phaseArrayData) {
		if (phaseArrayParseStatus.isPhaseArray()) {
			readPhaseArrayData(phaseArrayData);
			phaseArrayParseStatus.setPhaseArray(false);
		}
	}

	/**
	 * Reads named phases from a phase array.
	 * 
	 * @param phaseArrayData
	 */
	private void readPhaseArrayData(String phaseArrayData) {
		phaseMD.setPhaseArray(phaseArrayData);
	}
}
