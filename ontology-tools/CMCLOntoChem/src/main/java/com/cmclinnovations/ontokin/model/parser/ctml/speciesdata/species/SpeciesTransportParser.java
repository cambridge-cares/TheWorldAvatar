package com.cmclinnovations.ontokin.model.parser.ctml.speciesdata.species;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontokin.model.converter.ctml.CtmlConverter;

/**
 * This class parses transport properties of a species
 * 
 * @author msff2
 *
 */
public class SpeciesTransportParser extends CtmlConverter implements ISpeciesTransportParser{
	/**
	 * Parses transport properties of a species.
	 * 
	 * @param qName
	 * @param attributes
	 */
	public void parse(String qName, Attributes attributes) {
		// Calls the method that checks the appearance of 
		// transport properties of a species.
		parseTransport(qName, attributes);
		// Calls the method that checks the appearance and read the value 
		// of the transport model attribute of a species.
		parseTransportModel(qName, attributes);
		// Calls the method that checks the appearance of 
		// a comment about transport properties of a species.
		parseComment(qName, attributes);
		// Calls the method that checks the appearance of 
		// the string metadata about transport properties of a species.
		parseString(qName, attributes);
		// Calls the method that checks the appearance and read the value 
		// of the string title attribute of a species.
		parseStringTitle(qName, attributes);
		// Calls the method that checks the appearance of 
		// the LJ well depth transport parameter of a species.
		parseLJWellDepth(qName, attributes);
		// Calls the method that checks the appearance and read the value 
		// of the LJ Well Depth transport parameter units of a species.
		parseLJWellDepthUnits(qName, attributes);
		// Calls the method that checks the appearance of 
		// the LJ diameter transport parameter of a species.
		parseLJDiameter(qName, attributes);
		// Calls the method that checks the appearance and read the value 
		// of the LJ diameter transport parameter units of a species.
		parseLJDiameterUnits(qName, attributes);
		// Calls the method that checks the appearance of 
		// the dipole moment transport parameter of a species.
		parseDipoleMoment(qName, attributes);
		// Calls the method that checks the appearance and read the value 
		// of the dipole moment transport parameter units of a species.
		parseDipoleMomentUnits(qName, attributes);
		// Calls the method that checks the appearance of 
		// the polarizability transport parameter of a species.
		parsePolarizability(qName, attributes);
		// Calls the method that checks the appearance and read the value 
		// of the polarizability transport parameter units of a species.
		parsePolarizabilityUnits(qName, attributes);
		// Calls the method that checks the appearance of 
		// the rotational relaxation collision number transport 
		// parameter of a species.
		parseRotRelax(qName, attributes);
		// Calls the method that checks the appearance and read the value 
		// of the rotational relaxation collision number transport parameter 
		// units of a species.
		parseRotRelaxUnits(qName, attributes);
		
	}
	
	/**
	 * Checks the appearance of the transport tag within the scope of a species.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseTransport(String qName, Attributes attributes) {
		if (speciesParseStatus.isSpeciesDataSpecies()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesTransport())) {
				speciesTransportParseStatus.setTransport(true);
			}
		}
	}

	/**
	 * Checks the appearance of the transport model attribute
	 * within the scope of a species.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseTransportModel(String qName, Attributes attributes) {
		if (speciesParseStatus.isSpeciesDataSpecies()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesTransport())) {
				String model = attributes.getValue(appConfigCtml.getCtmlSpeciesTransportModel());
				if (model != null) {
					speciesTransport.setModel(model);
					speciesTransportParseStatus.setModel(true);
					speciesTransportParseStatus.setTransport(true);
				}
			}
		}
	}
	
	/**
	 * Checks the appearance of the comment tag under transport properties
	 * within the scope of a species.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseComment(String qName, Attributes attributes) {
		if (speciesParseStatus.isSpeciesDataSpecies()) {
			if (speciesTransportParseStatus.isTransport()) {
				if (qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesTransportComment())) {
					speciesTransportParseStatus.setComment(true);
				}
			}
		}
	}

	/**
	 * Checks the appearance of the string tag within the scope of a species.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseString(String qName, Attributes attributes) {
		if (speciesParseStatus.isSpeciesDataSpecies()) {
			if (speciesTransportParseStatus.isTransport()) {
				if (qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesTransportString())) {
					speciesTransportParseStatus.setString(true);
				}
			}
		}
	}
	
	/**
	 * Checks the appearance of the string title transport metadata within the 
	 * scope of a species.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseStringTitle(String qName, Attributes attributes) {
		if (speciesParseStatus.isSpeciesDataSpecies()) {
			if (speciesTransportParseStatus.isTransport()) {
				if (qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesTransportString())) {
					String title = attributes.getValue(appConfigCtml.getCtmlTransportStringTitle());
					if (title != null) {
						speciesTxString.setTitle(title);
						speciesTransportParseStatus.setTitle(true);
						speciesTransportParseStatus.setString(true);
					}
				}
			}
		}
	}
	
	/**
	 * Checks the appearance of the LJ_welldepth transport property
	 * within the scope of a species.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseLJWellDepth(String qName, Attributes attributes) {
		if (speciesParseStatus.isSpeciesDataSpecies()) {
			if (speciesTransportParseStatus.isTransport()) {
				if (qName.equalsIgnoreCase(appConfigCtml.getCtmlTransportLJWelldepth())) {
					speciesTransportParseStatus.setLJWellDepth(true);
				}
			}
		}
	}
	
	/**
	 * Checks the appearance and reads the value of the units of the 
	 * LJ_welldepth transport property within the scope of a species.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseLJWellDepthUnits(String qName, Attributes attributes) {
		if (speciesParseStatus.isSpeciesDataSpecies()) {
			if (speciesTransportParseStatus.isTransport()) {
				if (qName.equalsIgnoreCase(appConfigCtml.getCtmlTransportLJWelldepth())) {
					String units = attributes.getValue(appConfigCtml.getCtmlTransportLJWelldepthUnits());
					if (units != null) {
						speciesTxLJWellDepth.setUnits(units);
						speciesTransportParseStatus.setLJWellDepth(true);
						speciesTransportParseStatus.setLJWellDepthUnits(true);
					}
				}
			}
		}
	}

	/**
	 * Checks the appearance of the LJ_diameter transport property
	 * within the scope of a species.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseLJDiameter(String qName, Attributes attributes) {
		if (speciesParseStatus.isSpeciesDataSpecies()) {
			if (speciesTransportParseStatus.isTransport()) {
				if (qName.equalsIgnoreCase(appConfigCtml.getCtmlTransportLJDiameter())) {
					speciesTransportParseStatus.setLJDiameter(true);
				}
			}
		}
	}
	
	/**
	 * Checks the appearance and reads the value of the units of the 
	 * LJ_diameter transport property within the scope of a species.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseLJDiameterUnits(String qName, Attributes attributes) {
		if (speciesParseStatus.isSpeciesDataSpecies()) {
			if (speciesTransportParseStatus.isTransport()) {
				if (qName.equalsIgnoreCase(appConfigCtml.getCtmlTransportLJDiameter())) {
					String units = attributes.getValue(appConfigCtml.getCtmlTransportLJDiameterUnits());
					if (units != null) {
						speciesTxLJDiameter.setUnits(units);
						speciesTransportParseStatus.setLJDiameter(true);
						speciesTransportParseStatus.setLJDiameterUnits(true);
					}
				}
			}
		}
	}

	/**
	 * Checks the appearance of the dipolemoment transport property
	 * within the scope of a species.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseDipoleMoment(String qName, Attributes attributes) {
		if (speciesParseStatus.isSpeciesDataSpecies()) {
			if (speciesTransportParseStatus.isTransport()) {
				if (qName.equalsIgnoreCase(appConfigCtml.getCtmlTransportDipoleMoment())) {
					speciesTransportParseStatus.setDipoleMoment(true);
				}
			}
		}
	}
	
	/**
	 * Checks the appearance and reads the value of the units of the 
	 * dipolemoment transport property within the scope of a species.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseDipoleMomentUnits(String qName, Attributes attributes) {
		if (speciesParseStatus.isSpeciesDataSpecies()) {
			if (speciesTransportParseStatus.isTransport()) {
				if (qName.equalsIgnoreCase(appConfigCtml.getCtmlTransportDipoleMoment())) {
					String units = attributes.getValue(appConfigCtml.getCtmlTransportDipoleMomentUnits());
					if (units != null) {
						speciesTxDipoleMoment.setUnits(units);
						speciesTransportParseStatus.setDipoleMoment(true);
						speciesTransportParseStatus.setDipoleMomentUnits(true);
					}
				}
			}
		}
	}	
	
	/**
	 * Checks the appearance of the polarizability transport property
	 * within the scope of a species.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parsePolarizability(String qName, Attributes attributes) {
		if (speciesParseStatus.isSpeciesDataSpecies()) {
			if (speciesTransportParseStatus.isTransport()) {
				if (qName.equalsIgnoreCase(appConfigCtml.getCtmlTransportPolarizability())) {
					speciesTransportParseStatus.setPolarizability(true);
				}
			}
		}
	}
	
	/**
	 * Checks the appearance and reads the value of the units of the 
	 * polarizability transport property within the scope of a species.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parsePolarizabilityUnits(String qName, Attributes attributes) {
		if (speciesParseStatus.isSpeciesDataSpecies()) {
			if (speciesTransportParseStatus.isTransport()) {
				if (qName.equalsIgnoreCase(appConfigCtml.getCtmlTransportPolarizability())) {
					String units = attributes.getValue(appConfigCtml.getCtmlTransportPolarizabilityUnits());
					if (units != null) {
						speciesTxPolarizability.setUnits(units);
						speciesTransportParseStatus.setPolarizability(true);
						speciesTransportParseStatus.setPolarizabilityUnits(true);
					}
				}
			}
		}
	}	

	/**
	 * Checks the appearance of the rotational relaxation collision number
	 * transport property within the scope of a species.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseRotRelax(String qName, Attributes attributes) {
		if (speciesParseStatus.isSpeciesDataSpecies()) {
			if (speciesTransportParseStatus.isTransport()) {
				if (qName.equalsIgnoreCase(appConfigCtml.getCtmlTransportRotRelax())) {
					speciesTransportParseStatus.setRotRelax(true);
				}
			}
		}
	}
	
	/**
	 * Checks the appearance and reads the value of the units of the 
	 * rotational relaxation collision number transport property within 
	 * the scope of a species.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseRotRelaxUnits(String qName, Attributes attributes) {
		if (speciesParseStatus.isSpeciesDataSpecies()) {
			if (speciesTransportParseStatus.isTransport()) {
				if (qName.equalsIgnoreCase(appConfigCtml.getCtmlTransportRotRelax())) {
					String units = attributes.getValue(appConfigCtml.getCtmlTransportRotRelaxUnits());
					if (units != null) {
						speciesTxRotRelax.setUnits(units);
						speciesTransportParseStatus.setRotRelax(true);
						speciesTransportParseStatus.setRotRelaxUnits(true);
					}
				}
			}
		}
	}	
}
