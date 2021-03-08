/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.nist.info;

/**
 *
 * @author pb556
 */
public class NISTSpeciesId {
	// species name
	// species formula
	// CAS registry number (if known)

	private String name = "";
	private String formula = "";
	private String CASRegNr = "";

	/**
	 * 
	 * @author NK510 (caresssd@hermes.cam.ac.uk) NISTSpeciesId is extended with
	 *         identifier, bond, geometry, scf energy, zero point energy as species
	 *         attributes (properties)
	 * 
	 */

	private String identifier = "";
	private String bond = "";
	private String geometry = "";

	private String scfEnergy = "";
	private String zeroPointEnergy = "";

	private String standardEnthalpyOfFormation = "";

	public NISTSpeciesId(String CASRegNr) {
		this.CASRegNr = CASRegNr;
	}

	
	public NISTSpeciesId(String name, String formula, String CASRegNr) {
		this.name = name;
		this.formula = formula;
		this.CASRegNr = CASRegNr;
	}

	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	/**
	 * @param identifier the identifier
	 * @param CASRegNr the cas registry id
	 * @param bond the bond
	 * @param geometry  the geometry
	 * @param standardEnthalpyOfFormation  the standard enthalpy of formation
	 * @param scfEnergy the scf energy
	 * @param zeroPointEnergy the zero point energy
	 */
	public NISTSpeciesId(String identifier, String CASRegNr, String bond, String geometry,
			String standardEnthalpyOfFormation, String scfEnergy, String zeroPointEnergy) {
		this.identifier = identifier;
		this.CASRegNr = CASRegNr;
		this.bond = bond;
		this.geometry = geometry;
		this.scfEnergy = scfEnergy;
		this.zeroPointEnergy = zeroPointEnergy;
		this.standardEnthalpyOfFormation = standardEnthalpyOfFormation;
	}

	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 	
	/**
	 * @param CASRegNr the cas registry id
	 * @param bond the bond
	 * @param geometry the geometry
	 * @param standardEnthalpyOfFormation the standard enthalpy of formation
	 * @param scfEnergy the scf energy
	 * @param zeroPointEnergy the zero point energy
	 */
	public NISTSpeciesId(String CASRegNr, String bond, String geometry, String standardEnthalpyOfFormation,
			String scfEnergy, String zeroPointEnergy) {

		this.CASRegNr = CASRegNr;
		this.bond = bond;
		this.geometry = geometry;
		this.scfEnergy = scfEnergy;
		this.zeroPointEnergy = zeroPointEnergy;
		this.standardEnthalpyOfFormation = standardEnthalpyOfFormation;
	}

	public String getName() {
		return name;
	}

	public String getFormula() {
		return formula;
	}

	public String getCASRegNr() {
		return CASRegNr;
	}

	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * @return the identifier
	 */

	public String getIdentifier() {
		return identifier;
	}

	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * @return the bond
	 */
	public String getBond() {
		return bond;
	}

	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * @return the geometry
	 */
	public String getGeometry() {
		return geometry;
	}

	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * @return the scf energy
	 */
	public String getScfEnergy() {
		return scfEnergy;
	}

	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * @param scfEnergy
	 */
	public void setScfEnergy(String scfEnergy) {
		this.scfEnergy = scfEnergy;
	}

	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * @return zero point energy
	 */
	public String getZeroPointEnergy() {
		return zeroPointEnergy;
	}

	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * @param zeroPointEnergy
	 */
	public void setZeroPointEnergy(String zeroPointEnergy) {
		this.zeroPointEnergy = zeroPointEnergy;
	}

	public String getStandardEnthalpyOfFormation() {
		return standardEnthalpyOfFormation;
	}

	public void setStandardEnthalpyOfFormation(String standardEnthalpyOfFormation) {
		this.standardEnthalpyOfFormation = standardEnthalpyOfFormation;
	}
}