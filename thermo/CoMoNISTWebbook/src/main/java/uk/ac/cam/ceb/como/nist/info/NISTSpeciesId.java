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
     * @author NK510 (caresssd@hermes.cam.ac.uk)
     * NISTSpeciesId is extended with identifier, bond and geometry as species attributes (properties)
     * 
     */

    private String identifier="";
    private String bond="";
    private String geometry="";
    
    public NISTSpeciesId(String name, String formula, String CASRegNr) {
        this.name = name;
        this.formula = formula;
        this.CASRegNr = CASRegNr;
    }


    /**
     * @author NK510 (caresssd@hermes.cam.ac.uk)
     * 
     * @param identifier identifier created during generating species owl file 
     * @param CASRegNr the CAS registry number
     * @param bond the bond
     * @param geometry the geometry
     */
    public NISTSpeciesId(String identifier, String CASRegNr, String bond, String geometry) {
        this.identifier=identifier;
        this.CASRegNr = CASRegNr;
        this.bond=bond;
        this.geometry=geometry;
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
}