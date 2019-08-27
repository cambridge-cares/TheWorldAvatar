/*
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 * 
 */
package org.cam.ceb.como.nist.webbook.info;

import java.util.Collection;
import java.util.HashSet;

/**
 *
 * @author pb556
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *  
 */

public class NISTSpeciesInfo {

    private String name = "";
    private String inchi = "";
    private String key = "";
    private String cas = "";
    private double weight = 0.0;
    private ChemFormula formula = null;
    private Collection<String> names = new HashSet<String>();
    private Collection<String> isotopologues = new HashSet<String>();
    private String url2D = "";
    private String url3D = "";
    private String energy = "";
    private Temperature tBoil;
    private Temperature tCritical;
    private Pressure pTriple;
    private Temperature tFusion;
    
    /**
     *
     * @param permanentLink
     * @author NK510
     * 
     */
    private String permanentLink;
    
    /**
     * 
     * @param permanentLink
     * @author NK510
     * 
     */
    public void setPermanentLink(String permanentLink) {
    	
    	this.permanentLink=permanentLink;
    }
    

    public void setUrl2DMolFile(String url) {
        url2D = url;
    }
    
    public void setUrl3DSDFile(String url) {
        url3D = url;
    }
    
    public void setName(String name) {
        this.name = name;
    }

    public void setFormula(ChemFormula formula) {
        this.formula = formula;
    }

    public void setMolecularWeight(double weight) {
        this.weight = weight;
    }

    public void setInChI(String inchi) {
        this.inchi = inchi;
    }

    public void setInChIKey(String key) {
        this.key = key;
    }

    public void setCASRegNr(String nr) {
        cas = nr;
    }

    public void setOtherNames(Collection<String> names) {
        this.names = names;
    }

    public void setIsotopologues(Collection<String> isotopologues) {
        this.isotopologues = isotopologues;
    }

    public String getName() {
        return name;
    }

    public ChemFormula getFormula() {
        return formula;
    }

    public double getMolecularWeight() {
        return weight;
    }

    public String getInChI() {
        return inchi;
    }

    public String getInChIKey() {
        return key;
    }

    public String getCASRegNr() {
        return cas;
    }

    public Collection<String> getOtherNames() {
        return names;
    }

    public Collection<String> getIsotopologues() {
        return isotopologues;
    }
    
    public String getUrl2DMolFile() {
        return url2D;
    }
    
    public String getUrl3DSDFile() {
        return url3D;
    }
    
    /**
     * 
     * @author NK510
     * 
     */
    public String getPermanentLink() {
    	
    	return permanentLink;
    }

    /**
     * Returns electronic or total energy of the current species
     * 
     * @return
     */
	public String getEnergy() {
		return energy;
	}

	/**
	 * Sets electronic or total energy of the current species
	 * 
	 * @param energy
	 */
	public void setEnergy(String energy) {
		this.energy = energy;
	}

	/**
	 * Returns the boiling point of the current species including the temperature and units.
	 * 
	 * @return
	 */
	public Temperature gettBoil() {
		return tBoil;
	}

	/**
	 * Sets the boiling point of the current species including the temperature and units.
	 * 
	 * @param tBoil
	 */
	public void settBoil(Temperature tBoil) {
		this.tBoil = tBoil;
	}

	/**
	 * Returns the critical point of the current species including the temperature and units. 
	 * 
	 * @return
	 */
	public Temperature gettCritical() {
		return tCritical;
	}

	/**
	 * Sets the critical point of the current species including the temperature and units.
	 * 
	 * @param tCritical
	 */
	public void settCritical(Temperature tCritical) {
		this.tCritical = tCritical;
	}

	/**
	 * Returns the triple point of the current species including the pressure and units.
	 * 
	 * @return
	 */
	public Pressure getpTriple() {
		return pTriple;
	}

	/**
	 * Sets the triple point of the current species including the pressure and units.
	 * 
	 * @param pTriple
	 */
	public void setpTriple(Pressure pTriple) {
		this.pTriple = pTriple;
	}

	/**
	 * Returns the fusion or melting point of the current species including the temperature and units.
	 * 
	 * @return
	 */
	public Temperature gettFusion() {
		return tFusion;
	}

	/**
	 * Sets the fusion or melting point of the current species including the temperature and units.
	 * 
	 * @param tFusion
	 */
	public void settFusion(Temperature tFusion) {
		this.tFusion = tFusion;
	}
}