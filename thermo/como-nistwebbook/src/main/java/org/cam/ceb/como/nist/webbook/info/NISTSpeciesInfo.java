/*
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 * 
 */
package org.cam.ceb.como.nist.webbook.info;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.cam.ceb.como.nist.webbook.thermochem.NISTDensity;
import org.cam.ceb.como.nist.webbook.thermochem.NISTDipoleMoment;
import org.cam.ceb.como.nist.webbook.thermochem.NISTElectronicEnergy;
import org.cam.ceb.como.nist.webbook.thermochem.NISTEnthalpy;
import org.cam.ceb.como.nist.webbook.thermochem.NISTEnthalpyOfX;
import org.cam.ceb.como.nist.webbook.thermochem.NISTEnthalpyOfXStCondition;
import org.cam.ceb.como.nist.webbook.thermochem.NISTEntropy;
import org.cam.ceb.como.nist.webbook.thermochem.NISTGibbsFreeEnergy;
import org.cam.ceb.como.nist.webbook.thermochem.NISTHeatCapacity;
import org.cam.ceb.como.nist.webbook.thermochem.NISTHenrysLawConstant;
import org.cam.ceb.como.nist.webbook.thermochem.NISTSpeciesGeometry;
import org.cam.ceb.como.nist.webbook.thermochem.NISTVolume;

/**
 *	A class created for modelling a chemical species.
 *
 * @author pb556
 * @author msff2
 *  
 */
public class NISTSpeciesInfo {

    private String name;
    private String inchi;
    private String key;
    private String cas;
    private double weight = 0.0;
    private ChemFormula formula;
    private Collection<String> names = new HashSet<String>();
    private Collection<String> isotopologues = new HashSet<String>();
    private String url2D = "";
    private String url3D = "";
    private NISTTemperature tBoil;
    private List<NISTTemperature> tCritical = new ArrayList<NISTTemperature>();
    private List<NISTPressure> pCritical = new ArrayList<NISTPressure>();
    private List<NISTVolume> vCritical = new ArrayList<NISTVolume>();
    private List<NISTDensity> ρCritical = new ArrayList<NISTDensity>();
    private List<NISTEnthalpyOfXStCondition> enthalpyOfVapAtStC = new ArrayList<NISTEnthalpyOfXStCondition>();
    private List<NISTEnthalpyOfXStCondition> enthalpyOfSubAtStC = new ArrayList<NISTEnthalpyOfXStCondition>();
    private List<NISTEnthalpyOfX> enthalpiesOfVap = new ArrayList<NISTEnthalpyOfX>();
    private List<NISTEnthalpyOfX> enthalpiesOfSub = new ArrayList<NISTEnthalpyOfX>();
    private List<NISTEnthalpyOfX> enthalpiesOfFus = new ArrayList<NISTEnthalpyOfX>();
    private NISTTemperature tFusion;
    private List<NISTEnthalpy> enthalpy;
    private List<NISTEnthalpy> enthalpyOfFormationInLiquid = new ArrayList<NISTEnthalpy>();
    private List<NISTEnthalpy> enthalpyOfFormationInGas = new ArrayList<NISTEnthalpy>();
    private List<NISTEnthalpy> enthalpyOfFormationInSolid = new ArrayList<NISTEnthalpy>();
    private List<NISTEnthalpy> enthalpyOfCombustionInGas = new ArrayList<NISTEnthalpy>();
    private List<NISTEnthalpy> enthalpyOfCombustionInLiquid = new ArrayList<NISTEnthalpy>();
    private List<NISTEnthalpy> enthalpyOfCombustionInSolid = new ArrayList<NISTEnthalpy>();
    private List<NISTEnthalpy> enthalpyOfReactions = new ArrayList<NISTEnthalpy>();
    private List<NISTEntropy> entropyOfReactions = new ArrayList<NISTEntropy>();
    private List<NISTGibbsFreeEnergy> gibbsFreeEnergyOfReactions = new ArrayList<NISTGibbsFreeEnergy>();
    private List<NISTEntropy> entropyInLiquidAtStCondition = new ArrayList<NISTEntropy>();
    private List<NISTEntropy> entropyInGasAtStCondition = new ArrayList<NISTEntropy>();
    private List<NISTEntropy> entropyInSolidAtStCondition = new ArrayList<NISTEntropy>();
    private List<NISTEntropy> entropyOfFusion = new ArrayList<NISTEntropy>();
    private List<NISTEntropy> entropyOfSublimation = new ArrayList<NISTEntropy>();
    private List<NISTEntropy> entropyOfVaporisation = new ArrayList<NISTEntropy>();
    private List<NISTHenrysLawConstant> henrysLawConstant = new ArrayList<NISTHenrysLawConstant>();
    private String phase;
    private int unpairedElectrons;
    private int pairedElectrons;
    private int electrons;
    private List<Map<String, List<Integer>>> bondType;
    private List<AppearanceEnergy> appearanceEnergy;
    private List<IonisationEnergy> ionisationEnergy;
    private List<NISTHeatCapacity> heatCapacityOfGas;
    private List<NISTHeatCapacity> heatCapacityOfLiquid;
    private List<NISTHeatCapacity> heatCapacityOfSolid;
    private NISTSpeciesGeometry speciesGeometry;
    private NISTDipoleMoment dipoleMoment;
    private NISTElectronicEnergy electronicEnergy;
    private List<NISTTemperature> tTriple = new ArrayList<NISTTemperature>();
    private List<NISTPressure> pTriple = new ArrayList<NISTPressure>();
    
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
	 * Returns the boiling point of the current species including the temperature and units.
	 * 
	 * @return
	 */
	public NISTTemperature gettBoil() {
		return tBoil;
	}

	/**
	 * Sets the boiling point of the current species including the temperature and units.
	 * 
	 * @param tBoil
	 */
	public void settBoil(NISTTemperature tBoil) {
		this.tBoil = tBoil;
	}
	
	public List<NISTPressure> getpTriple() {
		return pTriple;
	}

	/**
	 * Returns the critical point of the current species including the temperature and units.
	 * 
	 * @return
	 */
	public List<NISTTemperature> gettCritical() {
		return tCritical;
	}

	/**
	 * Sets the critical point of the current species including the temperature and units.
	 * 
	 * @param tCritical
	 */
	public void settCritical(List<NISTTemperature> tCritical) {
		this.tCritical = tCritical;
	}


	public void setpTriple(List<NISTPressure> pTriple) {
		this.pTriple = pTriple;
	}


	/**
	 * Returns the fusion or melting point of the current species including the temperature and units.
	 * 
	 * @return
	 */
	public NISTTemperature gettFusion() {
		return tFusion;
	}

	/**
	 * Sets the fusion or melting point of the current species including the temperature and units.
	 * 
	 * @param tFusion
	 */
	public void settFusion(NISTTemperature tFusion) {
		this.tFusion = tFusion;
	}

	/**
	 * Returns the available enthalpies of formation of the current species</br> 
	 * including the value, units and references.
	 * 
	 * @return
	 */
	public List<NISTEnthalpy> getEnthalpy() {
		return enthalpy;
	}

	/**
	 * Sets the available enthalpies of formation of the current species</br> 
	 * including the value, units and references.
	 * 
	 * @param enthalpy
	 */
	public void setEnthalpy(List<NISTEnthalpy> enthalpy) {
		this.enthalpy = enthalpy;
	}

	/**
	 * Returns the phase of the current species at condensed state. 
	 * 
	 * @return
	 */
	public String getPhase() {
		return phase;
	}

	/**
	 * Sets the phase of the current species at condensed state.
	 * 
	 * @param phase
	 */
	public void setPhase(String phase) {
		this.phase = phase;
	}


	public int getUnpairedElectrons() {
		return unpairedElectrons;
	}


	public void setUnpairedElectrons(int unpairedElectrons) {
		this.unpairedElectrons = unpairedElectrons;
	}


	public int getPairedElectrons() {
		return pairedElectrons;
	}


	public void setPairedElectrons(int pairedElectrons) {
		this.pairedElectrons = pairedElectrons;
	}


	public int getElectrons() {
		return electrons;
	}


	public void setElectrons(int electrons) {
		this.electrons = electrons;
	}


	public List<Map<String, List<Integer>>> getBondType() {
		return bondType;
	}


	public void setBondType(List<Map<String, List<Integer>>> bondType) {
		this.bondType = bondType;
	}


	public List<AppearanceEnergy> getAppearanceEnergy() {
		return appearanceEnergy;
	}


	public void setAppearanceEnergy(List<AppearanceEnergy> appearanceEnergy) {
		this.appearanceEnergy = appearanceEnergy;
	}


	public List<IonisationEnergy> getIonisationEnergy() {
		return ionisationEnergy;
	}


	public void setIonisationEnergy(List<IonisationEnergy> ionisationEnergy) {
		this.ionisationEnergy = ionisationEnergy;
	}


	public List<NISTHeatCapacity> getHeatCapacityOfGas() {
		return heatCapacityOfGas;
	}


	public void setHeatCapacityOfGas(List<NISTHeatCapacity> heatCapacityOfGas) {
		this.heatCapacityOfGas = heatCapacityOfGas;
	}


	public NISTSpeciesGeometry getSpeciesGeometry() {
		return speciesGeometry;
	}


	public void setSpeciesGeometry(NISTSpeciesGeometry speciesGeometry) {
		this.speciesGeometry = speciesGeometry;
	}


	public NISTDipoleMoment getDipoleMoment() {
		return dipoleMoment;
	}


	public void setDipoleMoment(NISTDipoleMoment dipoleMoment) {
		this.dipoleMoment = dipoleMoment;
	}

	/**
	 * Returns the electronic or total energy of the current species
	 * @return
	 */
	public NISTElectronicEnergy getElectronicEnergy() {
		return electronicEnergy;
	}

	/**
	 * Sets the electronic or total energy of the current species.
	 * 
	 * @param electronicEnergy
	 */
	public void setElectronicEnergy(NISTElectronicEnergy electronicEnergy) {
		this.electronicEnergy = electronicEnergy;
	}

	/**
	 * Returns the triple point temperature(s) of the current species.
	 * 
	 * @return
	 */
	public List<NISTTemperature> gettTriple() {
		return tTriple;
	}

	/**
	 * Sets the triple point temperature(s) of the current species.
	 * 
	 * @param tTriple
	 */
	public void settTriple(List<NISTTemperature> tTriple) {
		this.tTriple = tTriple;
	}


	public List<NISTPressure> getpCritical() {
		return pCritical;
	}


	public void setpCritical(List<NISTPressure> pCritical) {
		this.pCritical = pCritical;
	}


	public List<NISTVolume> getvCritical() {
		return vCritical;
	}


	public void setvCritical(List<NISTVolume> vCritical) {
		this.vCritical = vCritical;
	}


	public List<NISTDensity> getρCritical() {
		return ρCritical;
	}


	public void setρCritical(List<NISTDensity> ρCritical) {
		this.ρCritical = ρCritical;
	}


	public List<NISTEnthalpyOfXStCondition> getEnthalpyOfVapAtStC() {
		return enthalpyOfVapAtStC;
	}


	public void setEnthalpyOfVapAtStC(List<NISTEnthalpyOfXStCondition> enthalpyOfVapAtStC) {
		this.enthalpyOfVapAtStC = enthalpyOfVapAtStC;
	}


	public List<NISTEnthalpyOfXStCondition> getEnthalpyOfSubAtStC() {
		return enthalpyOfSubAtStC;
	}


	public void setEnthalpyOfSubAtStC(List<NISTEnthalpyOfXStCondition> enthalpyOfSubAtStC) {
		this.enthalpyOfSubAtStC = enthalpyOfSubAtStC;
	}


	public List<NISTEnthalpyOfX> getEnthalpiesOfVap() {
		return enthalpiesOfVap;
	}


	public void setEnthalpiesOfVap(List<NISTEnthalpyOfX> enthalpiesOfVap) {
		this.enthalpiesOfVap = enthalpiesOfVap;
	}


	public List<NISTEnthalpyOfX> getEnthalpiesOfSub() {
		return enthalpiesOfSub;
	}


	public void setEnthalpiesOfSub(List<NISTEnthalpyOfX> enthalpiesOfSub) {
		this.enthalpiesOfSub = enthalpiesOfSub;
	}


	public List<NISTEnthalpyOfX> getEnthalpiesOfFus() {
		return enthalpiesOfFus;
	}


	public void setEnthalpiesOfFus(List<NISTEnthalpyOfX> enthalpiesOfFus) {
		this.enthalpiesOfFus = enthalpiesOfFus;
	}


	public List<NISTHeatCapacity> getHeatCapacityOfLiquid() {
		return heatCapacityOfLiquid;
	}


	public void setHeatCapacityOfLiquid(List<NISTHeatCapacity> heatCapacityOfLiquid) {
		this.heatCapacityOfLiquid = heatCapacityOfLiquid;
	}


	public List<NISTHeatCapacity> getHeatCapacityOfSolid() {
		return heatCapacityOfSolid;
	}


	public void setHeatCapacityOfSolid(List<NISTHeatCapacity> heatCapacityOfSolid) {
		this.heatCapacityOfSolid = heatCapacityOfSolid;
	}


	public List<NISTEnthalpy> getEnthalpyOfFormationInLiquid() {
		return enthalpyOfFormationInLiquid;
	}


	public void setEnthalpyOfFormationInLiquid(List<NISTEnthalpy> enthalpyOfFormationInLiquid) {
		this.enthalpyOfFormationInLiquid = enthalpyOfFormationInLiquid;
	}


	public List<NISTEnthalpy> getEnthalpyOfCombustionInLiquid() {
		return enthalpyOfCombustionInLiquid;
	}


	public void setEnthalpyOfCombustionInLiquid(List<NISTEnthalpy> enthalpyOfCombustionInLiquid) {
		this.enthalpyOfCombustionInLiquid = enthalpyOfCombustionInLiquid;
	}


	public List<NISTEnthalpy> getEnthalpyOfFormationInGas() {
		return enthalpyOfFormationInGas;
	}


	public void setEnthalpyOfFormationInGas(List<NISTEnthalpy> enthalpyOfFormationInGas) {
		this.enthalpyOfFormationInGas = enthalpyOfFormationInGas;
	}


	public List<NISTEntropy> getEntropyInLiquidAtStCondition() {
		return entropyInLiquidAtStCondition;
	}


	public void setEntropyInLiquidAtStCondition(List<NISTEntropy> entropyInLiquidAtStCondition) {
		this.entropyInLiquidAtStCondition = entropyInLiquidAtStCondition;
	}


	public List<NISTEntropy> getEntropyInGasAtStCondition() {
		return entropyInGasAtStCondition;
	}


	public void setEntropyInGasAtStCondition(List<NISTEntropy> entropyInGasAtStCondition) {
		this.entropyInGasAtStCondition = entropyInGasAtStCondition;
	}


	public List<NISTEntropy> getEntropyInSolidAtStCondition() {
		return entropyInSolidAtStCondition;
	}


	public void setEntropyInSolidAtStCondition(List<NISTEntropy> entropyInSolidAtStCondition) {
		this.entropyInSolidAtStCondition = entropyInSolidAtStCondition;
	}


	public List<NISTEntropy> getEntropyOfFusion() {
		return entropyOfFusion;
	}


	public void setEntropyOfFusion(List<NISTEntropy> entropyOfFusion) {
		this.entropyOfFusion = entropyOfFusion;
	}


	public List<NISTEntropy> getEntropyOfSublimation() {
		return entropyOfSublimation;
	}


	public void setEntropyOfSublimation(List<NISTEntropy> entropyOfSublimation) {
		this.entropyOfSublimation = entropyOfSublimation;
	}


	public List<NISTEntropy> getEntropyOfVaporisation() {
		return entropyOfVaporisation;
	}


	public void setEntropyOfVaporisation(List<NISTEntropy> entropyOfVaporisation) {
		this.entropyOfVaporisation = entropyOfVaporisation;
	}


	public List<NISTEnthalpy> getEnthalpyOfFormationInSolid() {
		return enthalpyOfFormationInSolid;
	}


	public void setEnthalpyOfFormationInSolid(List<NISTEnthalpy> enthalpyOfFormationInSolid) {
		this.enthalpyOfFormationInSolid = enthalpyOfFormationInSolid;
	}


	public List<NISTEnthalpy> getEnthalpyOfCombustionInSolid() {
		return enthalpyOfCombustionInSolid;
	}


	public void setEnthalpyOfCombustionInSolid(List<NISTEnthalpy> enthalpyOfCombustionInSolid) {
		this.enthalpyOfCombustionInSolid = enthalpyOfCombustionInSolid;
	}


	public List<NISTEnthalpy> getEnthalpyOfCombustionInGas() {
		return enthalpyOfCombustionInGas;
	}


	public void setEnthalpyOfCombustionInGas(List<NISTEnthalpy> enthalpyOfCombustionInGas) {
		this.enthalpyOfCombustionInGas = enthalpyOfCombustionInGas;
	}


	public List<NISTHenrysLawConstant> getHenrysLawConstant() {
		return henrysLawConstant;
	}


	public void setHenrysLawConstant(List<NISTHenrysLawConstant> henrysLawConstant) {
		this.henrysLawConstant = henrysLawConstant;
	}


	public List<NISTEnthalpy> getEnthalpyOfReactions() {
		return enthalpyOfReactions;
	}


	public void setEnthalpyOfReactions(List<NISTEnthalpy> enthalpyOfReactions) {
		this.enthalpyOfReactions = enthalpyOfReactions;
	}


	public List<NISTEntropy> getEntropyOfReactions() {
		return entropyOfReactions;
	}


	public void setEntropyOfReactions(List<NISTEntropy> entropyOfReactions) {
		this.entropyOfReactions = entropyOfReactions;
	}


	public List<NISTGibbsFreeEnergy> getGibbsFreeEnergyOfReactions() {
		return gibbsFreeEnergyOfReactions;
	}


	public void setGibbsFreeEnergyOfReactions(List<NISTGibbsFreeEnergy> gibbsFreeEnergyOfReactions) {
		this.gibbsFreeEnergyOfReactions = gibbsFreeEnergyOfReactions;
	}
}