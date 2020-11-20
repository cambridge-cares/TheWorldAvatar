package com.cmclinnovations.ontokin.model.data.structure.compchem;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * Holds the skeleton of CompChem containing its properties.
 * 
 * @author msff2
 *
 */
public class CompChem {

	private String comment;
	
	private String runDate;
	
	private String programName;
	
	private String basisSetValue;
	
	private String tMin;
	
	private HashMap<String, String> atomicMasses;
	
	private String atomicWeightUnits;
	
	private String highTCoeff;
	
	private int numberOfHighTCoeff;
	
	private String lowTCoeff;
	
	private int numberOfLowTCoeff;
	
	private String programVersion;
	
	private String phase;
	
	private HashMap<String, Integer> composition;
	
	private String levelOfTheory;
	
	private String tMid;
	
	private String name;
	
	private String tMax;
	
	private String uniqueSpeciesIRI;
	
	private String quantumCalculationIRI;

	private String thermoAgentIRI;
	
	private String commitHash;
	
	public String getComment() {
		return comment;
	}

	public void setComment(String comment) {
		this.comment = comment;
	}

	public String getRunDate() {
		return runDate;
	}

	public void setRunDate(String runDate) {
		this.runDate = runDate;
	}

	public String getProgramName() {
		return programName;
	}

	public void setProgramName(String programName) {
		this.programName = programName;
	}

	public String getBasisSetValue() {
		return basisSetValue;
	}

	public void setBasisSetValue(String basisSetValue) {
		this.basisSetValue = basisSetValue;
	}

	public String gettMin() {
		return tMin;
	}

	public void settMin(String tMin) {
		this.tMin = tMin;
	}

	public HashMap<String, String> getAtomicMasses() {
		return atomicMasses;
	}

	public void setAtomicMasses(HashMap<String, String> atomicMasses) {
		this.atomicMasses = atomicMasses;
	}

	public String getHighTCoeff() {
		return highTCoeff;
	}

	public void setHighTCoeff(String highTCoeff) {
		this.highTCoeff = highTCoeff;
	}

	public String getLowTCoeff() {
		return lowTCoeff;
	}

	public void setLowTCoeff(String lowTCoeff) {
		this.lowTCoeff = lowTCoeff;
	}

	public String getProgramVersion() {
		return programVersion;
	}

	public void setProgramVersion(String programVersion) {
		this.programVersion = programVersion;
	}

	public String getPhase() {
		return phase;
	}

	public void setPhase(String phase) {
		this.phase = phase;
	}
	
	public HashMap<String, Integer> getComposition() {
		return composition;
	}

	public void setComposition(HashMap<String, Integer> composition) {
		this.composition = composition;
	}

	public String getLevelOfTheory() {
		return levelOfTheory;
	}

	public void setLevelOfTheory(String levelOfTheory) {
		this.levelOfTheory = levelOfTheory;
	}

	public String gettMid() {
		return tMid;
	}

	public void settMid(String tMid) {
		this.tMid = tMid;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String gettMax() {
		return tMax;
	}

	public void settMax(String tMax) {
		this.tMax = tMax;
	}

	public String getUniqueSpeciesIRI() {
		return uniqueSpeciesIRI;
	}

	public void setUniqueSpeciesIRI(String uniqueSpeciesIRI) {
		this.uniqueSpeciesIRI = uniqueSpeciesIRI;
	}

	public String getQuantumCalculationIRI() {
		return quantumCalculationIRI;
	}

	public void setQuantumCalculationIRI(String quantumCalculationIRI) {
		this.quantumCalculationIRI = quantumCalculationIRI;
	}

	public int getNumberOfHighTCoeff() {
		return numberOfHighTCoeff;
	}

	public void setNumberOfHighTCoeff(int numberOfHighTCoeff) {
		this.numberOfHighTCoeff = numberOfHighTCoeff;
	}

	public int getNumberOfLowTCoeff() {
		return numberOfLowTCoeff;
	}

	public void setNumberOfLowTCoeff(int numberOfLowTCoeff) {
		this.numberOfLowTCoeff = numberOfLowTCoeff;
	}

	public String getThermoAgentIRI() {
		return thermoAgentIRI;
	}

	public void setThermoAgentIRI(String thermoAgentIRI) {
		this.thermoAgentIRI = thermoAgentIRI;
	}

	public String getCommitHash() {
		return commitHash;
	}

	public void setCommitHash(String commitHash) {
		this.commitHash = commitHash;
	}

	public String getAtomicWeightUnits() {
		return atomicWeightUnits;
	}

	public void setAtomicWeightUnits(String atomicWeightUnits) {
		this.atomicWeightUnits = atomicWeightUnits;
	}
}
