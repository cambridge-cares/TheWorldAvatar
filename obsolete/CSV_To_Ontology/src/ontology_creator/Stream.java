package ontology_creator;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.hp.hpl.jena.util.FileUtils;

import edu.stanford.smi.protegex.owl.ProtegeOWL;
import edu.stanford.smi.protegex.owl.jena.JenaOWLModel;
import edu.stanford.smi.protegex.owl.model.OWLDatatypeProperty;
import edu.stanford.smi.protegex.owl.model.OWLIndividual;
import edu.stanford.smi.protegex.owl.model.OWLNamedClass;
import edu.stanford.smi.protegex.owl.model.OWLObjectProperty;
import edu.stanford.smi.protegex.owl.model.RDFIndividual;

public class Stream {
	private String name;
	private float vapFrac = Float.NaN;
	private float temperature = Float.NaN;
	private float pressure = Float.NaN;
	private float molarFlow = Float.NaN;
	private float massFlow = Float.NaN;
	private Map<String,Float> gasPhaseMoleComp = new HashMap<String,Float>();
	private Map<String,Float> liqPhaseMoleComp = new HashMap<String,Float>();
	private Map<String,Float> liq2PhaseMoleComp = new HashMap<String,Float>();
	private Map<String,Float> aqPhaseMoleComp = new HashMap<String,Float>();
	private Map<String,Map<String,Float>> phaseSystem = new HashMap<String,Map<String,Float>>();
	private List<String> compArray = new ArrayList<String>();
	private List<String> hysysNames = new ArrayList<String>();
	private List<Float> molWeights = new ArrayList<Float>();
	private List<String> molFormulas = new ArrayList<String>();
	private String filename;
	
	Stream(String name) {
		this.name = name;
		this.filename = "Pipe_" + getName() + ".owl"; 
	}

	public String getName() { return name; }
	public float getVapFrac() { return vapFrac; }
	public float getTemperature() {	return temperature;	}
	public float getPressure() { return pressure; }
	public float getMolarFlow() { return molarFlow; }
	public float getMassFlow() { return massFlow; }
	public Map<String,Float> getGasPhaseMoleComp() { return gasPhaseMoleComp; }
	public Map<String,Float> getLiqPhaseMoleComp() { return liqPhaseMoleComp; }
	public Map<String,Float> getLiq2PhaseMoleComp() { return liq2PhaseMoleComp; }
	public Map<String,Float> getAqPhaseMoleComp() {	return aqPhaseMoleComp;	}
	public Map<String,Map<String,Float>> getPhaseSystem() {	return phaseSystem;	}
	public List<String> getCompArray() { return compArray; }
	public List<String> getHysysNames() { return hysysNames; }
	public List<Float> getMolWeights() {	return molWeights; }
	public List<String> getMolFormulas() { return molFormulas; }
	
	public void setName(String name) { this.name = name; }
	public void setVapFrac(float vapFrac) {	this.vapFrac = vapFrac;	}
	public void setTemperature(float temperature) {	this.temperature = temperature;	}
	public void setPressure(float pressure) { this.pressure = pressure;	}
	public void setMolarFlow(float molarFlow) {	this.molarFlow = molarFlow;	}
	public void setMassFlow(float massFlow) { this.massFlow = massFlow; }
	
	public void addCompArray(List<String> compArray) { this.compArray = compArray; }
	public void addHysysName(List<String> hysysName) { this.hysysNames = hysysName; }
	public void addMolWeight(List<Float> molWeight) { this.molWeights = molWeight; }
	public void addMolFormula(List<String> molFormula) { this.molFormulas = molFormula; }

	public void addGasPhaseMoleFrac(String species, float moleFrac) { 
		if (moleFrac != 0) { 
			this.gasPhaseMoleComp.put(species, moleFrac); 
		}	
	}
	public void addLiqPhaseMoleFrac(String species, float moleFrac) { 
		if (moleFrac != 0) {
			this.liqPhaseMoleComp.put(species, moleFrac);
		} 
	}
	public void addLiq2PhaseMoleFrac(String species, float moleFrac) { 
		if (moleFrac != 0) {	
			this.liq2PhaseMoleComp.put(species, moleFrac);
		}
	}
	public void addAqPhaseMoleFrac(String species, float moleFrac) { 
		if (moleFrac != 0) {
			this.aqPhaseMoleComp.put(species, moleFrac);
		}
	}
	public boolean isMultiPhase() {
		if (getPhaseSystem().size()<=1) { return false; }
		else { return true; }
	}
	
	public Set<String> getAllComponents() {
		Set<String> allComponents = new HashSet<String>();
		for (Map<String,Float> moleComp : getPhaseSystem().values()) {
			for (String component : moleComp.keySet()) {
				allComponents.add(component);
			}
		}
		return allComponents;
	}

	public String getSingleComponent() {
		String the_one = "";
		for (Map<String,Float> moleComp : getPhaseSystem().values()) {
			for (String component : moleComp.keySet()) {
				the_one = component;
			}
		}
		return the_one;
	}

	public boolean isSingleComponent() {
		Set<String> components = getAllComponents();
		if (components.size()==1) { return true; }
		else { return false; }
	}
	
	public String hysysNameToCompArray(String hysysName) {
		int index = getHysysNames().indexOf(hysysName);
		return getCompArray().get(index);
	}
	
	public Float hysysNameToMolWeight(String hysysName) {
		int index = getHysysNames().indexOf(hysysName);
		return getMolWeights().get(index);
	}
	
	public String hysysNameToMolFormula(String hysysName) {
		int index = getHysysNames().indexOf(hysysName);
		return getMolFormulas().get(index);
	}
	
	public Float componentToMolWeight(String component) {
		int index = getCompArray().indexOf(component);
		return getMolWeights().get(index);
	}
	
	public String componentToMolFormula(String component) {
		int index = getCompArray().indexOf(component);
		return getMolFormulas().get(index);
	}
	
	public void buildMultiPhaseSystem() {
		if (!aqPhaseMoleComp.isEmpty()) {
			phaseSystem.put("Aqueous", aqPhaseMoleComp);
		}
		if (!liqPhaseMoleComp.isEmpty()) {
			phaseSystem.put("Liquid", liqPhaseMoleComp);
		}
		if (!liq2PhaseMoleComp.isEmpty()) {
			phaseSystem.put("Liquid 2", liq2PhaseMoleComp);
		}
		if (!gasPhaseMoleComp.isEmpty()) {
			phaseSystem.put("Gas", gasPhaseMoleComp);
		}
	}
	
	public void buildSinglePhaseSystem(String phase) {
		Map<String,Float> moleComp;
		if (phase.equals("Aqueous")) { moleComp = this.aqPhaseMoleComp; }
		else if (phase.equals("Liquid")) { moleComp = this.liqPhaseMoleComp; }
		else if (phase.equals("Gas")) { moleComp = this.gasPhaseMoleComp; }
		else { moleComp = null; }
		this.phaseSystem.put(phase, moleComp);
	}
	
	public void createMultiPhase(JenaOWLModel owlModel) {
		/**
		 * The function is not tested for 3-phase vapour-liquid-liquid
		 * 
		 * Precondition:
		 * owlModel is a valid JenaOWLModel object
		 * the stream is a 2-phase vapour-liquid
		 * 
		 * Postcondition:
		 * added a multi phase system into owlModel with its properties
		 * 
		 */
		
		OWLIndividual singlePhase1=owlModel.getOWLIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+ filename +"#Single_Phase_1_"+ getName());
		
		OWLObjectProperty thermodynamicBehavior=owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/material/material.owl#thermodynamicBehavior");
		OWLObjectProperty isComposedOfSubsystem=owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isComposedOfSubsystem");
		OWLObjectProperty hasStateOfAggregation=owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#hasStateOfAggregation");
		OWLObjectProperty hasTemperature=owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#has_temperature");
		OWLObjectProperty hasPressure=owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#has_pressure");
		OWLObjectProperty hasVapourFraction=owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#hasVapourRatio");
		OWLObjectProperty hasUnitOfMeasure=owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");
		OWLObjectProperty hasValue = owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");
		OWLObjectProperty hasComposition=owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#has_composition");
		OWLObjectProperty comprisesDirectly=owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#comprisesDirectly");
		OWLDatatypeProperty numericalValue = owlModel.getOWLDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
		
		OWLNamedClass scalarValue = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue");
		OWLNamedClass singlePhase=owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#SinglePhase");	
		OWLNamedClass multiPhaseSystem=owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#MultiphaseSystem");	
		OWLNamedClass temperature = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#Temperature");
		OWLNamedClass pressure = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#Pressure");
		OWLNamedClass phaseSystemProperty = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#ThermodynamicStateProperty");
		OWLNamedClass composition=owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#Composition");
		OWLNamedClass moleFrac=owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#MoleFraction");
		
		OWLIndividual celsius=owlModel.getOWLIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#Celsius");
		OWLIndividual bar=owlModel.getOWLIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#bar");
		OWLIndividual liquid=owlModel.getOWLIndividual("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#liquid");
		OWLIndividual material=owlModel.getOWLIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+ filename + "#Material_" + getName());
		
		RDFIndividual multiPhaseSystemInstance=multiPhaseSystem.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+ filename +"#Multi_Phase_System_"+ getName());
		RDFIndividual singlePhase2=singlePhase.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + filename + "#Single_Phase_2_" + getName());
		RDFIndividual temperature2=temperature.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+"#Temperature_2_"+ getName());
		RDFIndividual pressure2=pressure.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+"#Pressure_2_"+ getName());
		RDFIndividual valueTemperature2=scalarValue.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+"#Value_Of_Temperature_2_"+ getName());
		RDFIndividual valuePressure2=scalarValue.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+"#Value_Of_Pressure_2_"+ getName());
		RDFIndividual vapourRatio=phaseSystemProperty.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+"#Vapour_Ratio_"+ getName());
		RDFIndividual valueVapourRatio=scalarValue.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+"#Value_Of_Vapour_Ratio_"+ getName());
		
		valueTemperature2.setPropertyValue(hasUnitOfMeasure,celsius);
		valueTemperature2.setPropertyValue(numericalValue, getTemperature());
		valuePressure2.setPropertyValue(hasUnitOfMeasure, bar);
		valuePressure2.setPropertyValue(numericalValue, getPressure());
		valueVapourRatio.setPropertyValue(numericalValue, getVapFrac());
		
		vapourRatio.addPropertyValue(hasValue, valueVapourRatio);
		temperature2.addPropertyValue(hasValue,valueTemperature2);
		pressure2.addPropertyValue(hasValue, valuePressure2);
		singlePhase2.addPropertyValue(hasTemperature, temperature2);
		singlePhase2.addPropertyValue(hasPressure, pressure2);
		singlePhase2.addPropertyValue(hasStateOfAggregation, liquid);
		
		material.removePropertyValue(thermodynamicBehavior, singlePhase1);
		material.addPropertyValue(thermodynamicBehavior,multiPhaseSystemInstance);
		multiPhaseSystemInstance.addPropertyValue(isComposedOfSubsystem, singlePhase1);
		multiPhaseSystemInstance.addPropertyValue(isComposedOfSubsystem, singlePhase2);
		multiPhaseSystemInstance.addPropertyValue(hasVapourFraction, vapourRatio);
		
		if (getPhaseSystem().containsKey("Aqueous") && !getPhaseSystem().containsKey("Liquid")) {
			for (String species : getAqPhaseMoleComp().keySet()) {
				try {
					RDFIndividual streamSpecies=composition.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+".owl#"+species+"_2_in_"+ getName());
					RDFIndividual speciesMoleFrac=moleFrac.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+".owl#"+"Mole_Fraction_Of_"+species+"_2_in_"+ getName());
					RDFIndividual speciesMoleFracValue=scalarValue.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+".owl#"+"Value_Of_Mole_Fraction_Of_"+species+"_2_in_"+ getName());
					singlePhase2.addPropertyValue(hasComposition, streamSpecies);
					streamSpecies.addPropertyValue(comprisesDirectly, speciesMoleFrac);
					speciesMoleFrac.addPropertyValue(hasValue, speciesMoleFracValue);
					speciesMoleFracValue.setPropertyValue(numericalValue, getAqPhaseMoleComp().get(species));
				} catch(IllegalArgumentException k) {
					System.out.println("Duplicate Species Added");
				}
			}
		}
		if (getPhaseSystem().containsKey("Liquid") && !getPhaseSystem().containsKey("Aqueous")) {
			for (String species : getLiqPhaseMoleComp().keySet()) {
				try {
					RDFIndividual streamSpecies=composition.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+".owl#"+species+"_2_in_"+ getName());
					RDFIndividual speciesMoleFrac=moleFrac.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+".owl#"+"Mole_Fraction_Of_"+species+"_2_in_"+ getName());
					RDFIndividual speciesMoleFracValue=scalarValue.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+".owl#"+"Value_Of_Mole_Fraction_Of_"+species+"_2_in_"+ getName());
					singlePhase2.addPropertyValue(hasComposition, streamSpecies);
					streamSpecies.addPropertyValue(comprisesDirectly, speciesMoleFrac);
					speciesMoleFrac.addPropertyValue(hasValue, speciesMoleFracValue);
					speciesMoleFracValue.setPropertyValue(numericalValue, getLiqPhaseMoleComp().get(species));
				} catch(IllegalArgumentException k) {
					System.out.println("Duplicate Species Added");	
				}
			}
		}
		if (getPhaseSystem().containsKey("Liquid") && getPhaseSystem().containsKey("Aqueous")) {
			for (String species : getAqPhaseMoleComp().keySet()) {
				try {
					RDFIndividual streamSpecies=composition.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+".owl#"+species+"_2_in_"+ getName());
					RDFIndividual speciesMoleFrac=moleFrac.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+".owl#"+"Mole_Fraction_Of_"+species+"_2_in_"+ getName());
					RDFIndividual speciesMoleFracValue=scalarValue.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+".owl#"+"Value_Of_Mole_Fraction_Of_"+species+"_2_in_"+ getName());
					singlePhase2.addPropertyValue(hasComposition, streamSpecies);
					streamSpecies.addPropertyValue(comprisesDirectly, speciesMoleFrac);
					speciesMoleFrac.addPropertyValue(hasValue, speciesMoleFracValue);
					speciesMoleFracValue.setPropertyValue(numericalValue, getAqPhaseMoleComp().get(species));
				} catch(IllegalArgumentException k) {
					System.out.println("Duplicate Species Added");
				}
			}
			for (String species : getLiqPhaseMoleComp().keySet()) {
				try {
					RDFIndividual singlePhase3=singlePhase.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + filename + "#Single_Phase_3_" + getName());
					
					RDFIndividual streamSpecies=composition.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+".owl#"+species+"_3_in_"+ getName());
					RDFIndividual speciesMoleFrac=moleFrac.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+".owl#"+"Mole_Fraction_Of_"+species+"_3_in_"+ getName());
					RDFIndividual speciesMoleFracValue=scalarValue.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+".owl#"+"Value_Of_Mole_Fraction_Of_"+species+"_3_in_"+ getName());
					singlePhase3.addPropertyValue(hasComposition, streamSpecies);
					streamSpecies.addPropertyValue(comprisesDirectly, speciesMoleFrac);
					speciesMoleFrac.addPropertyValue(hasValue, speciesMoleFracValue);
					speciesMoleFracValue.setPropertyValue(numericalValue, getLiqPhaseMoleComp().get(species));
				} catch(IllegalArgumentException k) {
					System.out.println("Duplicate Species Added");	
				}
			}
		}
		if (getPhaseSystem().containsKey("Liquid 2")) {
			for (String species : getLiq2PhaseMoleComp().keySet()) {
				try {
					RDFIndividual singlePhase3=singlePhase.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + filename + "#Single_Phase_3_" + getName());
					
					RDFIndividual streamSpecies=composition.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+".owl#"+species+"_3_in_"+ getName());
					RDFIndividual speciesMoleFrac=moleFrac.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+".owl#"+"Mole_Fraction_Of_"+species+"_3_in_"+ getName());
					RDFIndividual speciesMoleFracValue=scalarValue.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+".owl#"+"Value_Of_Mole_Fraction_Of_"+species+"_3_in_"+ getName());
					singlePhase3.addPropertyValue(hasComposition, streamSpecies);
					streamSpecies.addPropertyValue(comprisesDirectly, speciesMoleFrac);
					speciesMoleFrac.addPropertyValue(hasValue, speciesMoleFracValue);
					speciesMoleFracValue.setPropertyValue(numericalValue, getLiq2PhaseMoleComp().get(species));
				} catch(IllegalArgumentException k) {
					System.out.println("Duplicate Species Added");	
				}
			}
		}
		if (getPhaseSystem().containsKey("Gas")) {
			for (String species : getGasPhaseMoleComp().keySet()) {
				try {
					RDFIndividual streamSpecies=composition.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+".owl#"+species+"_1_in_"+ getName());
					RDFIndividual speciesMoleFrac=moleFrac.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+".owl#"+"Mole_Fraction_Of_"+species+"_1_in_"+ getName());
					RDFIndividual speciesMoleFracValue=scalarValue.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+".owl#"+"Value_Of_Mole_Fraction_Of_"+species+"_1_in_"+ getName());
					singlePhase1.addPropertyValue(hasComposition, streamSpecies);
					streamSpecies.addPropertyValue(comprisesDirectly, speciesMoleFrac);
					speciesMoleFrac.addPropertyValue(hasValue, speciesMoleFracValue);
					speciesMoleFracValue.setPropertyValue(numericalValue, getGasPhaseMoleComp().get(species));
				} catch(IllegalArgumentException l) {
					System.out.println("Duplicate Species Added");
				}
			}
		}
	}
	
	public void createSinglePhase(JenaOWLModel owlModel) {
		/**
		 * Precondition:
		 * owlModel is a valid JenaOWLModel object
		 * 
		 * Postcondition:
		 * added a single phase system into owlModel with its properties
		 * 
		 */
		OWLDatatypeProperty numericalValue = owlModel.getOWLDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
		OWLNamedClass scalarValue = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue");
		
		OWLObjectProperty hasValue = owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");
		OWLObjectProperty hasComposition=owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#has_composition");
		OWLNamedClass composition=owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#Composition");
		OWLNamedClass moleFrac=owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#MoleFraction");
		OWLObjectProperty comprisesDirectly=owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#comprisesDirectly");
		
		OWLIndividual singlePhaseSingle=owlModel.getOWLIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+ filename +"#Single_Phase_"+getName());
		
		if (getPhaseSystem().containsKey("Gas")) {
			for (String species : getGasPhaseMoleComp().keySet()) {
				RDFIndividual streamSpecies=composition.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+ filename +"#"+species+"_in_"+getName());
				RDFIndividual speciesMoleFrac=moleFrac.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+ filename +"#Mole_Fraction_Of_"+species+"_in_"+getName());
				RDFIndividual speciesMoleFracValue=scalarValue.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+ filename +"#Value_Of_Mole_Fraction_Of_"+species+"_in_"+getName());
				singlePhaseSingle.addPropertyValue(hasComposition, streamSpecies);
				streamSpecies.addPropertyValue(comprisesDirectly, speciesMoleFrac);
				speciesMoleFrac.addPropertyValue(hasValue, speciesMoleFracValue);
				speciesMoleFracValue.setPropertyValue(numericalValue, getGasPhaseMoleComp().get(species));
			}
		}
		else if (getPhaseSystem().containsKey("Liquid")) {
			for (String species : getLiqPhaseMoleComp().keySet()) {
				RDFIndividual streamSpecies=composition.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+ filename +"#"+species+"_in_"+getName());
				RDFIndividual speciesmolefrac=moleFrac.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+ filename +"#Mole_Fraction_Of_"+species+"_in_"+getName());
				RDFIndividual speciesmolefracvalue=scalarValue.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+ filename +"#Value_Of_Mole_Fraction_Of_"+species+"_in_"+getName());
				singlePhaseSingle.addPropertyValue(hasComposition, streamSpecies);
				streamSpecies.addPropertyValue(comprisesDirectly, speciesmolefrac);
				speciesmolefrac.addPropertyValue(hasValue, speciesmolefracvalue);
				speciesmolefracvalue.setPropertyValue(numericalValue, getLiqPhaseMoleComp().get(species));
			}
		}
		else if (getPhaseSystem().containsKey("Liquid 2")) {
			for (String species : getLiq2PhaseMoleComp().keySet()) {
				RDFIndividual streamSpecies=composition.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+ filename +"#"+species+"_in_"+getName());
				RDFIndividual speciesMoleFrac=moleFrac.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+ filename +"#Mole_Fraction_Of_"+species+"_in_"+getName());
				RDFIndividual speciesMoleFracValue=scalarValue.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+ filename +"#Value_Of_Mole_Fraction_Of_"+species+"_in_"+getName());
				singlePhaseSingle.addPropertyValue(hasComposition, streamSpecies);
				streamSpecies.addPropertyValue(comprisesDirectly, speciesMoleFrac);
				speciesMoleFrac.addPropertyValue(hasValue, speciesMoleFracValue);
				speciesMoleFracValue.setPropertyValue(numericalValue, getLiq2PhaseMoleComp().get(species));
			}
		}
		else if (getPhaseSystem().containsKey("Aqueous")) {
			for (String species : getAqPhaseMoleComp().keySet()) {
				RDFIndividual streamSpecies=composition.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+ filename +"#"+species+"_in_"+getName());
				RDFIndividual speciesMoleFrac=moleFrac.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+ filename +"#Mole_Fraction_Of_"+species+"_in_"+getName());
				RDFIndividual speciesMoleFracValue=scalarValue.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+ filename +"#Value_Of_Mole_Fraction_Of_"+species+"_in_"+getName());
				singlePhaseSingle.addPropertyValue(hasComposition, streamSpecies);
				streamSpecies.addPropertyValue(comprisesDirectly, speciesMoleFrac);
				speciesMoleFrac.addPropertyValue(hasValue, speciesMoleFracValue);
				speciesMoleFracValue.setPropertyValue(numericalValue, getAqPhaseMoleComp().get(species));
			}
		}
		else {}	
	}
	
	public void addChemicalSpeciesProperties(JenaOWLModel owlModel, RDFIndividual species, String component) {
		/**
		 * Helper function to add chemical species properties in intrinsic characteristics
		 * 
		 * Precondition:
		 * owlModel is a valid JenaOWLModel object
		 * species is a valid RDFIndividual and not a null
		 * 
		 * Postcondition:
		 * added the properties of the chemical species
		 * 
		 */
		OWLIndividual molecularMass=owlModel.getOWLIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/physical_dimension/derived_dimensions.owl#molecular_mass");
		OWLIndividual kg_per_kmol=owlModel.getOWLIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#kg_per_kmol");
		
		OWLNamedClass molecularWeight=owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#MolecularWeight");
		OWLNamedClass scalarValue = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue");
		OWLNamedClass molecularEntity=owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#MolecularEntity");
		
		OWLObjectProperty hasMolecularStructure=owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#hasMolecularStructure");
		OWLObjectProperty hasProperty=owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasProperty");
		OWLObjectProperty hasValue = owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");
		OWLObjectProperty hasDimension=owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasDimension");
		OWLObjectProperty hasUnitOfMeasure=owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");
		OWLObjectProperty hasMacroscopicAppearance=owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#hasMacroscopicAppearance");
		
		OWLDatatypeProperty numericalValue = owlModel.getOWLDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
		
		/* Add hasProperty molecular weight instance */
		RDFIndividual molecularWeightInstance;
		RDFIndividual molecularWeightCheck=owlModel.getOWLIndividual("http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#MolecularWeightOf"+component);
		if (molecularWeightCheck==null) {
			molecularWeightInstance=molecularWeight.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+"#MolecularWeightOf"+component);
		}
		else {
			molecularWeightInstance = molecularWeightCheck;
		}
		species.addPropertyValue(hasProperty, molecularWeightInstance);
		
		/* Add hasValue value of molecular weight */
		RDFIndividual valueMolecularWeight;
		RDFIndividual valuemolecularweightcheck=owlModel.getOWLIndividual("http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#ValueOfMolecularWeightOf"+component);
		if (valuemolecularweightcheck==null) {
			valueMolecularWeight=scalarValue.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+"#ValueOfMolecularWeightOf"+component);
		}
		else {
			valueMolecularWeight=owlModel.getRDFIndividual("http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#ValueOfMolecularWeightOf"+component);
		}
		molecularWeightInstance.addPropertyValue(hasValue, valueMolecularWeight);
		
		/* Add hasDimension to molecular weight*/
		molecularWeightInstance.addPropertyValue(hasDimension, molecularMass);
		
		/* Add numerical value and units to value of molecular weight*/
		valueMolecularWeight.setPropertyValue(numericalValue, componentToMolWeight(component));
		valueMolecularWeight.setPropertyValue(hasUnitOfMeasure, kg_per_kmol);
		
		/* Add hasMolecularStructure */
		RDFIndividual molecularStructure;
		RDFIndividual molecularStructureCheck=owlModel.getOWLIndividual("http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#"+componentToMolFormula(component));
		if (molecularStructureCheck==null) {
			molecularStructure = molecularEntity.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+"#"+componentToMolFormula(component));
			species.setPropertyValue(hasMolecularStructure, molecularStructure);
		}
		else {
			molecularStructure = molecularStructureCheck;
		}
		
		
		/* Add hasMacroscopicAppearance */
		molecularStructure.addPropertyValue(hasMacroscopicAppearance, species);
	}
	
	public void createIntrinsicCharacteristics(JenaOWLModel owlModel) {
		/**
		 * Precondition:
		 * owlModel is a valid JenaOWLModel object
		 * 
		 * Postcondition:
		 * added intrinsic characteristics into owlModel
		 * 
		 */
		OWLIndividual material=owlModel.getOWLIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+ filename + "#Material_" + getName());
		
		OWLNamedClass chemicalSpecies=owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#ChemicalSpecies");
		OWLNamedClass mixture=owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#Mixture");
		
		OWLObjectProperty intrinsicCharacteristics=owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/material/material.owl#intrinsicCharacteristics");
		OWLObjectProperty containsDirectly=owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#containsDirectly");
		
		if (isSingleComponent()) {
			String singleComponent = hysysNameToCompArray(getSingleComponent());
			
			/* Adding single species */
			RDFIndividual singleSpecies;
			RDFIndividual checkSpecies=owlModel.getOWLIndividual("http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#"+singleComponent);
			if (checkSpecies==null) {
				singleSpecies = chemicalSpecies.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+"#"+singleComponent);
			}
			else {
				singleSpecies = checkSpecies;
			}
			material.addPropertyValue(intrinsicCharacteristics, singleSpecies);
			
			/* Add properties to the species*/
			addChemicalSpeciesProperties(owlModel, singleSpecies, singleComponent);
		}
		else { // Multi Component
			RDFIndividual mixtureSpecies=mixture.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+"#Mixture_"+getName());
			material.addPropertyValue(intrinsicCharacteristics, mixtureSpecies);
			
			Set<String> allComponents = new HashSet<String>();
			Set<String> hysysNames = getAllComponents();
			for (String hysysName : hysysNames) {
				allComponents.add(hysysNameToCompArray(hysysName));
			}
			
			for (String component : allComponents) {
				/* Add containsDirectly to Mixture*/
				RDFIndividual species;
				RDFIndividual checkSpecies=owlModel.getRDFIndividual("http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#"+component);
				if (checkSpecies==null) {
					species = chemicalSpecies.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+"#"+component);
				}
				else {
					species = checkSpecies;
				}
				mixtureSpecies.addPropertyValue(containsDirectly, species);
				
				/* Add properties to species*/
				addChemicalSpeciesProperties(owlModel, species, component);
			}
		}
	}
	
	public void createOWLFile() {
		/**
		 * Precondition:
		 * Assumes that the template file exists at the specified location
		 * Assumes that the stream is not a 2-phase liquid-liquid or 3-phase liquid-liquid-vapour 
		 * 
		 * Postcondition:
		 * creates the OWL file for 1 stream/pipe instance
		 * 
		 */
		Tools.replaceString("_340002","_" + getName(), "OWL Templates/Stream_Template.owl", filename);
		Tools.replaceString("36.7447", Float.toString(getMolarFlow()), filename);
		Tools.replaceString("35.7447", Float.toString(getMassFlow()), filename);
		Tools.replaceString("30.7447", Float.toString(getTemperature()), filename);
		Tools.replaceString("40.7447", Float.toString(getPressure()), filename);
				
		if(getVapFrac()>0.999) {
			buildSinglePhaseSystem("Gas");
		}
		else if(getVapFrac()<1e-4) {
			Tools.replaceString("gaseous", "liquid", filename);
			if (!getAqPhaseMoleComp().isEmpty() && getLiqPhaseMoleComp().isEmpty() && getLiq2PhaseMoleComp().isEmpty()) {
				buildSinglePhaseSystem("Aqueous");
			}
			else if (!getLiqPhaseMoleComp().isEmpty() && getAqPhaseMoleComp().isEmpty() && getLiq2PhaseMoleComp().isEmpty()) {
				buildSinglePhaseSystem("Liquid");
			}
		}
		else {
			buildMultiPhaseSystem();
			Tools.replaceString("Single_Phase_", "Single_Phase_1_", filename);
			Tools.replaceString("Temperature_", "Temperature_1_", filename);
			Tools.replaceString("Pressure_", "Pressure_1_", filename);
			Tools.replaceString("Value_of_Temperature_", "Value_of_Temperature_1_", filename);
			Tools.replaceString("Value_of_Pressure_", "Value_of_Pressure_1_", filename);
		}
		/* Call Jena*/
		JenaOWLModel owlModel = Tools.callJena(filename);
		if (!isMultiPhase()) {
			createSinglePhase(owlModel);
		}
		else {
			createMultiPhase(owlModel);
		}
		createIntrinsicCharacteristics(owlModel);
		Tools.saveJena(owlModel, filename);
		/* Save Jena*/
		
	}
	
}
