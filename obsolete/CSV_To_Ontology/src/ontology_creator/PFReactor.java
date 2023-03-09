package ontology_creator;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.hp.hpl.jena.util.FileUtils;

import edu.stanford.smi.protege.exception.OntologyLoadException;
import edu.stanford.smi.protegex.owl.ProtegeOWL;
import edu.stanford.smi.protegex.owl.jena.JenaOWLModel;
import edu.stanford.smi.protegex.owl.model.OWLDatatypeProperty;
import edu.stanford.smi.protegex.owl.model.OWLIndividual;
import edu.stanford.smi.protegex.owl.model.OWLNamedClass;
import edu.stanford.smi.protegex.owl.model.OWLObjectProperty;
import edu.stanford.smi.protegex.owl.model.RDFIndividual;

public class PFReactor {
	private String name = "";
	private float temperature = Float.NaN;
	private float pressureDrop = Float.NaN;
	private Set<String> feedStream = new HashSet<String>();
	private String prodStream = "";
	private String utilFeed = "";
	private String utilProd = "";
	private String rxnSet = "";
	private Map<String,Reaction> chemRxns = new HashMap<String,Reaction>();
	private float duty = 0;
	private int n_tubes = 0;
	private float length = Float.NaN;
	private float diameter = Float.NaN;
	private int n_nozzle = 2;
	private String filename;
	
	PFReactor(String name) {
		this.name = name;
		this.filename = name + ".owl";
	}

	public String getName() {
		return name;
	}

	public float getTemperature() {
		return temperature;
	}

	public float getPressureDrop() {
		return pressureDrop;
	}

	public Set<String> getFeedStreams() {
		return feedStream;
	}

	public String getProdStream() {
		return prodStream;
	}
	
	public String getUtilFeed() {
		return utilFeed;
	}
	
	public String getUtilProd() {
		return utilProd;
	}

	public String getRxnSet() {
		return rxnSet;
	}

	public Map<String,Reaction> getChemRxns() {
		return chemRxns;
	}
	
	public float getDuty() {
		return duty;
	}
	
	public int getN_tubes() {
		return n_tubes;
	}
	
	public float getLength() {
		return length;
	}
	
	public float getDiameter() {
		return diameter;
	}

	public int getN_Nozzle() {
		return n_nozzle;
	}

	public void setName(String name) {
		this.name = name;
	}

	public void setTemperature(float temperature) {
		this.temperature = temperature;
	}

	public void setPressureDrop(float pressureDrop) {
		this.pressureDrop = pressureDrop;
	}

	public void setFeedStreams(Set<String> feedStreams) {
		this.feedStream = feedStreams;
	}
	
	public void addFeedStream(String feedStream) {
		this.feedStream.add(feedStream);
	}

	public void setProdStream(String prodStream) {
		this.prodStream = prodStream;
	}
	
	public void setUtilFeed(String utilFeed) {
		this.utilFeed = utilFeed;
	}
	
	public void setUtilProd(String utilProd) {
		this.utilProd = utilProd;
	}

	public void setRxnSet(String rxnSet) {
		this.rxnSet = rxnSet;
	}
	
	public void setChemRxns(Map<String,Reaction> chemRxns) {
		this.chemRxns = chemRxns;
	}

	public void addChemRxn(String chemRxn) {
		this.chemRxns.put(chemRxn, new Reaction(chemRxn));
	}

	public void addReactant(String chemRxn, String reactant) {
		if (!this.chemRxns.containsKey(chemRxn)) {
			addChemRxn(chemRxn);
		}
		Reaction rxn = this.chemRxns.get(chemRxn);
		rxn.addReactant(reactant);
		this.chemRxns.put(chemRxn, rxn);
	}
	
	public void addProduct(String chemRxn, String product) {
		if (!this.chemRxns.containsKey(chemRxn)) {
			addChemRxn(chemRxn);
		}
		Reaction rxn = this.chemRxns.get(chemRxn);
		rxn.addProduct(product);
		this.chemRxns.put(chemRxn, rxn);
	}

	public void setDuty(float duty) {
		this.duty = duty;
	}
	
	public void setN_tubes(int n_tubes) {
		this.n_tubes = n_tubes;
	}
	
	public void setLength(float length) {
		this.length = length;
	}
	
	public void setDiameter(float diameter) {
		this.diameter = diameter;
	}
	
	public void setN_Nozzle(int n_nozzle) {
		this.n_nozzle = n_nozzle;
	}

	public void addNozzleCount() {
		this.n_nozzle++;
	}

	public boolean isComplete() {
		boolean complete = true;
		if (getName().isEmpty() || getTemperature()==Float.NaN || getPressureDrop()==Float.NaN || getFeedStreams().isEmpty() || getProdStream().isEmpty() || getRxnSet().isEmpty() || getChemRxns().isEmpty()) {
			complete = false;
		}
		else {
			for (String chemRxn : getChemRxns().keySet()) {
				if (!getChemRxns().get(chemRxn).isComplete()) {
					complete = false;
				}
			}
		}
		return complete;
	}
	
	
	
	public void createUtility(JenaOWLModel owlModel) {
		/**
		 * Precondition:
		 * owlModel is a valid JenaOWLModel object
		 * 
		 * Postcondition:
		 * Add new nozzles & pipes to the owlModel
		 * 
		 */
		OWLNamedClass NozzleClass = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Nozzle");
		OWLNamedClass PipeClass = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Pipe");
		
		OWLObjectProperty hasConnector = owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#hasConnector");
		OWLObjectProperty isDirectlyConnectedTo = owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isDirectlyConnectedTo");
		
		RDFIndividual Reactor = owlModel.getRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#" + getName());
		
		addNozzleCount();
		RDFIndividual NewNozzle1 = NozzleClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#" + getName() + "_N_" + getN_Nozzle());
		addNozzleCount();
		RDFIndividual NewNozzle2 = NozzleClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#" + getName() + "_N_" + getN_Nozzle());
		
		RDFIndividual UtilFeedPipe = PipeClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#Pipe_" + getUtilFeed());
		RDFIndividual UtilProdPipe = PipeClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#Pipe_" + getUtilProd());
		
		Reactor.addPropertyValue(hasConnector, NewNozzle1);
		Reactor.addPropertyValue(hasConnector, NewNozzle2);
		NewNozzle1.addPropertyValue(isDirectlyConnectedTo, UtilFeedPipe);
		NewNozzle2.addPropertyValue(isDirectlyConnectedTo, UtilProdPipe);
		
		/* Create new utility streams */
		RDFIndividual ReactionSet = owlModel.getRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#Reaction_" + getRxnSet());
		
		OWLNamedClass UtilityClass = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#Utility");
		
		OWLObjectProperty hasInput = owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#hasInput");
		OWLObjectProperty hasOutput = owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#hasOutput");
		
		RDFIndividual UtilityFeed = UtilityClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#UtilityStream_" + getUtilFeed());
		RDFIndividual UtilityProduct = UtilityClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#UtilityStream_" + getUtilProd());
		
		ReactionSet.addPropertyValue(hasInput, UtilityFeed);
		ReactionSet.addPropertyValue(hasOutput, UtilityProduct);
	}

	public void createTubes(JenaOWLModel owlModel) {
		/**
		 * Precondition:
		 * Assumes owlModel is a valid JenaOWLModel object
		 * 
		 * Postcondition:
		 * Added Tube Bundle and its properties to the owlModel
		 * 
		 */
		OWLNamedClass TubeBundleClass = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant_equipment/fixture.owl#TubeBundle");
		OWLNamedClass ScalarValueClass = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue");
		OWLNamedClass EdgeLengthClass = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#EdgeLength");
		OWLNamedClass InsideDiameterClass = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#InsideDiameter");
		
		OWLObjectProperty contains = owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#contains");
		OWLObjectProperty hasNumberOfTubes = owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant_equipment/fixture.owl#hasNumberOfTubes");
		OWLObjectProperty has_length = owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#has_length");
		OWLObjectProperty hasInsideDiameter = owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#hasInsideDiameter");
		OWLObjectProperty hasUnitOfMeasure = owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");
		OWLDatatypeProperty numericalValue = owlModel.getOWLDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
		
		RDFIndividual PFR = owlModel.getRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#" + getName());
		RDFIndividual TubeBundle = TubeBundleClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#TubeBundle_" + getName());
		RDFIndividual NumberOfTubes = ScalarValueClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#NumberOfTubes_" + getName());
		RDFIndividual EdgeLength = EdgeLengthClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#EdgeLength_" + getName());
		RDFIndividual InsideDiameter = InsideDiameterClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName()+ ".owl#InsideDiameter_" + getName());
		RDFIndividual meter = owlModel.getRDFIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#m");
		
		PFR.addPropertyValue(contains, TubeBundle);
		TubeBundle.addPropertyValue(hasNumberOfTubes, NumberOfTubes);
		NumberOfTubes.addPropertyValue(numericalValue, getN_tubes());
		TubeBundle.addPropertyValue(has_length, EdgeLength);
		EdgeLength.addPropertyValue(numericalValue, getLength());
		EdgeLength.addPropertyValue(hasUnitOfMeasure, meter);
		TubeBundle.addPropertyValue(hasInsideDiameter, InsideDiameter);
		InsideDiameter.addPropertyValue(numericalValue, getDiameter());
		InsideDiameter.addPropertyValue(hasUnitOfMeasure, meter);
	}

	public void createReaction(JenaOWLModel owlModel) {
		try {
			/**
			 * Precondition:
			 * Assumes that owlModel is a valid JenaOWLModel object
			 * 
			 * Postcondition:
			 * added ReactionNetwork, ChemicalReaction, Reactants, and Products to the owlModel
			 * 
			 */
			OWLNamedClass ChemicalReactionClass = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#ChemicalReaction");
			OWLNamedClass ChemicalSpeciesClass = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#ChemicalSpecies");

			OWLIndividual ReactionNetwork = owlModel.getOWLIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#ReactionNetwork_" + getRxnSet());

			OWLObjectProperty hasDirectSubsystem = owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasDirectSubsystem");
			OWLObjectProperty hasReactant = owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#hasReactant");
			OWLObjectProperty hasProduct = owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#hasProduct");
			
			for (String rxnName : getChemRxns().keySet()) {
				Reaction rxn = getChemRxns().get(rxnName);
				RDFIndividual ChemicalReaction = ChemicalReactionClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#" + "ChemicalReaction_" + rxnName);
				ReactionNetwork.addPropertyValue(hasDirectSubsystem, ChemicalReaction);
				
				for (String reactant : rxn.getReactants()) {
					RDFIndividual ChemicalSpecies = owlModel.getOWLIndividual("http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#" + reactant);
					try {
						ChemicalSpecies.getName();
					} catch (NullPointerException e) {
						ChemicalSpecies = ChemicalSpeciesClass.createRDFIndividual("http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#" + reactant);
					}
					ChemicalReaction.addPropertyValue(hasReactant, ChemicalSpecies);
				}
				
				for (String product : rxn.getProducts()) {
					RDFIndividual ChemicalSpecies = owlModel.getOWLIndividual("http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#" + product);
					try {
						ChemicalSpecies.getName();
					} catch (NullPointerException e) {
						ChemicalSpecies = ChemicalSpeciesClass.createRDFIndividual("http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#" + product);
					}
					ChemicalReaction.addPropertyValue(hasProduct, ChemicalSpecies);
				}
			}
		} catch (NullPointerException e) {
			e.printStackTrace();
			System.out.println("Make sure that the Reaction Set appear in the table before the reactions, and reactions before reactants.");
		}
	}
	
	public void createOWLFile() {
		/**
		 * Precondition:
		 * Assumes that the template file exists at the specified location
		 * Assumes that the reactor only has 1 inlet
		 * 
		 * Postcondition:
		 * creates the OWL file for 1 reactor instance
		 * 
		 */
		Tools.replaceString("R-340001", getName(), "OWL Templates/R-340001_Template_PF.owl", filename);
		Tools.replaceString("Reaction_SBRProduction", "Reaction_" + getRxnSet(), filename);
		Tools.replaceString("ReactionNetwork_SBRProduction", "ReactionNetwork_" + getRxnSet(), filename);
		
		Tools.replaceString("ProcessStream_400015", "ProcessStream_" + getProdStream(), filename);
		Tools.replaceString("Pipe_400015", "Pipe_" + getProdStream(), filename);
		
		Tools.replaceString("1.1234567", Float.toString(getPressureDrop()), filename);
		Tools.replaceString("0.1234567", Float.toString(getDuty()), filename);
		
		String feedStream = getFeedStreams().iterator().next();
		Tools.replaceString("ProcessStream_400012", "ProcessStream_" + feedStream, filename);
		Tools.replaceString("Pipe_400012", "Pipe_" + feedStream, filename);
		if (getFeedStreams().size() > 1) {
			Set feedStreamsToBeMade = getFeedStreams();
			feedStreamsToBeMade.remove(feedStream);
		}
		
		JenaOWLModel owlModel = Tools.callJena(filename);
		createTubes(owlModel);
		createReaction(owlModel);
		// TODO add more feed streams
		if (getDuty() != 0) { // If reactor is not adiabatic (duty is not 0), add utility streams
			createUtility(owlModel);
		}
		Tools.createIRI(owlModel);
		Tools.saveJena(owlModel, filename);
		
	}

}
