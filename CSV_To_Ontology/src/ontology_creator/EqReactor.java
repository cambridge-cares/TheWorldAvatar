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
import edu.stanford.smi.protegex.owl.model.OWLIndividual;
import edu.stanford.smi.protegex.owl.model.OWLNamedClass;
import edu.stanford.smi.protegex.owl.model.OWLObjectProperty;
import edu.stanford.smi.protegex.owl.model.RDFIndividual;

public class EqReactor {
	private String name = "";
	private float temperature = Float.NaN;
	private float pressure = Float.NaN;
	private float pressureDrop = Float.NaN;
	private Set<String> feedStream = new HashSet<String>();
	private String vapProd = "";
	private String liqProd = "";
	private String rxnSet = "";
	private String utilFeed = "";
	private String utilProd = "";
	private Map<String,Reaction> chemRxns = new HashMap<String,Reaction>();
	private float duty = 0;
	private int n_nozzle = 3;
	private String filename;
	
	EqReactor(String name) {
		this.name = name;
		this.filename = name + ".owl";
	}

	public String getName() {
		return name;
	}

	public float getTemperature() {
		return temperature;
	}
	
	public float getPressure() {
		return pressure;
	}

	public float getPressureDrop() {
		return pressureDrop;
	}

	public Set<String> getFeedStreams() {
		return feedStream;
	}

	public String getVapProd() {
		return vapProd;
	}

	public String getLiqProd() {
		return liqProd;
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
	
	public int getN_Nozzle() {
		return n_nozzle;
	}

	public void setName(String name) {
		this.name = name;
	}

	public void setTemperature(float temperature) {
		this.temperature = temperature;
	}
	
	public void setPressure(float pressure) {
		this.pressure = pressure;
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

	public void setVapProd(String vapProd) {
		this.vapProd = vapProd;
	}

	public void setLiqProd(String liqProd) {
		this.liqProd = liqProd;
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
	
	public void setN_Nozzle(int n_nozzle) {
		this.n_nozzle = n_nozzle;
	}
	
	public void addNozzleCount() {
		this.n_nozzle++;
	}
	
	public void decNozzleCount() {
		this.n_nozzle--;
	}
	
	public boolean isComplete() {
		boolean complete = true;
		if (getName().isEmpty() || getTemperature()==Float.NaN || getPressureDrop()==Float.NaN || getFeedStreams().isEmpty() || getVapProd().isEmpty() || getLiqProd().isEmpty() || getRxnSet().isEmpty() || getChemRxns().isEmpty()) {
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
	
	public void deleteSecondOutletStream(JenaOWLModel owlModel) {
		OWLIndividual nozzle3 = owlModel.getOWLIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#" + getName() + "_N_3");
		OWLIndividual pipeOut2 = owlModel.getOWLIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#Pipe_out_2");
		OWLIndividual streamOut2 = owlModel.getOWLIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#ProcessStream_out_2");
		
		nozzle3.delete();
		pipeOut2.delete();
		streamOut2.delete();
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
		
		RDFIndividual reactor = owlModel.getRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#" + getName());
		
		addNozzleCount();
		RDFIndividual newNozzle1 = NozzleClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#" + getName() + "_N_" + getN_Nozzle());
		addNozzleCount();
		RDFIndividual newNozzle2 = NozzleClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#" + getName() + "_N_" + getN_Nozzle());
		
		RDFIndividual utilFeedPipe = PipeClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#Pipe_" + getUtilFeed());
		RDFIndividual utilProdPipe = PipeClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#Pipe_" + getUtilProd());
		
		reactor.addPropertyValue(hasConnector, newNozzle1);
		reactor.addPropertyValue(hasConnector, newNozzle2);
		newNozzle1.addPropertyValue(isDirectlyConnectedTo, utilFeedPipe);
		newNozzle2.addPropertyValue(isDirectlyConnectedTo, utilProdPipe);
		
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
	
	public void createReaction(JenaOWLModel owlModel) {
		/**
		 * Precondition:
		 * Assumes that owlModel is a valid JenaOWLModel object
		 * 
		 * Postcondition:
		 * added ReactionNetwork, ChemicalReaction, Reactants, and Products to the owlModel
		 * 
		 */
		try {
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
		Tools.replaceString("R-340001", getName(), "OWL Templates/R-340001_Template_Eq.owl", filename);
		Tools.replaceString("Reaction_SBRProduction", "Reaction_" + getRxnSet(), filename);
		Tools.replaceString("ReactionNetwork_SBRProduction", "ReactionNetwork_" + getRxnSet(), filename);
		
		if (!getLiqProd().contains("x") && !getVapProd().contains("x")) {
			Tools.replaceString("ProcessStream_400015", "ProcessStream_" + getLiqProd(), filename);
			Tools.replaceString("Pipe_400015", "Pipe_" + getLiqProd(), filename);
			Tools.replaceString("ProcessStream_out_2", "ProcessStream_" + getVapProd(), filename);
			Tools.replaceString("Pipe_out_2", "Pipe_" + getVapProd(), filename);
		}
		else if (!getLiqProd().contains("x")) {
			Tools.replaceString("ProcessStream_400015", "ProcessStream_" + getLiqProd(), filename);
			Tools.replaceString("Pipe_400015", "Pipe_" + getLiqProd(), filename);
			decNozzleCount();
		}
		else if (!getVapProd().contains("x")) {
			Tools.replaceString("ProcessStream_400015", "ProcessStream_" + getVapProd(), filename);
			Tools.replaceString("Pipe_400015", "Pipe_" + getVapProd(), filename);
			decNozzleCount();
		}
		
		String feedStream = getFeedStreams().iterator().next();
		Tools.replaceString("ProcessStream_400012", "ProcessStream_" + feedStream, filename);
		Tools.replaceString("Pipe_400012", "Pipe_" + feedStream, filename);
		
		if (getFeedStreams().size() > 1) {
			Set feedStreams = getFeedStreams();
			feedStreams.remove(feedStream);
			// TODO API to add more inlets
		}
		
		Tools.replaceString("5.1234567", Float.toString(getTemperature()), filename);
		Tools.replaceString("13243546", Float.toString(getPressure()), filename);
		Tools.replaceString("6.1234567", Float.toString(getPressureDrop()), filename);
		Tools.replaceString("2.1234567", Float.toString(getDuty()), filename);
		
		JenaOWLModel owlModel = Tools.callJena(filename);
		if (getN_Nozzle()<3) { // If reactor has a dummy outlet stream, delete dummy stream
			deleteSecondOutletStream(owlModel);
		}
		// TODO add more feed streams
		createReaction(owlModel);
		if (getDuty() != 0) { // If reactor is not adiabatic (duty is not 0), add utility streams
			createUtility(owlModel);
		}
		Tools.createIRI(owlModel);
		Tools.saveJena(owlModel, filename);
	}
}
