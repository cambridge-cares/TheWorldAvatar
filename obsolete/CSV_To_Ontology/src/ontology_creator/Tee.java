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

public class Tee {
	private String name;
	private List<String> prodStreams = new ArrayList<String>();
	private String feedStream;
	private String filename;
	
	Tee(String name) {
		this.name = name;
		this.filename = name + ".owl";
	}

	public String getName() {
		return name;
	}

	public List<String> getProdStreams() {
		return prodStreams;
	}

	public String getFeedStream() {
		return feedStream;
	}

	public void setName(String name) {
		this.name = name;
	}

	public void setProdStreams(List<String> prodStreams) {
		this.prodStreams = prodStreams;
	}
	
	public void addProdStream(String prodStream) {
		this.prodStreams.add(prodStream);
	}

	public void setFeedStream(String feedStream) {
		this.feedStream = feedStream;
	}

	public void createOWLFile() {
		/**
		 * Precondition:
		 * Assumes that the template file exists at the specified location
		 * 
		 * Postcondition:
		 * creates the OWL file for 1 tee instance
		 * 
		 */
		Tools.replaceString("TEE-340001", getName(), "OWL Templates/TEE-340001_Template.owl", filename);
		Tools.replaceString("_340001", "_" + getFeedStream(), filename);
		Tools.replaceString("_340002", "_" + getProdStreams().get(0), filename);
		Tools.replaceString("_340003", "_" + getProdStreams().get(1), filename);
		JenaOWLModel owlModel = Tools.callJena(filename);
		if (getProdStreams().size()>2) {
			OWLNamedClass NozzleClass = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Nozzle");
			OWLNamedClass ProcessStreamClass = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#ProcessStream");
			OWLNamedClass PipeClass = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Pipe");

			OWLIndividual MixingProcess = owlModel.getOWLIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#Mixing1");
			OWLIndividual MixerInstance = owlModel.getOWLIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#" + getName());

			OWLObjectProperty hasConnector = owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#hasConnector");
			OWLObjectProperty hasOutput = owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#hasOutput");
			OWLObjectProperty isDirectlyConnectedTo = owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isDirectlyConnectedTo");
			
			for (int index=2; index<getProdStreams().size(); index++) {
				RDFIndividual Nozzle = NozzleClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#" + getName() + "_N_" + Integer.toString(index + 1));
				RDFIndividual ProcessStream = ProcessStreamClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#ProcessStream" + getProdStreams().get(index));
				RDFIndividual Pipe = PipeClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#Pipe_" + getProdStreams().get(index));
				
				MixerInstance.addPropertyValue(hasConnector, Nozzle);
				MixingProcess.addPropertyValue(hasOutput, ProcessStream);
				Pipe.addPropertyValue(isDirectlyConnectedTo, Nozzle);
				Nozzle.addPropertyValue(isDirectlyConnectedTo, Pipe);
			}
		}
		Tools.createIRI(owlModel);
		Tools.saveJena(owlModel, filename);
	}
	
}
