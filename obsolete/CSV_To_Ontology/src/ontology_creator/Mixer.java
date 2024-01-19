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

public class Mixer {
	private String name;
	private List<String> feedStreams = new ArrayList<String>();
	private String prodStream;
	private String filename;
	
	Mixer(String name) {
		this.name = name;
		this.filename = name + ".owl";
	}

	public String getName() {
		return name;
	}

	public List<String> getFeedStreams() {
		return feedStreams;
	}

	public String getProdStream() {
		return prodStream;
	}

	public void setName(String name) {
		this.name = name;
	}

	public void setFeedStreams(List<String> feedStreams) {
		this.feedStreams = feedStreams;
	}
	
	public void addFeedStream(String feedStream) {
		this.feedStreams.add(feedStream);
	}

	public void setProdStream(String prodStream) {
		this.prodStream = prodStream;
	}
	
	public void createOWLFile() {
		/**
		 * Precondition:
		 * Assumes that the template file exists at the specified location
		 * 
		 * Postcondition:
		 * creates the OWL file for 1 mixer instance
		 * 
		 */
		Tools.replaceString("MIX-340001", getName(), "OWL Templates/MIX-340001_Template.owl", filename);
		Tools.replaceString("_340003", "_" + getProdStream(), filename);
		Tools.replaceString("_340001", "_" + getFeedStreams().get(0), filename);
		Tools.replaceString("_340002", "_" + getFeedStreams().get(1), filename);
		JenaOWLModel owlmodel = Tools.callJena(filename);
		if (getFeedStreams().size()>2) {
			OWLNamedClass NozzleClass = owlmodel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Nozzle");
			OWLNamedClass ProcessStreamClass = owlmodel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#ProcessStream");
			OWLNamedClass PipeClass = owlmodel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Pipe");

			OWLIndividual MixingProcess = owlmodel.getOWLIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#Mixing1");
			OWLIndividual MixerInstance = owlmodel.getOWLIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#" + getName());

			OWLObjectProperty hasConnector = owlmodel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#hasConnector");
			OWLObjectProperty hasInput = owlmodel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#hasInput");
			OWLObjectProperty isDirectlyConnectedTo = owlmodel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isDirectlyConnectedTo");
			
			for (int index=2; index<getFeedStreams().size(); index++) {
				RDFIndividual Nozzle = NozzleClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#" + getName() + "_N_" + Integer.toString(index + 2));
				RDFIndividual ProcessStream = ProcessStreamClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#ProcessStream" + getFeedStreams().get(index));
				RDFIndividual Pipe = PipeClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#Pipe_" + getFeedStreams().get(index));
				
				MixerInstance.addPropertyValue(hasConnector, Nozzle);
				MixingProcess.addPropertyValue(hasInput, ProcessStream);
				Pipe.addPropertyValue(isDirectlyConnectedTo, Nozzle);
				Nozzle.addPropertyValue(isDirectlyConnectedTo, Pipe);
			}
		}
		Tools.createIRI(owlmodel);
		Tools.saveJena(owlmodel, filename);
	}
	
}
