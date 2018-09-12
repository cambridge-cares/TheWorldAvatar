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
import java.util.Map;

import com.hp.hpl.jena.util.FileUtils;

import edu.stanford.smi.protege.exception.OntologyLoadException;
import edu.stanford.smi.protegex.owl.ProtegeOWL;
import edu.stanford.smi.protegex.owl.jena.JenaOWLModel;
import edu.stanford.smi.protegex.owl.model.OWLDatatypeProperty;
import edu.stanford.smi.protegex.owl.model.OWLNamedClass;
import edu.stanford.smi.protegex.owl.model.OWLObjectProperty;
import edu.stanford.smi.protegex.owl.model.RDFIndividual;

public class TrayColumn {
	private String name;
	private String columnType;
	private String separationName = "Sprtn";
	private int n_trays = -1;
	private int stageNumberingMethod;
	private Map<Integer,String> inletDrawTypes = new HashMap<Integer,String>();
	private Map<Integer,String> inletDrawNames = new HashMap<Integer,String>();
	private Map<Integer,TrayStage> trays = new HashMap<Integer,TrayStage>();
	private String distFeed;
	private String userDefFeed = "";
	private String topIn;
	private String topOut;
	private String btmIn;
	private String btmOut;
	private int n_nozzle = 4;
	private String filename;
	
	TrayColumn(String name) {
		this.name = name;
		this.filename = name + ".owl";
	}
	public String getName() {
		return name;
	}
	public String getColumnType() {
		return columnType;
	}
	public String getSeparationName() {
		return separationName;
	}
	public int getN_trays() {
		return n_trays;
	}
	public int getStageNumberingMethod() {
		return stageNumberingMethod;
	}
	public Map<Integer, String> getInletDrawTypes() {
		return inletDrawTypes;
	}
	public Map<Integer, String> getInletDrawNames() {
		return inletDrawNames;
	}
	public Map<Integer, TrayStage> getTrays() {
		return trays;
	}
	public TrayStage getTray(int trayNo) {
		if (trays.containsKey(trayNo)) {
			return trays.get(trayNo);
		}
		else {
			addNewTray(trayNo);
			return trays.get(trayNo);
		}
	}
	public String getDistFeed() {
		return distFeed;
	}
	public String getUserDefFeed() {
		return userDefFeed;
	}
	public String getTopIn() {
		return topIn;
	}
	public String getTopOut() {
		return topOut;
	}
	public String getBtmIn() {
		return btmIn;
	}
	public String getBtmOut() {
		return btmOut;
	}
	public int getN_nozzle() {
		return n_nozzle;
	}
	public void setName(String name) {
		this.name = name;
	}
	public void setColumnType(String columnType) {
		this.columnType = columnType;
	}
	public void setSeparationName(String separationName) {
		this.separationName = separationName;
	}
	public void setN_trays(int n_trays) {
		this.n_trays = n_trays;
	}
	public void setStageNumberingMethod(int stageNumberingMethod) {
		this.stageNumberingMethod = stageNumberingMethod;
	}
	public void setDistFeed(String feedStream) {
		this.distFeed = feedStream;
	}
	public void setUserDefFeed(String userDefFeed) {
		this.userDefFeed = userDefFeed;
	}
	public void addInletDrawTypes(int trayNo, String type) {
		inletDrawTypes.put(trayNo, type);
	}
	public void addInletDrawNames(int trayNo, String stream) {
		inletDrawNames.put(trayNo, stream);
	}
	public void addNewTray(int trayNo) {
		trays.put(trayNo, new TrayStage(this, trayNo));
	}
	public void addTray(int trayNo, TrayStage tray) {
		trays.put(trayNo, tray);
	}
	public void addFeedStream(int trayNo, String feedStream) {
		if (!feedStream.equals(btmIn) && !feedStream.equals(topIn)) {
			TrayStage tray = getTray(trayNo);
			tray.addFeedStream(feedStream);
			trays.put(trayNo, tray);
		}
	}
	public void addDrawStream(int trayNo, String drawStream) {
		if (!drawStream.equals(btmOut) && !drawStream.equals(topOut)) {
			TrayStage tray = getTray(trayNo);
			tray.addDrawStream(drawStream);
			trays.put(trayNo, tray);
		}
	}
	public void setTopIn(String topIn) {
		this.topIn = topIn;
	}
	public void setTopOut(String topOut) {
		this.topOut = topOut;
	}
	public void setBtmIn(String btmIn) {
		this.btmIn = btmIn;
	}
	public void setBtmOut(String btmOut) {
		this.btmOut = btmOut;
	}
	public void setN_nozzle(int n_nozzle) {
		this.n_nozzle = n_nozzle;
	}
	public void addN_nozzle() {
		this.n_nozzle++;
	}
	
	public boolean isComplete () {
		boolean complete = true;
		if (getName().isEmpty() || getColumnType().isEmpty() || getN_trays()==-1 || getTopIn().isEmpty() || getTopOut().isEmpty() || getBtmIn().isEmpty() || getBtmOut().isEmpty()) {
			complete = false;
		}
		return complete;
	}
	
	public void createSideFeedsDraws(JenaOWLModel owlModel) {
		for (int trayNo : trays.keySet()) {
			TrayStage tray = trays.get(trayNo);
			for (String feedStream : tray.getFeedStreams()) {
				setDistFeed(feedStream);
				// Create Process Stream and adds it to the separation process
				OWLNamedClass ProcessStreamClass = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#ProcessStream");

				OWLObjectProperty hasInput = owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#hasInput");

				RDFIndividual SeparationProcess = owlModel.getRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#" + getSeparationName());
				RDFIndividual ProcessStream = ProcessStreamClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#ProcessStream_" + feedStream);
				
				SeparationProcess.addPropertyValue(hasInput, ProcessStream);
				
				// Create a new Nozzle and adds it to the Column
				addN_nozzle();
				OWLNamedClass NozzleClass = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Nozzle");

				OWLObjectProperty hasConnector = owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#hasConnector");
				OWLDatatypeProperty is_at_numbered_tray = owlModel.getOWLDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#is_at_numbered_tray");

				RDFIndividual column = owlModel.getRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#" + getName() );
				RDFIndividual nozzle = NozzleClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName()  + ".owl#"+ getName()  + "_N_" + Integer.toString(getN_nozzle()));

				column.addPropertyValue(hasConnector, nozzle);
				nozzle.addPropertyValue(is_at_numbered_tray, trayNo);

				// Create a new Pipe and connects it to the Nozzle
				OWLNamedClass PipeClass = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Pipe");

				OWLObjectProperty isDirectlyConnectedTo = owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isDirectlyConnectedTo");

				RDFIndividual Pipe = PipeClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#Pipe_" + feedStream);

				Pipe.addPropertyValue(isDirectlyConnectedTo, nozzle);
				nozzle.addPropertyValue(isDirectlyConnectedTo, Pipe);
			}
			for (String drawStream : tray.getDrawStreams()) {
				// Create Process Stream and adds it to the Distilling process
				OWLNamedClass ProcessStreamClass = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#ProcessStream");

				OWLObjectProperty hasOutput = owlModel.getOWLObjectProperty("http://www.theworldavatar.com/OntoCAPE/meta_model/topology/topology.owl#hasOutput");

				RDFIndividual SeparationProcess = owlModel.getRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName()+ ".owl#" + getSeparationName());
				RDFIndividual ProcessStream = ProcessStreamClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName()+ ".owl#ProcessStream_" + drawStream);

				SeparationProcess.addPropertyValue(hasOutput, ProcessStream);

				// Create a new connector and adds it to the Column
				addN_nozzle();
				OWLNamedClass NozzleClass = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Nozzle");

				OWLObjectProperty hasConnector = owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#hasConnector");
				OWLDatatypeProperty is_at_numbered_tray = owlModel.getOWLDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#is_at_numbered_tray");
				
				RDFIndividual column = owlModel.getRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName()+ ".owl#" + getName());
				RDFIndividual nozzle = NozzleClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#"+ getName() + "_N_" + Integer.toString(getN_nozzle()));

				column.addPropertyValue(hasConnector, nozzle);
				nozzle.addPropertyValue(is_at_numbered_tray, trayNo);
				
				// Create a new Pipe and connects it to the Nozzle
				OWLNamedClass PipeClass = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Pipe");

				OWLObjectProperty isDirectlyConnectedTo = owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isDirectlyConnectedTo");

				RDFIndividual Pipe = PipeClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName()+ ".owl#Pipe_" + drawStream);

				Pipe.addPropertyValue(isDirectlyConnectedTo, nozzle);
				nozzle.addPropertyValue(isDirectlyConnectedTo, Pipe);
			}
		}
	}
	
	public void createOWLFile() {
		Tools.replaceString("T-300000", getName(), "OWL Templates/T-300000_Template.owl", filename);
		Tools.replaceString("20202020", Integer.toString(getN_trays()), filename);
		Tools.replaceString("TopIn", getTopIn(), filename);
		Tools.replaceString("TopOut", getTopOut(), filename);
		Tools.replaceString("BtmIn", getBtmIn(), filename);
		Tools.replaceString("BtmOut", getBtmOut(), filename);
		
		JenaOWLModel owlModel = Tools.callJena(filename);
		createSideFeedsDraws(owlModel);
		Tools.createIRI(owlModel);
		Tools.saveJena(owlModel, filename);
		
		if (getColumnType().equals("Distillation")) {
			Tools.replaceString(getSeparationName(), "Distilling_ProcessStream_" + getDistFeed(), filename);
			setSeparationName("Distilling_ProcessStream_" + getDistFeed());
		}
		if (getColumnType().equals("Absorber")) {
			Tools.replaceString("Separation", "Extracting", filename);
			Tools.replaceString("Sprtn", "Absorbing_ProcessStream_" + getBtmIn(), filename);
		}
		if (getColumnType().equals("Stripping")) {
			Tools.replaceString("Separation", "Extracting", filename);
			Tools.replaceString("Sprtn", "Stripping_ProcessStream_" + getTopIn(), filename);
		}
		if (getColumnType().equals("Liquid-Liquid Extractor")) {
			Tools.replaceString("Separation", "Extracting", filename);
			Tools.replaceString("Sprtn", "Extracting_ProcessStream_" + getUserDefFeed(), filename);
		}
		if (getStageNumberingMethod()==0) {
			Tools.replaceString("top-down", "bottom-up", filename);
		}
	}
}
