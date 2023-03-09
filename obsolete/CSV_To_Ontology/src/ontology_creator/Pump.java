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

public class Pump {
	private String name;
	private String feedStream;
	private String prodStream;
	private float pumpHead = Float.NaN;
	private float power = Float.NaN;
	private float outPressure = Float.NaN;
	private String filename;
	
	Pump(String name) {
		this.name = name;
		this.filename = name + ".owl";
	}
	
	public String getName() {
		return name;
	}
	public String getFeedStream() {
		return feedStream;
	}
	public String getProdStream() {
		return prodStream;
	}
	public float getPumpHead() {
		return pumpHead;
	}
	public float getPower() {
		return power;
	}
	public float getOutPressure() {
		return outPressure;
	}
	public void setName(String name) {
		this.name = name;
	}
	public void setFeedStream(String feedStream) {
		this.feedStream = feedStream;
	}
	public void setProdStream(String prodStream) {
		this.prodStream = prodStream;
	}
	public void setPumpHead(float pumpHead) {
		this.pumpHead = pumpHead;
	}
	public void setPower(float power) {
		this.power = power;
	}
	public void setOutPressure(float outPressure) {
		this.outPressure = outPressure;
	}
	
	public void createOWLFile() {
		/**
		 * Precondition:
		 * Assumes that the template file exists at the specified location
		 * 
		 * Postcondition:
		 * creates the OWL file for 1 pump instance
		 * 
		 */
		Tools.replaceString("P-340001", getName(), "OWL Templates/P-340001_Template.owl", filename);
		Tools.replaceString("_12345", "_" + getFeedStream(), filename);
		Tools.replaceString("_340004", "_" + getProdStream(), filename);
		
		JenaOWLModel owlModel = Tools.callJena(filename);
		
		OWLIndividual PumpHead = owlModel.getOWLIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#ValueOfPumpHeadOf" + getName());
		OWLIndividual Power = owlModel.getOWLIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#ValueOfHeatDutyOf" + getName());
		OWLIndividual Pressure = owlModel.getOWLIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#ValueOfOutletPressureOf" + getName());
		
		OWLDatatypeProperty numericalvalue = owlModel.getOWLDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
		
		PumpHead.setPropertyValue(numericalvalue, getPumpHead());
		Power.setPropertyValue(numericalvalue, getPower());
		Pressure.setPropertyValue(numericalvalue, getOutPressure());
		
		Tools.createIRI(owlModel);
		Tools.saveJena(owlModel, filename);
	}
	
}
