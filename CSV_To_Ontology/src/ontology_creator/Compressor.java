package ontology_creator;

import edu.stanford.smi.protegex.owl.jena.JenaOWLModel;
import edu.stanford.smi.protegex.owl.model.OWLDatatypeProperty;
import edu.stanford.smi.protegex.owl.model.OWLIndividual;
import ontology_creator.Plant;

public class Compressor {
	private String name;
	private String feedStream;
	private String prodStream;
	private float power = Float.NaN;
	private float outPressure = Float.NaN;
	private String filename;
	
	Compressor(String name) {
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
	public void setPower(float power) {
		this.power = power;
	}
	public void setOutPressure(float outPressure) {
		this.outPressure = outPressure;
	}
	
	public void createOWLFile(String plantName) {
		/**
		 * Precondition:
		 * Assumes that the template file exists at the specified location
		 * 
		 * Postcondition:
		 * creates the OWL file for 1 compressor instance
		 * 
		 */
		Tools.replaceString("C-340001", getName(), "OWL Templates/C-340001_Template.owl", filename);
		Tools.replaceString("_340007", "_" + getFeedStream(), filename);
		Tools.replaceString("_340008", "_" + getProdStream(), filename);
		String subdirofplantname= plantName.toLowerCase();
		
		JenaOWLModel owlmodel = Tools.callJena(filename);

		OWLIndividual Power = owlmodel.getOWLIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#ValueOfHeatDutyOf" + getName());
		OWLIndividual Pressure = owlmodel.getOWLIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + getName() + ".owl#ValueOfOutletPressureOf" + getName());

		OWLDatatypeProperty numericalvalue = owlmodel.getOWLDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
		
		Power.setPropertyValue(numericalvalue, getPower());
		Pressure.setPropertyValue(numericalvalue, getOutPressure());
		
		Tools.createIRI(owlmodel);
		Tools.saveJena(owlmodel, filename);
	}
}
