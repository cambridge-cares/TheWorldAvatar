package ontology_creator;

import edu.stanford.smi.protegex.owl.jena.JenaOWLModel;

public class Valve {
	private String name;
	private String feedStream;
	private String prodStream;
	private float pressDrop;
	private float outPressure;
	private String filename;
	
	Valve(String name) {
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

	public float getPressDrop() {
		return pressDrop;
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

	public void setPressDrop(float pressDrop) {
		this.pressDrop = pressDrop;
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
		 * creates the OWL file for 1 valve instance
		 * 
		 */
		Tools.replaceString("VLV-340001", getName(), "OWL Templates/VLV-340001_Template.owl", filename);
		Tools.replaceString("ProcessStream_in", "ProcessStream_" + getFeedStream(), filename);
		Tools.replaceString("Pipe_in", "Pipe_" + getFeedStream(), filename);
		Tools.replaceString("ProcessStream_out", "ProcessStream_" + getProdStream(), filename);
		Tools.replaceString("Pipe_out", "Pipe_" + getProdStream(), filename);
		Tools.replaceString("987654321", Float.toString(getPressDrop()), filename);
		Tools.replaceString("123456789", Float.toString(getOutPressure()), filename);
		
		JenaOWLModel owlModel = Tools.callJena(filename);
		Tools.createIRI(owlModel);
		Tools.saveJena(owlModel, filename);
	}
}
