package ontology_creator;

import edu.stanford.smi.protegex.owl.jena.JenaOWLModel;


public class Separator {
	private String name;
	private String feedStream;
	private String liqProdStream;
	private String vapProdStream;
	private float duty;
	private float temp;
	private float pressDrop;
	private String filename;
	
	Separator(String name) {
		this.name = name;
		this.filename = name +".owl";
	}

	public String getName() {
		return name;
	}

	public String getFeedStream() {
		return feedStream;
	}

	public String getLiqProdStream() {
		return liqProdStream;
	}

	public String getVapProdStream() {
		return vapProdStream;
	}

	public float getDuty() {
		return duty;
	}

	public float getTemp() {
		return temp;
	}

	public float getPressDrop() {
		return pressDrop;
	}

	public void setName(String name) {
		this.name = name;
	}

	public void setFeedStream(String feedStream) {
		this.feedStream = feedStream;
	}

	public void setLiqProdStream(String liqProdStream) {
		this.liqProdStream = liqProdStream;
	}

	public void setVapProdStream(String vapProdStream) {
		this.vapProdStream = vapProdStream;
	}

	public void setDuty(float duty) {
		this.duty = duty;
	}

	public void setTemp(float temp) {
		this.temp = temp;
	}

	public void setPressDrop(float pressDrop) {
		this.pressDrop = pressDrop;
	}
	
	public void createOWLFile() {
		/**
		 * Precondition:
		 * Assumes that the template file exists at the specified location
		 * 
		 * Postcondition:
		 * creates the OWL file for 1 separator instance
		 * 
		 */
		Tools.replaceString("V-300000", getName(), "OWL Templates/V-300000_Template.owl", filename);
		Tools.replaceString("ProcessStream_in", "ProcessStream_" + getFeedStream(), filename);
		Tools.replaceString("Pipe_in", "Pipe_" + getFeedStream(), filename);
		Tools.replaceString("ProcessStream_out1", "ProcessStream_" + getLiqProdStream(), filename);
		Tools.replaceString("Pipe_out1", "Pipe_" + getLiqProdStream(), filename);
		Tools.replaceString("ProcessStream_out2", "ProcessStream_" + getVapProdStream(), filename);
		Tools.replaceString("Pipe_out2", "Pipe_" + getVapProdStream(), filename);
		Tools.replaceString("123456789", Float.toString(getDuty()), filename);
		Tools.replaceString("987654321", Float.toString(getTemp()), filename);
		Tools.replaceString("147258369", Float.toString(getPressDrop()), filename);
		
		JenaOWLModel owlModel = Tools.callJena(filename);
		Tools.createIRI(owlModel);
		Tools.saveJena(owlModel, filename);
	}
}
