package ontology_creator;

import java.util.ArrayList;
import java.util.List;

import edu.stanford.smi.protegex.owl.jena.JenaOWLModel;
import edu.stanford.smi.protegex.owl.model.OWLDatatypeProperty;
import edu.stanford.smi.protegex.owl.model.OWLIndividual;
import edu.stanford.smi.protegex.owl.model.OWLNamedClass;

public class HeatExchanger {
	private String name;
	private float duty;
	private String shellFeed;
	private String shellProd;
	private String tubeFeed;
	private String tubeProd;
	private String utilSide;
	private float tubeDeltaT;
	private float tubeDeltaVF;
	private float shellDeltaVF;
	private int utilStartNum;
	private int n_nozzle = 4;
	private String filename;
	
	HeatExchanger(String name) {
		this.name = name;
		this.filename = name + ".owl";
	}
	
	public String getName() {
		return name;
	}
	public float getDuty() {
		return duty;
	}
	public String getShellFeed() {
		return shellFeed;
	}
	public String getShellProd() {
		return shellProd;
	}
	public String getTubeFeed() {
		return tubeFeed;
	}
	public String getTubeProd() {
		return tubeProd;
	}
	public float getTubeDeltaT() {
		return tubeDeltaT;
	}
	public float getTubeDeltaVF() {
		return tubeDeltaVF;
	}
	public float getShellDeltaVF() {
		return shellDeltaVF;
	}
	public String getUtilSide() {
		return utilSide;
	}
	public int getUtilStartNum() {
		return utilStartNum;
	}
	public int getN_nozzle() {
		return n_nozzle;
	}
	public void setName(String name) {
		this.name = name;
	}
	public void setDuty(float duty) {
		this.duty = duty;
	}
	public void setShellFeed(String shellFeed) {
		this.shellFeed = shellFeed;
	}
	public void setShellProd(String shellProd) {
		this.shellProd = shellProd;
	}
	public void setTubeFeed(String tubeFeed) {
		this.tubeFeed = tubeFeed;
	}
	public void setTubeProd(String tubeProd) {
		this.tubeProd = tubeProd;
	}
	public void setUtilSide (String utilSide) {
		this.utilSide = utilSide;
	}
	public void setTubeDeltaT(float tubeDeltaT) {
		this.tubeDeltaT = tubeDeltaT;
	}
	public void setTubeDeltaVF(float tubeDeltaVF) {
		this.tubeDeltaVF = tubeDeltaVF;
	}
	public void setShellDeltaVF(float shellDeltaVF) {
		this.shellDeltaVF = shellDeltaVF;
	}
	public void setUtilStartNum (int utilStartNum) {
		this.utilStartNum = utilStartNum;
	}
	public void inputHeatDuty(JenaOWLModel owlmodel) {
		OWLIndividual HeatDuty = owlmodel.getOWLIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"	+ filename + "#ValueOfHeatDutyOf" + getName());
		OWLDatatypeProperty numericalvalue = owlmodel.getOWLDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
		HeatDuty.setPropertyValue(numericalvalue, getDuty());
	}
	
	public void createOWLFile() {
		Tools.replaceString("E-340001", getName(), "OWL Templates/E-340001_Template.owl", filename);
		
		if (getTubeDeltaVF()<0 && getUtilSide().equalsIgnoreCase("Shell") || getShellDeltaVF()<0 && getUtilSide().equalsIgnoreCase("Tube")) {
			Tools.replaceString("Cooling_", "Condensing_", filename);
			Tools.replaceString("TemperatureChange", "PhaseChange", filename);
		}
		else if (getTubeDeltaVF()>0 && getUtilSide().equalsIgnoreCase("Shell") || getShellDeltaVF()>0 && getUtilSide().equalsIgnoreCase("Tube")) {
			Tools.replaceString("Cooling_", "Vaporizing_", filename);
			Tools.replaceString("TemperatureChange", "PhaseChange", filename);
		}
		else if ((getUtilSide().equalsIgnoreCase("Tube") && getTubeDeltaT() < 0) || (getUtilSide().equalsIgnoreCase("Shell") && getTubeDeltaT() > 0)) {
			Tools.replaceString("Cooling_", "Heating_", filename);
		}
		
		if (getUtilSide().equalsIgnoreCase("Shell")) {
			Tools.replaceString("_340008", "_" + getTubeFeed(), filename);
			Tools.replaceString("_340009", "_" + getTubeProd(), filename);
			Tools.replaceString("_340031", "_" + getShellFeed(), filename);
			Tools.replaceString("_340032", "_" + getShellProd(), filename);
		}
		else if (getUtilSide().equalsIgnoreCase("Tube")) {
			Tools.replaceString("_340008", "_" + getShellFeed(), filename);
			Tools.replaceString("_340009", "_" + getShellProd(), filename);
			Tools.replaceString("_340031", "_" + getTubeFeed(), filename);
			Tools.replaceString("_340032", "_" + getTubeProd(), filename);
		}
		
		JenaOWLModel owlmodel = Tools.callJena(filename);
		inputHeatDuty(owlmodel);
		// For heat exchangers with process streams on both sides
		List<OWLIndividual> OWLIndividuals = new ArrayList<OWLIndividual>(owlmodel.getOWLIndividuals());
		for (int i=0; i<OWLIndividuals.size(); i++) {
			try {
				OWLIndividual obj = OWLIndividuals.get(i);
				if (obj.getLocalName().startsWith("UtilityStream_")) {
					String streamName = obj.getLocalName();
					int streamNumber = Integer.parseInt(streamName.substring(streamName.length()-6));
					System.out.println(streamNumber);
					if (streamNumber < utilStartNum) {
						OWLNamedClass ProcessStreamClass = owlmodel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#ProcessStream");
						obj.setRDFType(ProcessStreamClass);
						obj.rename("ProcessStream_" + Integer.toString(streamNumber));
					}
				}
				else {}
			} catch (ClassCastException e) {}
		}
		Tools.createIRI(owlmodel);
		Tools.saveJena(owlmodel, filename);
	}
}
