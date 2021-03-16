package uk.ac.cam.cares.jps.virtualsensor.sparql;

public class GasPollutant {
	private String species;
	private double flowrate;
	
	public GasPollutant() {}
	
	public String getSpecies() {
		return this.species;
	}
	public double getFlowrate() {
		return this.flowrate;
	}
	public void setSpecies(String species) {
		this.species = species;
	}
	public void setFlowrate(double flowrate) {
		this.flowrate = flowrate;
	}
}
