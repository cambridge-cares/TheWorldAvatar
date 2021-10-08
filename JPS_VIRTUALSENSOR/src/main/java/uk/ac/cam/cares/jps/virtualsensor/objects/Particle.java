package uk.ac.cam.cares.jps.virtualsensor.objects;

public class Particle {
	private double flowrate;
	private double massFraction;
	private double diameter;
	private double density;
	
	public double getFlowrate() {
		return this.flowrate;
	}
	public double getMassFraction () {
		return this.massFraction;
	}
	public double getDiameter() {
		return this.diameter;
	}
	public double getDensity() {
		return this.density;
	}
	public void setFlowrate(double flowrate) {
		this.flowrate = flowrate;
	}
	public void setMassFraction(double massFraction) {
		this.massFraction = massFraction;
	}
	public void setDiameter(double diameter) {
		this.diameter = diameter;
	}
	public void setDensity(double density) {
		this.density = density;
	}
}
