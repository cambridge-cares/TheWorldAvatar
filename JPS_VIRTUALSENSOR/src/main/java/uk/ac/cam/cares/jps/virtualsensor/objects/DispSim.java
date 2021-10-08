package uk.ac.cam.cares.jps.virtualsensor.objects;

public class DispSim {
    private int nx;
    private int ny;
    private double[] dz;
    private Scope sc;
    private int numSubStations;
    private String serviceAgent;
    private String simCRS; // used for simulation
    
	public int getNx() {
		return nx;
	}
	public void setNx(int nx) {
		this.nx = nx;
	}
	public int getNy() {
		return ny;
	}
	public void setNy(int ny) {
		this.ny = ny;
	}
	public Scope getScope() {
		return sc;
	}
	public void setScope(Scope sc) {
		this.sc = sc;
	}
	public double[] getDz() {
		return dz;
	}
	public void setDz(double[] dz) {
		this.dz = dz;
	}
	public int getNumSubStations() {
		return numSubStations;
	}
	public void setNumSubStations(int numSubStations) {
		this.numSubStations = numSubStations;
	}
	public String getServiceAgent() {
		return serviceAgent;
	}
	public void setServiceAgent(String serviceAgent) {
		this.serviceAgent = serviceAgent;
	}
	public String getSimCRS() {
		return simCRS;
	}
	public void setSimCRS(String simCRS) {
		this.simCRS = simCRS;
	}
}
