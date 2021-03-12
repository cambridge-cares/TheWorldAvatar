package uk.ac.cam.cares.jps.virtualsensor.objects;

import uk.ac.cam.cares.jps.base.region.Scope;

public class DispSim {
    private int nx;
    private int ny;
    private double[] z;
    private Scope sc;
    private int numSubStations;
    
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
	public double[] getZ() {
		return z;
	}
	public void setZ(double[] z) {
		this.z = z;
	}
	public int getNumSubStations() {
		return numSubStations;
	}
	public void setNumSubStations(int numSubStations) {
		this.numSubStations = numSubStations;
	}
}
