package uk.ac.cam.cares.jps.virtualsensor.configuration;

public class EpisodeConfig {
	private double dx_rec=100.0; //TODO hardcoded? decide the dx for the receptor
	private double dy_rec=100.0;//TODO hardcoded? decide the dy for the receptor
	private int nx=10 ;//decide the nx for the scope
	private int ny=10 ;//decide the ny for the scope	
	private double dz=20;//10 previously
	private int nz=30;//13 previously
	private double z_rec=9.5;
	private double upperheight=25.0;
	private double lowerheight=2.0;
	//typical gradient = 0.6K/100m
	//therefore deltaT= 0.23*0.6= 0.138
	private double deltaT=0.138; //the unit is on K , not K/m 
	public double getDx_rec() {
		return dx_rec;
	}
	public void setDx_rec(double dx_rec) {
		this.dx_rec = dx_rec;
	}
	public double getDy_rec() {
		return dy_rec;
	}
	public void setDy_rec(double dy_rec) {
		this.dy_rec = dy_rec;
	}
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
	public double getDz() {
		return dz;
	}
	public void setDz(double dz) {
		this.dz = dz;
	}
	public int getNz() {
		return nz;
	}
	public void setNz(int nz) {
		this.nz = nz;
	}
	public double getUpperheight() {
		return upperheight;
	}
	public void setUpperheight(double upperheight) {
		this.upperheight = upperheight;
	}
	public double getLowerheight() {
		return lowerheight;
	}
	public void setLowerheight(double lowerheight) {
		this.lowerheight = lowerheight;
	}
	public double getDeltaT() {
		return deltaT;
	}
	public void setDeltaT(double deltaT) {
		this.deltaT = deltaT;
	}
	public double getZ_rec() {
		return z_rec;
	}
	public void setZ_rec(double z_rec) {
		this.z_rec = z_rec;
	}

}
