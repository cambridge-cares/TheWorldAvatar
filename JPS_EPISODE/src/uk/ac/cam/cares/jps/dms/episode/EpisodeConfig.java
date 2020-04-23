package uk.ac.cam.cares.jps.dms.episode;

public class EpisodeConfig {
	private double dx_rec=100; //TODO hardcoded? decide the dx for the receptor
	private double dy_rec=100;//TODO hardcoded? decide the dy for the receptor
	private double dx=1000.0 ;//decide the dx for the scope
	private double dy=1000.0 ;//decide the dy for the scope	
	private double dz=10;
	private double nz=13;
	private double upperheight=75.0;
	private double lowerheight=2.0;
	private double deltaT=0.022;
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
	public double getDx() {
		return dx;
	}
	public void setDx(double dx) {
		this.dx = dx;
	}
	public double getDy() {
		return dy;
	}
	public void setDy(double dy) {
		this.dy = dy;
	}
	public double getDz() {
		return dz;
	}
	public void setDz(double dz) {
		this.dz = dz;
	}
	public double getNz() {
		return nz;
	}
	public void setNz(double nz) {
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

}
