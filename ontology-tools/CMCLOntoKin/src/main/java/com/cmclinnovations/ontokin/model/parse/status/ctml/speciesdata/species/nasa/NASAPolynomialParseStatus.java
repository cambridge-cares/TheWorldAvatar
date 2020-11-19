package com.cmclinnovations.ontokin.model.parse.status.ctml.speciesdata.species.nasa;

public class NASAPolynomialParseStatus {
	boolean NASA = false;
	boolean Tmax = false;
	boolean Tmin = false;
	boolean P0 = false;
	public boolean isNASA() {
		return NASA;
	}
	public void setNASA(boolean nASA) {
		NASA = nASA;
	}
	public boolean isTmax() {
		return Tmax;
	}
	public void setTmax(boolean tmax) {
		Tmax = tmax;
	}
	public boolean isTmin() {
		return Tmin;
	}
	public void setTmin(boolean tmin) {
		Tmin = tmin;
	}
	public boolean isP0() {
		return P0;
	}
	public void setP0(boolean p0) {
		P0 = p0;
	}
}
