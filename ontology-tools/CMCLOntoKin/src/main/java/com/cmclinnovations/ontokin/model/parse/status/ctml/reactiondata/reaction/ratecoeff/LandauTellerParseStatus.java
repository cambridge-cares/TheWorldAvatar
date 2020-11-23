package com.cmclinnovations.ontokin.model.parse.status.ctml.reactiondata.reaction.ratecoeff;

public class LandauTellerParseStatus {
	boolean landauTeller = false;
	boolean B = false;
	boolean BUnits = false;
	boolean C = false;
	boolean CUnits = false;
	public boolean isLandauTeller() {
		return landauTeller;
	}
	public void setLandauTeller(boolean landauTeller) {
		this.landauTeller = landauTeller;
	}
	public boolean isB() {
		return B;
	}
	public void setB(boolean b) {
		B = b;
	}
	public boolean isC() {
		return C;
	}
	public void setC(boolean c) {
		C = c;
	}
	public boolean isBUnits() {
		return BUnits;
	}
	public void setBUnits(boolean bUnits) {
		BUnits = bUnits;
	}
	public boolean isCUnits() {
		return CUnits;
	}
	public void setCUnits(boolean cUnits) {
		CUnits = cUnits;
	}
}
