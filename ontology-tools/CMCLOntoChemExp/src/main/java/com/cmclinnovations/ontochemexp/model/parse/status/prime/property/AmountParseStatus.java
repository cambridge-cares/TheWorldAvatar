package com.cmclinnovations.ontochemexp.model.parse.status.prime.property;

public class AmountParseStatus {
	boolean amount = false;
	boolean units = false;

	public boolean isAmount() {
		return amount;
	}

	public void setAmount(boolean amount) {
		this.amount = amount;
	}

	public boolean isUnits() {
		return units;
	}

	public void setUnits(boolean units) {
		this.units = units;
	}
}
