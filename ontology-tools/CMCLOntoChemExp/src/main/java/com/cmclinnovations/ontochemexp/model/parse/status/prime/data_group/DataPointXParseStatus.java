package com.cmclinnovations.ontochemexp.model.parse.status.prime.data_group;

public class DataPointXParseStatus {
	boolean x = false;
	boolean parsed = false;
	
	public boolean isX() {
		return x;
	}

	public void setX(boolean x) {
		this.x = x;
	}
	
	public boolean isParsed() {
		return parsed;
	}
	
	public void setParsed(boolean parsed) {
		this.parsed = parsed;
	}
}
