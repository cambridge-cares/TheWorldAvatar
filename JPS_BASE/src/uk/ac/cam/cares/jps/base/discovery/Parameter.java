package uk.ac.cam.cares.jps.base.discovery;

import java.io.Serializable;

public class Parameter implements Serializable {

	private static final long serialVersionUID = 2109996100052539444L;
	
	private String key = null;
	private String value = null;
	
	public Parameter(String key, String value) {
		this.key = key;
		this.value = value;
	}
	
	public String getKey() {
		return key;
	}
	
	public void setKey(String key) {
		this.key = key;
	}
	
	public String getValue() {
		return value;
	}
	
	public void setValue(String value) {
		this.value = value;
	}
}
