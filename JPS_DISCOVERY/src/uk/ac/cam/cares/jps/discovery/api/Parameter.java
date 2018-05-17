package uk.ac.cam.cares.jps.discovery.api;

import java.io.Serializable;

public class Parameter implements Serializable {

	private static final long serialVersionUID = 2109996100052539444L;
	
	private IType key = null;
	private IType value = null;
	
	public Parameter(IType key, IType value) {
		this.key = key;
		this.value = value;
	}
	
	public IType getKey() {
		return key;
	}
	
	public void setKey(IType key) {
		this.key = key;
	}
	
	public IType getValue() {
		return value;
	}
	
	public void setValue(IType value) {
		this.value = value;
	}
}
