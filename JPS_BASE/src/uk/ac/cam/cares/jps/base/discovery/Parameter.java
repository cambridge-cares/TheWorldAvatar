package uk.ac.cam.cares.jps.base.discovery;

import java.io.Serializable;

public class Parameter implements Serializable {

	private static final long serialVersionUID = 2109996100052539444L;
	
	private TypeString key = null;
	private TypeString value = null;
	
	public Parameter(TypeString key, TypeString value) {
		this.key = key;
		this.value = value;
	}
	
	public TypeString getKey() {
		return key;
	}
	
	public void setKey(TypeString key) {
		this.key = key;
	}
	
	public TypeString getValue() {
		return value;
	}
	
	public void setValue(TypeString value) {
		this.value = value;
	}
}
