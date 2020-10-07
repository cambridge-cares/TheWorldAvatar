package uk.ac.cam.cares.jps.base.discovery;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class Parameter implements Serializable {

	private static final long serialVersionUID = 2109996100052539444L;
	
	private String key = null;
	private Object value = null;
	
	public Parameter(String key, Object value) {
		setKey(key);
		setValue(value);
	}
	
	public String getKey() {
		return key;
	}
	
	public void setKey(String key) {
		if (key == null) {
			throw new JPSRuntimeException("key must not be null");
		}
		this.key = key;
	}
	
	public Object getValue() {
		return value;
	}
	
	public void setValue(Object value) {
		checkType(value);
		this.value = value;
	}
	
	private void checkType(Object value) {
		
		if ((value == null) || isPrimitiveType(value)) {
			return;
		} else if (value instanceof List) {
			for (Object current : (List) value) {
				if (!isPrimitiveType(current)) {
					throw new JPSRuntimeException("Unsupported element type in list = " + value + ", parameter name = " + key);
				}
			}
			return;
		} else if (value instanceof Map) {
			Map map = (Map) value;
			for (Object current : map.keySet()) {
				if (! (current instanceof String)) {
					throw new JPSRuntimeException("Unsupported key type in map = " + value + ", parameter name = " + key);
				}
				Object mapValue = map.get(current);
				if (! (isPrimitiveType(mapValue))) {
					throw new JPSRuntimeException("Unsupported value type in map = " + value + ", parameter name = " + key);
				}
			}
			return;
		}
		
		throw new JPSRuntimeException("Unsupported type for value = " + value + ", parameter name = " + key);
	}
	
	private boolean isPrimitiveType(Object value) {
		return value instanceof String || value instanceof Number || value instanceof Boolean;
	}
}
