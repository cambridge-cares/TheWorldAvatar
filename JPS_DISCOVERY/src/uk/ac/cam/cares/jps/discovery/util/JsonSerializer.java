package uk.ac.cam.cares.jps.discovery.util;

import java.io.Serializable;
import java.util.Optional;

import com.google.gson.Gson;

public class JsonSerializer implements ISerializer {

	@Override
	public String convertToString(Serializable object) {
		return new Gson().toJson(object);
	}

	@Override
	public <T extends Serializable> Optional<T> convertFrom(String objectAsString, Class<T> classtype) {
		Object o = new Gson().fromJson(objectAsString, classtype);
		
		return Optional.of((T) o);
	}

	
	
}
