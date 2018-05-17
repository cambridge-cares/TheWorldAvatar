package uk.ac.cam.cares.jps.discovery.util;

import java.io.Serializable;
import java.util.Optional;

public interface ISerializer {

	public String convertToString(final Serializable object);
	public <T extends Serializable> Optional<T> convertFrom(final String objectAsString);

}
