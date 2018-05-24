package uk.ac.cam.cares.jps.discovery.factory;

import uk.ac.cam.cares.jps.discovery.registry.IRegistry;
import uk.ac.cam.cares.jps.discovery.registry.SimpleInMemoryRegistry;
import uk.ac.cam.cares.jps.discovery.util.ISerializer;
import uk.ac.cam.cares.jps.discovery.util.JsonSerializer;

public class DiscoveryFactory {

	public static ISerializer getSerializer() {
		return new JsonSerializer();
	}
	
	public static IRegistry getRegistry() {
		return SimpleInMemoryRegistry.getInstance();
	}
}
