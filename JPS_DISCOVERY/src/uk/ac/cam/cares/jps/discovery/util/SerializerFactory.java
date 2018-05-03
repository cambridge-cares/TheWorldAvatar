package uk.ac.cam.cares.jps.discovery.util;

public class SerializerFactory {

	public static ISerializer createSerializer() {
		return new JavaSerializer();
	}
}
