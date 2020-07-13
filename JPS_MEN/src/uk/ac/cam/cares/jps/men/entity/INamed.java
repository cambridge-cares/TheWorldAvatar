package uk.ac.cam.cares.jps.men.entity;

public interface INamed {

	String getName();
	
	static boolean equalNames(INamed obj1, INamed obj2) {
		return (obj1.getName().equals(obj2.getName()));
	}
}
