package uk.ac.cam.cares.jps.composition.util;

import java.net.URI;

public class MatchingTool {

	public static boolean compareURI(URI a, URI b) {		
		return (a.toASCIIString().contentEquals(b.toASCIIString()));
	}
	
	
}
