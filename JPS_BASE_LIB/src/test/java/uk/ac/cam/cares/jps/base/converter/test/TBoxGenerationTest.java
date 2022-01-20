package uk.ac.cam.cares.jps.base.converter.test;

import org.junit.jupiter.api.Test;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class TBoxGenerationTest {
	
	@Test
	public void testClassHierachy(){
		
	}
	
	@Test
	public void testObjectPropertyHierachy(){
		
	}

	@Test
	public void testObjectPropertyInverseOfRelation(){
		
	}

	@Test
	public void testObjectPropertyCharacteristics(){
		
	}
	
	@Test
	public void testDataPropertyHierachy(){
		
	}

	@Test
	public void testDataPropertyCharacteristics(){
		
	}

	@Test
	public void testEquivalentRelation(){
		
	}
	
	/**
	 * Adds the protocol 'file:' at the beginning of a file path
	 * to form a URL that can be used in an OWL file as a URL.
	 * 
	 * @param path an absolute file path that needs to be converted
	 * to a URL that can be used in an OWL file.
	 * @return an OWL file formatted URL.
	 * @throws JPSRuntimeException a specialised exception designed to deal with
	 * errors at runtime in JPS libraries.
	 */
	public static String addFileProtocol(String path) throws JPSRuntimeException{
		if(path==null){
			throw new JPSRuntimeException("A null input path has been provided.");
		}
		if(!path.contains("file:")){
			path = "file:"+path;
		}
		return path;
	}
}
