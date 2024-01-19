package uk.ac.cam.cares.jps.ontomatch.test;

import org.junit.Test;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.ontomatch.properties.OntomatchProperties;

public class TestOntomatchProperties extends TestCase{

	@Test
	public void testGetProperty() {
		String pyname2 = OntomatchProperties.getInstance().getProperty(OntomatchProperties.PY_NAME_LEXICALPROCESSOR);
        System.out.println(pyname2);

		assertEquals("ontologyWrapper.py", pyname2);

	}
	
}
