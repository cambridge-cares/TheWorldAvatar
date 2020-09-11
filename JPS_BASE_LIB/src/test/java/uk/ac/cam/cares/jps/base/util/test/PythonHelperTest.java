package uk.ac.cam.cares.jps.base.util.test;

import static org.junit.Assert.*;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;

import org.junit.Test;
import org.junit.Before;
import org.junit.After;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.util.PythonHelper;

public class PythonHelperTest {

	@Test
	public void testProcessCommand() throws IOException, URISyntaxException {
		
		// Path to Python script in target/test-classes
		// (Method copied from AgentLocator.getCurrentJpsAppDirectory)
		String path = Paths.get(this.getClass().getResource("/PythonHelperTest.py").toURI()).toFile().getPath();
		
		String[] cmd = { "python", path };
		
		String result = PythonHelper.processCommand(cmd);
		
		assertEquals("Hello World!",result);
	}

	
	private Path pathToScript; 
	
	// Set-up for testing callPython
	// Copy the Python script from test resources into the python folder in the current project. 
	// This is the expected location of python scripts.
	@Before
	public void setup() throws URISyntaxException, IOException {
		
		Path source = Paths.get(this.getClass().getResource("/PythonHelperTest.py").toURI());
		// Call the same method called by the methods under test to give the target path... Not testing AgentLocator methods here. Assume correct. 
	    Path target = Paths.get(AgentLocator.getNewPathToPythonScript("/PythonHelperTest.py", this));	
	    pathToScript = target;
	    
	    Files.createDirectories(target.getParent()); // create directory if it does not already exist
	    Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING);
	}
		
	// Clean up test script
	@After
	public void cleanup() {
		
		pathToScript.toFile().delete();
		
		assertFalse(pathToScript.toFile().exists());
	}
	
	@Test
	public void testCallPython1() throws IOException {
		
		String result = PythonHelper.callPython("/PythonHelperTest.py", "param1", this);
		
		assertEquals("Hello World! param1", result);
		
	}
	
	@Test
	public void testCallPython2() throws IOException {
		
		String result = PythonHelper.callPython("/PythonHelperTest.py", "param1", "param2", this);
		
		assertEquals("Hello World! param1 param2", result);
	}
	
	@Test
	public void testCallPython4() throws IOException {
		
		String result = PythonHelper.callPython("/PythonHelperTest.py", "param1", "param2", "param3", "param4", this);
		
		assertEquals("Hello World! param1 param2 param3 param4", result);
	}
	
}