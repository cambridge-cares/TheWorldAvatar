package uk.ac.cam.cares.jps.base.util;

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

/**
 * This class contains unit tests for PythonHelper.
 * 
 * @author CSL
 *
 */
public class PythonHelperTest {

	/**
	 * Test processCommand method, which is also called by the callPython methods, tested below.
	 * The python script is run in the test resources directory (target/test-classes).
	 * 
	 * @throws IOException
	 * @throws URISyntaxException
	 */
	@Test
	public void testProcessCommand() throws IOException, URISyntaxException {
		
		// Path to Python script in target/test-classes
		String path = Paths.get(this.getClass().getResource("/PythonHelperTest.py").toURI()).toFile().getPath();
		
		String[] cmd = { "python", path };
		
		String result = PythonHelper.processCommand(cmd);
		
		assertEquals("Hello World!",result);
	}

	private Path pathToScript; // Variable to store the path to python script.  
	
	/**
	 * Set-up for testing callPython methods.
	 * Copy the Python script from test resources into the "python" folder in the current project,
	 * this is the expected location of python scripts in the project.
	 * 
	 * @throws URISyntaxException
	 * @throws IOException
	 */
	@Before
	public void setup() throws URISyntaxException, IOException {
		
		Path source = Paths.get(this.getClass().getResource("/PythonHelperTest.py").toURI());
		
		// Call the same method (AgentLocator.) that is called by the methods under test to get the target path.
		// Not testing AgentLocator methods here. 
	    Path target = Paths.get(AgentLocator.getNewPathToPythonScript("/PythonHelperTest.py", this));	
	    pathToScript = target;
	    
	    Files.createDirectories(target.getParent()); // create directory if it does not already exist
	    Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING);
	}
	
	/**
	 * Clean up test script
	 */
	@After
	public void cleanup() {
		
		pathToScript.toFile().delete();
		assertFalse(pathToScript.toFile().exists());
	}
	
	/**
	 * Call python with one parameter.
	 * Python test script expected to echo "Hello World!" + parameter.
	 * 
	 * @throws IOException
	 */
	@Test
	public void testCallPython1() throws IOException {
		
		String result = PythonHelper.callPython("/PythonHelperTest.py", "param1", this);
		
		assertEquals("Hello World! param1", result);
		
	}
	
	/**
	 * Call python with two parameters.
	 * Python test script expected to echo "Hello World!" + parameters.
	 * 
	 * @throws IOException
	 */
	@Test
	public void testCallPython2() throws IOException {
		
		String result = PythonHelper.callPython("/PythonHelperTest.py", "param1", "param2", this);
		
		assertEquals("Hello World! param1 param2", result);
	}
	
	/**
	 * Call python with four parameters.
	 * Python test script expected to echo "Hello World!" + parameters.
	 * 
	 * @throws IOException
	 */
	@Test
	public void testCallPython4() throws IOException {
		
		String result = PythonHelper.callPython("/PythonHelperTest.py", "param1", "param2", "param3", "param4", this);
		
		assertEquals("Hello World! param1 param2 param3 param4", result);
	}
	
}