package uk.ac.cam.cares.jps.base.util;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URISyntaxException;
import java.nio.file.Paths;
import java.util.ArrayList;

/**
 * This class contains unit tests for CommandHelper 
 * 
 * @author CSL
 *
 */
class testCommandHelper {

	private CommandHelper commandHelper;
	private String targetFolder;
	private String resourcesFolderPath;
	
	@BeforeEach
	public void init() throws URISyntaxException {
		
		commandHelper = new CommandHelper();
		
		resourcesFolderPath = Paths.get(this.getClass().getResource("/").toURI()).toFile().getPath(); // path to test resources in target/test-classes/
		targetFolder = resourcesFolderPath; // use the test resources folder as the working directory
	}

	/**
	 * Test executeSingleCcommand. Echo "Hello World!".
	 */
	@Test
	public void testExecuteSingleCommand() {
		String expected = null;
		String actual = null;
		if (CommandHelper.isWindows()) {
			actual = CommandHelper.executeSingleCommand(targetFolder, "cmd /c echo Hello World!");
			expected = "Hello World!";
		} else if (CommandHelper.isMac()) {
			actual = CommandHelper.executeSingleCommand(targetFolder, "echo \"Hello World\"");
			expected = "Hello World!";
		}
		assertEquals(expected, actual, "The command was not executed correctly");
	}

	/**
	 * Check exception thrown for bad path and command
	 */
	@Test
	public void testExecuteSingleCommandException() {

		assertThrows(JPSRuntimeException.class, () -> CommandHelper.executeSingleCommand("sd", "sdsds"),
				"Unknown exception thrown, expected JPS runtime exception");
	}
	
	/**
	 * Test executeCommands. Echo "Hello World!".
	 */
	@Test
	public void testexecuteCommands() {
		ArrayList<String> cmds = new ArrayList<String>();
		String expected = null;
		if (CommandHelper.isMac()) {
			cmds.add("echo");
			cmds.add("\"Hello World!\"");
			expected = "Hello World!";
		} else if (CommandHelper.isWindows()) {
			cmds.add("cmd");
			cmds.add("/c");
			cmds.add("echo");
			cmds.add("Hello World!");
			expected = "\"Hello World!\"";
		}

		String actual = CommandHelper.executeCommands(targetFolder, cmds);
		assertEquals(expected, actual, "The command was not executed correctly");
	}

	/**
	 * Check exception thrown for bad path.
	 */
	@Test
	public void testexecuteCommandsException() {
		
		ArrayList<String> cmds = new ArrayList<String>();
		if (CommandHelper.isMac()) {
			cmds.add("diff");
			cmds.add("test.txt");
			cmds.add("test2.txt");
		} else if (CommandHelper.isWindows()) {
			cmds.add("fc");
			cmds.add("test.txt");
			cmds.add("test2.txt");
		}
		assertThrows(JPSRuntimeException.class, () -> CommandHelper.executeCommands("sd", cmds),
				"Unknown exception thrown, expected JPS runtime exception");
	}
	
	/**
	 * Test executeAsyncSingleCommand. Command run in a separate window. Expected to return "".
	 */
	@Test
	public void testexecuteAsyncSingleCommand() {
		
		String expected = "";
		String actual = CommandHelper.executeAsyncSingleCommand(resourcesFolderPath, "CommandHelperTest.bat");
		assertEquals(expected, actual, "The command was not executed correctly");
	}

	/**
	 * Check exception thrown for bad path and command.
	 */
	@Test
	public void testexecuteAsyncSingleCommandException() {

		// nonexistent target and bad command
		assertThrows(JPSRuntimeException.class, () -> CommandHelper.executeAsyncSingleCommand("sd", "sdsds"),
				"Unknown exception thrown, expected JPS runtime exception");
	}
	
	/**
	 * Test getCommandProcess(String targetFolder, String command), a private member of CommandHelper
	 * called by executeSingleCommand. Expected to return a Process
	 * 
	 * 
	 * @throws NoSuchMethodException
	 * @throws SecurityException
	 * @throws IllegalAccessException
	 * @throws IllegalArgumentException
	 * @throws InvocationTargetException
	 */
	@Test
	public void testGetCommandProcess() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
		
		// access private member
		assertNotNull(commandHelper.getClass().getDeclaredMethod("getCommandProcess", String.class, String.class));
        Method getCommandProcess = commandHelper.getClass().getDeclaredMethod("getCommandProcess", String.class, String.class);
        getCommandProcess.setAccessible(true);
		
        Process pr = null;
 
        if (CommandHelper.isWindows()) {
        	pr = (Process) getCommandProcess.invoke(commandHelper, targetFolder, "cmd /c echo Hello World!");
		} else if (CommandHelper.isMac()) {
			pr = (Process) getCommandProcess.invoke(commandHelper, targetFolder, "echo \"Hello World!\"");
		}
   
        // assert a process is returned
        assertNotNull(pr);
	}
	
	/**
	 * Test getCommandProcess(String targetFolder, String command), JPSRuntimeException thrown by passing nonexistent target folder.
	 * 
	 * @throws NoSuchMethodException
	 * @throws SecurityException
	 * @throws IllegalAccessException
	 * @throws IllegalArgumentException
	 * @throws InvocationTargetException
	 */
	@Test
	public void testGetCommandProcessTargetFolderException() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
		
		// access private member
		assertNotNull(commandHelper.getClass().getDeclaredMethod("getCommandProcess", String.class, String.class));
        Method getCommandProcess = commandHelper.getClass().getDeclaredMethod("getCommandProcess", String.class, String.class);
        getCommandProcess.setAccessible(true);
		
        String nonexistentFolder = targetFolder + "/nonexistentFolder";
        Throwable ex = null;
        
        // Method.Invoke will throw a InvocationTargetException. The cause of this will be a JPSRuntimeException thrown by getCommandProcess.
        if (CommandHelper.isWindows()) {
        	ex = assertThrows(InvocationTargetException.class, () -> getCommandProcess.invoke(commandHelper, nonexistentFolder, "cmd /c echo Hello World!"));
        } else if (CommandHelper.isMac()){
        	ex = assertThrows(InvocationTargetException.class, () -> getCommandProcess.invoke(commandHelper, nonexistentFolder, "echo \"Hello World!\""));
        }
        assertEquals(JPSRuntimeException.class, ex.getCause().getClass(),"Unknown exception thrown, expected JPS runtime exception");
	}
	
	/**
	 * Test getCommandProcess(String targetFolder, String command), JPSRuntimeException thrown by passing bad command.
	 * 
	 * @throws NoSuchMethodException
	 * @throws SecurityException
	 * @throws IllegalAccessException
	 * @throws IllegalArgumentException
	 * @throws InvocationTargetException
	 */
	@Test
	public void testGetCommandProcessCommandException() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
		
		// access private member
		assertNotNull(commandHelper.getClass().getDeclaredMethod("getCommandProcess", String.class, String.class));
        Method getCommandProcess = commandHelper.getClass().getDeclaredMethod("getCommandProcess", String.class, String.class);
        getCommandProcess.setAccessible(true);
   
        // Method.Invoke will throw a InvocationTargetException. The cause of this will be a JPSRuntimeException thrown by getCommandProcess.	
        Throwable ex = assertThrows(InvocationTargetException.class, () -> getCommandProcess.invoke(commandHelper, targetFolder, "badcommand \"Hello World!\""));
        assertEquals(JPSRuntimeException.class, ex.getCause().getClass(),"Unknown exception thrown, expected JPS runtime exception");
	}
	
	/**
	 * Test getCommandProcess(String targetFolder, String [] command), private member called by executeCommands.
	 * Expected to return a process.
	 * 
	 * @throws NoSuchMethodException
	 * @throws SecurityException
	 * @throws IllegalAccessException
	 * @throws IllegalArgumentException
	 * @throws InvocationTargetException
	 */
	@Test
	public void testGetCommandProcessOverload() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
		
		// access private member
		assertNotNull(commandHelper.getClass().getDeclaredMethod("getCommandProcess", String.class, String[].class));
        Method getCommandProcess = commandHelper.getClass().getDeclaredMethod("getCommandProcess", String.class, String[].class);
        getCommandProcess.setAccessible(true);
		
        Process pr = null;
 
        if (CommandHelper.isWindows()) {
        	String[] cmds = {"cmd","/c","echo", "Hello World!"};
        	pr = (Process) getCommandProcess.invoke(commandHelper, targetFolder, cmds);
		} else if (CommandHelper.isMac()) {
			String[] cmds = {"echo", "\"Hello World!\""};
			pr = (Process) getCommandProcess.invoke(commandHelper, targetFolder, cmds);
		}
   
        // assert a process is returned
        assertNotNull(pr);
	}
		
	/**
	 * Test getCommandProcess(String targetFolder, String [] command), JPSRuntimeException thrown by passing nonexistent target folder.
	 * 
	 * @throws NoSuchMethodException
	 * @throws SecurityException
	 * @throws IllegalAccessException
	 * @throws IllegalArgumentException
	 * @throws InvocationTargetException
	 */
	@Test
	public void testGetCommandProcessOverloadTargetFolderException() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
		
		// access private member
		assertNotNull(commandHelper.getClass().getDeclaredMethod("getCommandProcess", String.class, String[].class));
        Method getCommandProcess = commandHelper.getClass().getDeclaredMethod("getCommandProcess", String.class, String[].class);
        getCommandProcess.setAccessible(true);
		
        // path to target/test-classes/
        String nonexistentFolder = targetFolder + "/testNonexistentFolder";
        Throwable ex = null;
        
        // Method.Invoke will throw a InvocationTargetException. The cause of this will be a JPSRuntimeException thrown by getCommandProcess.
        if (CommandHelper.isWindows()) {
        	String[] cmds = {"cmd","/c","echo", "Hello World!"};
        	ex = assertThrows(InvocationTargetException.class, () -> getCommandProcess.invoke(commandHelper, nonexistentFolder, cmds));
        } else if (CommandHelper.isMac()){
        	String[] cmds = {"echo", "\"Hello World!\""};
        	ex = assertThrows(InvocationTargetException.class, () -> getCommandProcess.invoke(commandHelper, nonexistentFolder, cmds));
        }
        assertEquals(JPSRuntimeException.class, ex.getCause().getClass(),"Unknown exception thrown, expected JPS runtime exception");
	}
	
	/**
	 * Test getCommandProcess(String targetFolder, String [] command), JPSRuntimeException thrown by passing bad command.
	 * 
	 * @throws NoSuchMethodException
	 * @throws SecurityException
	 * @throws IllegalAccessException
	 * @throws IllegalArgumentException
	 * @throws InvocationTargetException
	 */
	@Test
	public void testGetCommandProcessOverloadCommandException() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
		
		// access private member
		assertNotNull(commandHelper.getClass().getDeclaredMethod("getCommandProcess", String.class, String[].class));
        Method getCommandProcess = commandHelper.getClass().getDeclaredMethod("getCommandProcess", String.class, String[].class);
        getCommandProcess.setAccessible(true);
		
        String[] cmds = {"badcommand", "\"Hello World!\""};
        
        // Method.Invoke will throw a InvocationTargetException. The cause of this will be a JPSRuntimeException thrown by getCommandProcess.	
        Throwable ex = assertThrows(InvocationTargetException.class, () -> getCommandProcess.invoke(commandHelper, targetFolder, cmds));
        assertEquals(JPSRuntimeException.class, ex.getCause().getClass(),"Unknown exception thrown, expected JPS runtime exception");
	}
	
	/**
	 * Test getAsyncCommandProcess(String targetFolder, String command), private member called by executeAsyncSingleCommand.
	 * 
	 * @throws NoSuchMethodException
	 * @throws SecurityException
	 * @throws IllegalAccessException
	 * @throws IllegalArgumentException
	 * @throws InvocationTargetException
	 */ 
	@Test
	public void testGetAsyncCommandProcess() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
		
		// access private member
		assertNotNull(commandHelper.getClass().getDeclaredMethod("getAsyncCommandProcess", String.class, String.class));
        Method getAsyncCommandProcess = commandHelper.getClass().getDeclaredMethod("getAsyncCommandProcess", String.class, String.class);
        getAsyncCommandProcess.setAccessible(true);
		
        Process pr = (Process) getAsyncCommandProcess.invoke(commandHelper, resourcesFolderPath, "CommandHelperTest.bat");
   
        // assert a process is returned
        assertNotNull(pr);
	}
	
	/**
	 * Test getCommandProcess(String targetFolder, String [] command), JPSRuntimeException thrown by passing nonexistent target folder.
	 * 
	 * @throws NoSuchMethodException
	 * @throws SecurityException
	 * @throws IllegalAccessException
	 * @throws IllegalArgumentException
	 * @throws InvocationTargetException
	 */
	@Test
	public void testGetAsyncCommandProcessException() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
		
		// access private member
		assertNotNull(commandHelper.getClass().getDeclaredMethod("getAsyncCommandProcess", String.class, String.class));
        Method getAsyncCommandProcess = commandHelper.getClass().getDeclaredMethod("getAsyncCommandProcess", String.class, String.class);
        getAsyncCommandProcess.setAccessible(true);
		
        // path to target/test-classes/nonexistentFolder
        String nonexistentFolder = targetFolder + "/testNonexistentFolder";
        
        Throwable ex = null;
        
        // Method.Invoke will throw a InvocationTargetException. The cause of this will be a JPSRuntimeException thrown by getAsyncCommandProcess.
        ex = assertThrows(InvocationTargetException.class, () -> getAsyncCommandProcess.invoke(commandHelper, nonexistentFolder, "CommandHelperTest.bat"));
        assertEquals(JPSRuntimeException.class, ex.getCause().getClass(),"Unknown exception thrown, expected JPS runtime exception");
	}
	
	/**
	 * Check output from executing command is read by getCommandResultString.
	 * 
	 * @throws IOException
	 */
	@Test
	public void testGetCommandResultString() throws IOException {
		
		String expected = "Hello World!";
		Process pr = null;
        File target = new File(targetFolder); 
		
		// create a process
		if (CommandHelper.isMac()) {
	        pr = Runtime.getRuntime().exec("echo \"Hello World!\"", null, target); 
		} else if (CommandHelper.isWindows()) {
	        pr = Runtime.getRuntime().exec("cmd /c echo Hello World!", null, target); 
		}
		
		// assert result string is returned
		if (pr != null) {
			String result = CommandHelper.getCommandResultString(pr);
			assertEquals(expected, result);
		}else {
			fail("The command was not executed correctly.");
		}
	}
}
