package uk.ac.cam.cares.jps.base.util;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.File;
import java.util.ArrayList;
import static org.mockito.Mockito.*;

class testCommandHelper {

	CommandHelper commandHelper;

	@BeforeEach
	public void init() {
		commandHelper = new CommandHelper();
	}

	@Test

	public void testExecuteSingleCommand() {
		String expected = "Sum.javatest.txttest2";
		String actual = null;
		if (commandHelper.isWindows()) {
			actual = commandHelper.executeSingleCommand(System.getProperty("user.dir") + "/test_sample_dir", "dir");
		} else if (commandHelper.isMac()) {
			actual = commandHelper.executeSingleCommand(System.getProperty("user.dir") + "/test_sample_dir", "ls");
		}
		assertThrows(JPSRuntimeException.class, () -> commandHelper.executeSingleCommand("sd", "sdsds"),
				"Unknown exception thrown, expected JPS runtime exception");
		assertEquals(expected, actual, "The command was not executed correctly");

	}

	@Test
	public void testexecuteCommands() {
		ArrayList<String> cmds = new ArrayList<String>();
		if (commandHelper.isWindows()) {
			cmds.add("dir");
		} else if (commandHelper.isMac()) {
			cmds.add("java");
			cmds.add("Sum.java");
			cmds.add("1");
			cmds.add("2");
			cmds.add("3");
			
		}
		String expected = "test2/test.txt";
		String actual = commandHelper.executeCommands(System.getProperty("user.dir") + "/test_sample_dir/", cmds);
		assertThrows(JPSRuntimeException.class, () -> commandHelper.executeCommands("sd", cmds),
				"Unknown exception thrown, expected JPS runtime exception");
		assertEquals(expected, actual, "The command was not executed correctly");

	}

	@Test
	public void testexecuteAsyncSingleCommand() {
		String expected = "";
		String actual = commandHelper.executeAsyncSingleCommand(System.getProperty("user.dir") + "/test_sample_dir","test.txt");
		assertThrows(JPSRuntimeException.class, () -> commandHelper.executeAsyncSingleCommand("sd", "sdsds"),
				"Unknown exception thrown, expected JPS runtime exception");
		assertEquals(expected, actual, "The command was not executed correctly");

	}

}
