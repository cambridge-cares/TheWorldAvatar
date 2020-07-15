package uk.ac.cam.cares.jps.base.util;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import java.util.ArrayList;

class testCommandHelper {

	CommandHelper commandHelper;

	@BeforeEach
	public void init() {
		commandHelper = new CommandHelper();
	}

	@Test
	public void testExecuteSingleCommand() {
		String expected = "hello.battest.txttest2test2.txt";
		String actual = null;
		if (commandHelper.isWindows()) {
			actual = commandHelper.executeSingleCommand(System.getProperty("user.dir") + "/test_sample_dir/", "fc test.txt test2.txt");
			expected = "Comparing files test.txt and TEST2.TXT***** test.txtDfds***** TEST2.TXTDfdscopy*****";
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
		String expected = null;
		if (commandHelper.isMac()) {
			cmds.add("diff");
			cmds.add("test.txt");
			cmds.add("test2.txt");
			expected = "1c1< Dfds\\ No newline at end of file---> Dfdscopy\\ No newline at end of file";
		} else if (commandHelper.isWindows()) {
			cmds.add("fc");
			cmds.add("test.txt");
			cmds.add("test2.txt");
			expected = "Comparing files test.txt and TEST2.TXT***** test.txtDfds***** TEST2.TXTDfdscopy*****";
		}

		String actual = commandHelper.executeCommands(System.getProperty("user.dir") + "/test_sample_dir/", cmds);
		assertThrows(JPSRuntimeException.class, () -> commandHelper.executeCommands("sd", cmds),
				"Unknown exception thrown, expected JPS runtime exception");
		assertEquals(expected, actual, "The command was not executed correctly");
	}

	@Test
	public void testexecuteAsyncSingleCommand() {
		String expected = "";
		String actual = commandHelper.executeAsyncSingleCommand(System.getProperty("user.dir") + "/test_sample_dir",
				"hello.bat");
		assertThrows(JPSRuntimeException.class, () -> commandHelper.executeAsyncSingleCommand("sd", "sdsds"),
				"Unknown exception thrown, expected JPS runtime exception");
		assertEquals(expected, actual, "The command was not executed correctly");

	}

}
