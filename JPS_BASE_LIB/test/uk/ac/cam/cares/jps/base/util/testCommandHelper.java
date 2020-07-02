package uk.ac.cam.cares.jps.base.util;

import org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import java.util.ArrayList;
import static org.mockito.Mockito.*;


class testCommandHelper {

	CommandHelper commandHelper;
	
	@BeforeEach
	public void init(){
			commandHelper= new CommandHelper();
		}

	@Test
		
		public void testExecuteSingleCommand() {
		String expected = "test.txttest2";
		String actual = commandHelper.executeSingleCommand(System.getProperty("user.dir")+"/test_sample_dir", "ls");
	    assertThrows(JPSRuntimeException.class, () ->commandHelper.executeSingleCommand("sd", "sdsds"),"Unknown exception thrown, expected JPS runtime exception");
	    //doReturn("foo").when(spy).executeSingleCommand(System.getProperty("user.dir")+"/test_sample_dir", "ls");
		assertEquals(expected,actual,"The command was not executed correctly");
		
		}
	
	@Test
	public void testexecuteCommands() {
		ArrayList<String> cmds = new ArrayList<String>();
		cmds.add("ls");
	  // cmds.add("cd test2");  
	  // cmds.add("ls");  
	String expected = "test.txttest2";
	String actual = commandHelper.executeCommands(System.getProperty("user.dir")+"/test_sample_dir", cmds);
    assertThrows(JPSRuntimeException.class, () ->commandHelper.executeCommands("sd", cmds),"Unknown exception thrown, expected JPS runtime exception");
	assertEquals(expected,actual,"The command was not executed correctly");
	
	}
	
	@Test
	public void testexecuteAsyncSingleCommand() {
	String expected = "";
	String actual = commandHelper.executeAsyncSingleCommand(System.getProperty("user.dir")+"/test_sample_dir", "test.txt");
    assertThrows(JPSRuntimeException.class, () ->commandHelper.executeAsyncSingleCommand("sd", "sdsds"),"Unknown exception thrown, expected JPS runtime exception");
	assertEquals(expected,actual,"The command was not executed correctly");
	
	}
	
	}


