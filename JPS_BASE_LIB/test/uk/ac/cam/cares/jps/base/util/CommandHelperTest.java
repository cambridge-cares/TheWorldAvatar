package uk.ac.cam.cares.jps.base.util;

import org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

class CommandHelperTest {

	CommandHelper commandHelper;
	
	@BeforeEach
	public void init(){
			commandHelper= new CommandHelper();
		}

	@Test
		public void testExecuteSingleCommand() {
		String expected = "";
		String actual = commandHelper.executeSingleCommand(System.getProperty("user.dir")+"/test_sample_dir", "javac Hello.java");
		assertThrows(JPSRuntimeException.class, () ->commandHelper.executeSingleCommand("sd", "sdsds"),"Unknown exception thrown, expected JPS runtime exception");
		assertEquals(expected,actual,"The command was not executed correctly");
		
		}
	}


