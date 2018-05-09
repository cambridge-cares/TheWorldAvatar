package uk.ac.cam.cares.jps.config.test;

import java.io.IOException;

import com.google.gson.Gson;
import junit.framework.TestCase;
import uk.ac.cam.cares.jps.config.AgentLocator;
import uk.ac.cam.cares.jps.util.PythonHelper;

// IMPORTANT: have to put test.levels.gst file in the actual JPS directory e.g. C:\Users\WE\Desktop\JPS\JParkSimulator-git\JPS\workingdir\ADMS\caresjpsadmsinputs\test.levels.gst

public class TestADMSOutput extends TestCase {
	
	public void testDoGetHttpServletRequestHttpServletResponse() {
		
		String jsonString = "[79173.32,454193.47]";
		
		Gson g = new Gson();
		
		String outputFile = AgentLocator.getPathToWorkingDir() + "/" + "ADMS/caresjpsadmsinputs/test.levels.gst";
		
		try {
		
			String result = PythonHelper.callPython("caresjpsadmsinputs/ADMSOutput.py", outputFile , g.toJson(jsonString), this);
			assertEquals("[79173.32, 454193.47, 55.1295, 54.2263, 51.6046, 47.5137]", result);
			
		} catch (IOException ie) {
			ie.printStackTrace();
		}
	}

}
