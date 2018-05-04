package uk.ac.cam.cares.jps.config.test;

import java.io.IOException;

import com.google.gson.Gson;
import junit.framework.TestCase;
import uk.ac.cam.cares.jps.adms.PythonException;
import uk.ac.cam.cares.jps.util.PythonHelper;

public class TestADMSHelper extends TestCase {
	
	public void testPythonException() throws IOException {
		
		String jsonString = "";
		
		Gson g = new Gson();
		
		
		boolean pythonExcWasCaught = false;
		try {
			String result = PythonHelper.callPython("caresjpsadmsinputs/ADMSGeoJsonGetter.py", g.toJson(jsonString));
//			assertEquals("INVALID QUERY", result);
		} catch (PythonException e) {
			e.printStackTrace();
			pythonExcWasCaught = true;
		}
		
		assertTrue(pythonExcWasCaught);
	}

}
