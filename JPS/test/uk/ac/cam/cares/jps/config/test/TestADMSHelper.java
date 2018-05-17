package uk.ac.cam.cares.jps.config.test;

import java.io.IOException;

import com.google.gson.Gson;
import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.util.PythonHelper;
import uk.ac.cam.cares.jps.base.exception.PythonException;

public class TestADMSHelper extends TestCase {
	
	public void testPythonException() throws IOException {
		
		String jsonString = "1";
		
		Gson g = new Gson();
		
		
		boolean pythonExcWasCaught = false;
		try {
			String result = PythonHelper.callPython("caresjpsadmsinputs/ADMSGeoJsonGetter.py", g.toJson(jsonString), this);
//			assertEquals("INVALID QUERY", result);
		} catch (PythonException e) {
			e.printStackTrace();
			pythonExcWasCaught = true;
		}
		
		assertTrue(pythonExcWasCaught);
	}

}
