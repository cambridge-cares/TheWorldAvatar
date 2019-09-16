package uk.ac.cam.cares.jps.config.test;

import java.io.IOException;
import java.util.List;

import com.google.gson.Gson;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.adms.ADMSOutputAllForShips;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.PythonException;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.base.util.PythonHelper;

public class TestADMSHelper extends TestCase {
	
	public void testPythonException() throws IOException {
		
		String jsonString = "1";
		
		Gson g = new Gson();
		
		
		boolean pythonExcWasCaught = false;
		try {
			String result = PythonHelper.callPython("caresjpsadmsinputs/ADMSGeoJsonGetter.py", g.toJson(jsonString), this);
			System.out.println("result="+result);
//			assertEquals("INVALID QUERY", result);
		} catch (PythonException e) {
			e.printStackTrace();
			pythonExcWasCaught = true;
		}
		
		assertTrue(pythonExcWasCaught);
	}
	
	public void testpolcalculation() {
		
		
		String csv = new QueryBroker().readFile("D:/JPS-git/JParkSimulator-git/JPS_SHIP/workingdir/test.levels.gst");
		List<String[]> simulationResult = MatrixConverter.fromCsvToArray(csv);
		int startcontentindex=7;
		int sizeofpol = new ADMSOutputAllForShips().findHowManyPol(simulationResult, startcontentindex);
		int heightamount=(simulationResult.get(0).length-startcontentindex)/sizeofpol;
		
		assertEquals(4,heightamount);
		assertEquals(7,sizeofpol);
		
	}

}
