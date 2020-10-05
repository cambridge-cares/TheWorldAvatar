package uk.ac.cam.cares.jps.base.util.test;



import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;


public class TestUtils extends TestCase {
	
	private final static String CSV = 
			"building,name\r\n" + 
			"http://www.theworldavatar.com/Building/10_buildings0.owl#BuildingGUID_83EFA0E4-FC06-46B3-8482-E38C8CF602BC,{83EFA0E4-FC06-46B3-8482-E38C8CF602BC}\r\n" + 
			"http://www.theworldavatar.com/Building/10_buildings0.owl#BuildingGUID_E77C9F0F-554A-4986-8332-75EDFF2DCF07,{E77C9F0F-554A-4986-8332-75EDFF2DCF07}\r\n" + 
			"http://www.theworldavatar.com/Building/10_buildings0.owl#BuildingGUID_E5F7DC6A-59FE-4DC2-A826-0CABEEB70451,{E5F7DC6A-59FE-4DC2-A826-0CABEEB70451}\r\n" + 
			"http://www.theworldavatar.com/Building/10_buildings0.owl#BuildingGUID_94405F3C-FB53-4EC8-93A1-5F95FEC74CBD,{94405F3C-FB53-4EC8-93A1-5F95FEC74CBD}\r\n" + 
			"http://www.theworldavatar.com/Building/10_buildings0.owl#BuildingGUID_8D251403-40CD-463F-AE65-AC8951A1B2FD,{8D251403-40CD-463F-AE65-AC8951A1B2FD}\r\n";


	public static void main(String[] args) {

		String targetFolder = "/home/zhouxiaochi/Documents/JPS/JParkSimulator-git/JPS_BASE/src/uk/ac/cam/cares/jps/base/util/T";
		
		ArrayList<String> list = new ArrayList<String>();
		list.add("python3");
		list.add("foo.py");
		list.add("The password of the server is ...");
		
		
		CommandHelper.executeSingleCommand(targetFolder, "touch bar2.txt");
		CommandHelper.executeCommands(targetFolder, list);
		
	}
	
	public void testCsvToMatrixConversion() {
		
		Map<String, List<String>> matrix = MatrixConverter.fromCsv(CSV);
		
		//assertEquals(2, matrix.size());
		assertEquals(5, matrix.get("building").size());
		assertEquals(5, matrix.get("name").size());
		assertEquals("http://www.theworldavatar.com/Building/10_buildings0.owl#BuildingGUID_E77C9F0F-554A-4986-8332-75EDFF2DCF07", matrix.get("building").get(1));
		assertEquals("{E77C9F0F-554A-4986-8332-75EDFF2DCF07}", matrix.get("name").get(1));
	}
	
	public void testCsvToArrayConversion() {
		
		List<String[]> rows = MatrixConverter.fromCsvToArray(CSV);
		assertEquals("building", rows.get(0)[0]);
		String actual = MatrixConverter.fromArraytoCsv(rows);
		assertEquals(CSV, actual);
	}
	/** test ValidateInput boolean for InputValidator class
	 * 
	 */
	public void testBoolean() {
		boolean trueBool = InputValidator.checkBoolean("true");
		assertTrue(trueBool);
		boolean falseBool = InputValidator.checkBoolean("False");
		assertTrue(falseBool);
		boolean fakeBool = InputValidator.checkBoolean("fake");
		assertFalse(fakeBool);
	}
}
