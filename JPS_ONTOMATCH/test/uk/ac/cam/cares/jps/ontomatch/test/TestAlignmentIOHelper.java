package uk.ac.cam.cares.jps.ontomatch.test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.ontomatch.AlignmentIOHelper;


public class TestAlignmentIOHelper extends TestCase {

	public void testWrite2KB() {
		List<Map> testAlignment = new ArrayList<Map>();
		Map cell1 = new HashMap();
		cell1.put("entity1", "www.test1.com/e1");
		cell1.put("entity1", "www.test2.com/e2");
		cell1.put("measure", 0.7);
		testAlignment.add(cell1);
	try {
		AlignmentIOHelper.writeAlignment2File(testAlignment, "www.test1.com", "www.test2.com", "http://www.theworldavatar.com/kb/testA.owl");
	} catch (IOException e) {
		e.printStackTrace();
	}
		
	}
	
}
