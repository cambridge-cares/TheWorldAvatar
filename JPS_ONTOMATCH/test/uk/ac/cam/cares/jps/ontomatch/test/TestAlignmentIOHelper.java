package uk.ac.cam.cares.jps.ontomatch.test;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.junit.Ignore;
import org.junit.Test;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.query.sparql.QueryBuilder;
import uk.ac.cam.cares.jps.ontomatch.alignment.AlignmentIOHelper;


public class TestAlignmentIOHelper extends TestCase {


    @Ignore	
	@Test
	public void testWrite2KB() {
		List<Map> testAlignment = new ArrayList<Map>();
		Map cell1 = new HashMap();
		cell1.put("entity1", "www.test1.com/e1");
		cell1.put("entity2", "www.test2.com/e2");
		cell1.put("measure", 0.7);
		testAlignment.add(cell1);
	try {
		AlignmentIOHelper.writeAlignment2File(testAlignment, "www.test1.com", "www.test2.com", "http://www.theworldavatar.com/kb/testA.owl");
	} catch (IOException e) {
		e.printStackTrace();
	}		
	}
	
    @Test
	public void testreadMap() {
		try {
			List<Map> a = AlignmentIOHelper.readAlignmentFileAsMapList("http://www.theworldavatar.com/a.owl");
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ParseException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}
	
}
