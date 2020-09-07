package uk.ac.cam.cares.jps.base.util.test;

import static org.junit.Assert.*;

import org.junit.Test;
import org.json.JSONObject;
import org.json.JSONArray;
import java.util.ArrayList;
import uk.ac.cam.cares.jps.base.util.MiscUtil;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class MiscUtilTest {

	@Test
	public void testNotNull() {
		
		JSONObject jo = new JSONObject();
		jo.put("testKey", "testValue");

		assertEquals("testValue", MiscUtil.notNull(jo, "testKey"));
	}
	
	@Test(expected = JPSRuntimeException.class)
	public void testNotNullNull() {
		
		JSONObject jo = new JSONObject();
		jo.put("testKey", JSONObject.NULL);

		MiscUtil.notNull(jo, "testKey");
	}

	@Test
	public void testFormat() {
		
		assertEquals("Test 12.40", MiscUtil.format("Test %.2f", 12.4));
	}
	
	@Test
	public void testConcat() {
		
		String[] strArray = new String[] {"A", "B", "C"};
		assertEquals("A/B/C",MiscUtil.concat(strArray, "/"));
		
	}
	
	@Test
	public void testToList() {
		
		JSONArray ja = new JSONArray();
		ja.put("A");
		ja.put("B");
		ja.put("C");
		
		ArrayList<String> al = new ArrayList<String>();
		al.add("A");
		al.add("B");
		al.add("C");
		
		assertEquals(al, MiscUtil.toList(ja));
	}
	
	@Test
	public void testOptNullKey() {
		JSONObject jo = new JSONObject();
		jo.put("testKey", "testValue");

		assertEquals("testValue", MiscUtil.optNullKey(jo, "testKey"));
	}
	
	//Test null 
	@Test
	public void testOptNullKeyNull() {
		JSONObject jo = new JSONObject();
		jo.put("testKey", JSONObject.NULL);

		assertEquals(null, MiscUtil.optNullKey(jo, "testKey"));
	}
	
	
}
