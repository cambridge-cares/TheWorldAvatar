package uk.ac.cam.cares.jps.base.util;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/**
 * This class contains unit tests for MiscUtil.
 * 
 * @author CSL
 *
 */
public class MiscUtilTest {

	/**
	 * Check notNull returns the correct value for a given key from a JSONObject.
	 */
	@Test
	public void testNotNull() {
		
		JSONObject jo = new JSONObject();
		jo.put("testKey", "testValue");
		jo.put("testKey2", "testValue2");

		assertEquals("testValue", MiscUtil.notNull(jo, "testKey"));
	}
	
	/**
	 * Check runtime exception is thrown if value is null for given key.
	 */
	@Test(expected = JPSRuntimeException.class)
	public void testNotNullNullValue() {
		
		JSONObject jo = new JSONObject();
		jo.put("testKey", JSONObject.NULL);

		// Pass JSONObject with null value. Expect exception.
		MiscUtil.notNull(jo, "testKey");
	}

	/**
	 * Check runtime exception is thrown if key does not exist.
	 */
	@Test(expected = JPSRuntimeException.class)
	public void testNotNullNoKey() {
		
		JSONObject jo = new JSONObject();
		jo.put("testKey", "testValue");

		MiscUtil.notNull(jo, "noKey");
	}
	
	/**
	 * Check decimals are formatted with a decimal point.
	 */
	@Test
	public void testFormat() {
		
		assertEquals("Test 12.40", MiscUtil.format("Test %.2f", 12.4));
	}
	
	/**
	 * Checks a string array is concatenated to string.
	 */
	@Test
	public void testConcat() {
		
		String[] strArray = new String[] {"A", "B", "C"};
		assertEquals("A/B/C",MiscUtil.concat(strArray, "/"));	
	}
	
	/**
	 * Test conversion of JSON array to list.
	 */
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
	
	/**
	 * Check that optNullKey returns correct value for given key.
	 */
	@Test
	public void testOptNullKey() {
		JSONObject jo = new JSONObject();
		jo.put("testKey", "testValue");
		jo.put("testKey2", "testValue2");

		assertEquals("testValue", MiscUtil.optNullKey(jo, "testKey"));
	}
	
	/**
	 * Check that optNullKey returns null if JSONObject has null value for given key.
	 */
	@Test
	public void testOptNullKeyNullValue() {
		JSONObject jo = new JSONObject();
		jo.put("testKey", JSONObject.NULL);

		// Pass JSONObject with null value. Expect null.
		assertEquals(null, MiscUtil.optNullKey(jo, "testKey"));
	}
	
	/**
	 * Check that optNullKey returns null if key does not exist.
	 */
	@Test
	public void testOptNullKeyNoKey() {
		JSONObject jo = new JSONObject();
		jo.put("testKey", "testValue");

		// Pass JSONObject with null value. Expect null.
		assertEquals(null, MiscUtil.optNullKey(jo, "noKey"));
	}
}