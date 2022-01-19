package uk.ac.cam.cares.jps.base.config.test;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class TestKeyValueManager extends TestCase {

	public void testReplaceOneKey() {
		
		KeyValueManager.set("key1", "value1");
		String value = KeyValueManager.get("key1");
		assertEquals("value1", value);
		
		KeyValueManager.set("key1", "valueB");
		value = KeyValueManager.get("key1");
		assertEquals("valueB", value);
	}
	
	public void testGetAndSetSeveralKeys() {
		
		KeyValueManager.set("key1", "value1", "key2", "value2", "key3", "value3");
		String value = KeyValueManager.get("key2");
		assertEquals("value2", value);
		
		KeyValueManager.set("key2", "valueA", "key4", "value4", "key5", "value5");
		value = KeyValueManager.get("key2");
		assertEquals("valueA", value);
		
		value = KeyValueManager.get("key5");
		assertEquals("value5", value);
	}
	
	public void testKeysFromPropertyFile() {
		String value = KeyValueManager.get("host");
		boolean ok = false;
		if ("localhost".equals(value)) {
			ok = true;
		}
		assertTrue(ok);
	}
	
}
