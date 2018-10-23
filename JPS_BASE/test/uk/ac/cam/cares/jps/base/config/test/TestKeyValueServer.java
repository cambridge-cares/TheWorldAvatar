package uk.ac.cam.cares.jps.base.config.test;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.KeyValueServer;

public class TestKeyValueServer extends TestCase {

	public void testReplaceOneKey() {
		
		KeyValueServer.set("key1", "value1");
		String value = KeyValueServer.get("key1");
		assertEquals("value1", value);
		
		KeyValueServer.set("key1", "valueB");
		value = KeyValueServer.get("key1");
		assertEquals("valueB", value);
	}
	
	public void testGetAndSetSeveralKeys() {
		
		KeyValueServer.set("key1", "value1", "key2", "value2", "key3", "value3");
		String value = KeyValueServer.get("key2");
		assertEquals("value2", value);
		
		KeyValueServer.set("key2", "valueA", "key4", "value4", "key5", "value5");
		value = KeyValueServer.get("key2");
		assertEquals("valueA", value);
		
		value = KeyValueServer.get("key5");
		assertEquals("value5", value);
	}
	
	public void testKeysFromPropertyFile() {
		String value = KeyValueServer.get("host");
		boolean ok = false;
		if ("www.theworldavatar.com".equals(value) || "localhost".equals(value)) {
			ok = true;
		}
		assertTrue(ok);
	}
}
