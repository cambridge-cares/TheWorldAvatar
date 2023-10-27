package uk.ac.cam.cares.jps.base.cache;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class LRUCacheTest {

	@Test
	void testConstructor() {
		
		int size = 100;
		LRUCache<String,String> lruCache = new LRUCache<String,String>(size);
		
		assertTrue(lruCache.isEmpty());
		assertEquals(size, lruCache.capacity());
	}
		
	@Test
	void testLRU() {
		LRUCache<String,String> lruCache = new LRUCache<String,String>(3);
		
		lruCache.put("Key1","Value1");
		lruCache.put("Key2","Value2");
		lruCache.put("Key3","Value3");
		
		assertTrue(lruCache.contains("Key1"));
		assertEquals("Value1",lruCache.get("Key1")); //Key1 Least Recently Used 
		
		assertTrue(lruCache.contains("Key2"));
		assertEquals("Value2",lruCache.get("Key2"));
		
		assertTrue(lruCache.contains("Key3"));
		assertEquals("Value3",lruCache.get("Key3"));
		
		//Order is {3,2,1} (Most -> Least recently used)
		
		//Put Key4 and drop LRU Key1
		lruCache.put("Key4","Value4");
		//Order is {4,3,2}
		assertFalse(lruCache.contains("Key1"));
		assertTrue(lruCache.contains("Key4"));
		assertEquals("Value4",lruCache.get("Key4"));
		
		//Get Key 2
		lruCache.get("Key2");
		//Order is {2,4,3}
		
		//Put Key5 and drop LRU Key3
		lruCache.put("Key5","Value5");
		//Order is {5,2,4}
		assertFalse(lruCache.contains("Key3"));
		assertTrue(lruCache.contains("Key5"));
		assertEquals("Value5",lruCache.get("Key5"));
	}
	
	@Test
	void testClearIsEmpty() {
		
		int size = 2;
		LRUCache<String,String> lruCache = new LRUCache<String,String>(size);
		
		assertTrue(lruCache.isEmpty());
		lruCache.put("Key1","Value1");
		lruCache.put("Key2","Value2");
		assertFalse(lruCache.isEmpty());
		
		lruCache.clear();
		assertTrue(lruCache.isEmpty());
	}
}
