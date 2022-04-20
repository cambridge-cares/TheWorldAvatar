package uk.ac.cam.cares.jps.base.cache.test;

import static org.junit.jupiter.api.Assertions.*;

import java.lang.reflect.Field;

import org.junit.jupiter.api.Test;

import uk.ac.cam.cares.jps.base.cache.LRUCache;

class LRUCacheTest {

	@Test
	void testConstructor() throws NoSuchFieldException, SecurityException {
		
		int size = 100;
		LRUCache<String,String> lruCache = new LRUCache<String,String>(size);
		
		assertTrue(lruCache.isEmpty());
		assertEquals(size, lruCache.capacity());
		
		assertNotNull(lruCache.getClass().getDeclaredField("cache"));
        Field field = lruCache.getClass().getDeclaredField("cache");
        field.setAccessible(true);
        //TODO
	}
	
	@Test
	void testGet() {
		
	}

	@Test
	void testPut() {
		
	}
}
