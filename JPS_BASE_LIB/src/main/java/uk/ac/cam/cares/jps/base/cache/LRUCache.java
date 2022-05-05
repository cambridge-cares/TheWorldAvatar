package uk.ac.cam.cares.jps.base.cache;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

import uk.ac.cam.cares.jps.base.interfaces.CacheInterface;

/**
 * Least Recently Used (LRU) Cache implementation of 
 * {@link uk.ac.cam.cares.jps.base.interfaces.CacheInterface CacheInterface}
 * using a LinkedHashMap
 *
 * @author csl37
 *
 * @param <K>
 * @param <V>
 */
public class LRUCache<K,V> implements CacheInterface<K,V>{
	
    //Default value to return if there is no mapping
    private final V DEFAULT_VALUE = null;
    
	private final int capacity;
	private Map<K,V> cache;
	
	//Constructor
	public LRUCache(int size) {
		this.capacity = size;
		this.cache = Collections.synchronizedMap(
			new LinkedHashMap<K,V>(size, 0.75f, true){
				@Override
				protected boolean removeEldestEntry(Map.Entry eldest) {
					return size() > capacity;
				}
			});
	}
	
	/**
	 * Put key-value pair into cache.
	 */
	@Override
	public V put(K key, V value) {
		return cache.put(key, value);
	}

	/**
	 * Get value for supplied key. If the key does not exist,
	 * return the default value: null.
	 */
	@Override
	public V get(K key) {	
		return cache.getOrDefault(key, DEFAULT_VALUE);
	}
	
	/**
	 * Check if the cache contains a key.
	 */
	@Override
	public boolean contains(K key) {
		return cache.containsKey(key);
	}
	
	/**
	 * Check if the cache is empty.
	 */
	@Override
	public boolean isEmpty() {
		return cache.size()==0;
	}

	/**
	 * Clear the cache.
	 */
	@Override
	public void clear() {
		cache.clear();
	}
	
	/**
	 * Get the max capacity of the cache.
	 */
	@Override
	public int capacity() {
		return capacity;
	}
}
