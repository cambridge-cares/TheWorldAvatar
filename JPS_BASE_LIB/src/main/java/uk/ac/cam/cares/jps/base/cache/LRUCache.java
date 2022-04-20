package uk.ac.cam.cares.jps.base.cache;

import java.util.LinkedHashMap;
import java.util.Map;

import uk.ac.cam.cares.jps.base.interfaces.CacheInterface;

/**
 * Least Recently Used (LRU) cache Implementation of CacheInterface\
 * using a LinkedHashMap
 * @author csl37
 *
 * @param <K>
 * @param <V>
 */
public class LRUCache<K,V> implements CacheInterface<K,V>{
	
    //Default value to return if there is no mapping
    private final V DEFAULT_VALUE = null;
    
	private final int capacity;
	private LinkedHashMap<K,V> cache;
	
	//Constructor
	public LRUCache(int size) {
		this.capacity = size;
		this.cache = new LinkedHashMap<K,V>(size, 0.75f, true){
			@Override
			protected boolean removeEldestEntry(Map.Entry eldest) {
				return size() > capacity;
			}
        };
	}
	
	@Override
	public V put(K key, V value) {
		return cache.put(key, value);
	}

	@Override
	public V get(K key) {	
		return cache.getOrDefault(key, DEFAULT_VALUE);
	}
	
	@Override
	public boolean contains(K key) {
		return cache.containsKey(key);
	}
	
	@Override
	public boolean isEmpty() {
		return cache.size()==0;
	}

	@Override
	public void clear() {
		cache.clear();
	}
	
	@Override
	public int capacity() {
		return capacity;
	}
}
