package uk.ac.cam.cares.jps.base.cache;

import java.util.LinkedHashMap;
import java.util.Map;

import uk.ac.cam.cares.jps.base.interfaces.CacheInterface;

public class LRUCache<K,V> implements CacheInterface<K,V>{

	private final int capacity;
	private Map<K,V> cache;
	
	//TODO set default value?
	
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
	public boolean set(K key, V value) {
		cache.put(key, value);
		return false;
	}

	@Override
	public V get(K key) {	
		return cache.getOrDefault(key, null);
	}

	@Override
	public boolean isEmpty() {
		return cache.size()==0;
	}

	@Override
	public void clear() {
		cache.clear();
	}
}
