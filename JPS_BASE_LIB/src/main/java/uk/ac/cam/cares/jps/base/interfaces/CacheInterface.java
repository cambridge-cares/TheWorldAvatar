package uk.ac.cam.cares.jps.base.interfaces;

/**
 * Interface for cache
 * @author csl37
 *
 * @param <K> key type
 * @param <V> value type
 */
public interface CacheInterface<K, V> {

	/**
	 * Shall implement logic to put key-value pair into the cache
	 * @param key
	 * @param value
	 * @return
	 */
	V put(K key, V value);
	
	/**
	 * Shall implement logic to get value mapped to key from the cache
	 * @param key
	 * @return
	 */
    V get(K key);
    
    /**
     * Shall implement logic to return true if the key is present in the cache
     * @param key
     * @return
     */
    boolean contains(K key);
    
    /**
     * Shall implement logic to return true if the cache is empty
     * @return
     */
    boolean isEmpty();
    
    /**
     * Shall implement logic to clear cache
     */
    void clear();
    
    /**
     * Shall implement logic to return the max cache capacity
     * @return
     */
    int capacity();
}