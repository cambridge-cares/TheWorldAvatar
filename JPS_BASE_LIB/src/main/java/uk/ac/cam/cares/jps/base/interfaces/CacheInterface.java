package uk.ac.cam.cares.jps.base.interfaces;

/**
 * Interface for cache
 * @author csl37
 *
 * @param <K> key type
 * @param <V> value type
 */
public interface CacheInterface<K, V> {

	V put(K key, V value);
    V get(K key);
    boolean contains(K key);
    boolean isEmpty();
    void clear();
    int capacity();
}