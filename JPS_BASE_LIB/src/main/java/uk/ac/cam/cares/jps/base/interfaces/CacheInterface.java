package uk.ac.cam.cares.jps.base.interfaces;

/**
 * Interface for cache
 * @author csl37
 *
 * @param <K>
 * @param <V>
 */
public interface CacheInterface<K, V> {

	V put(K key, V value);
    V get(K key);
    boolean isEmpty();
    void clear();
}