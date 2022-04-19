package uk.ac.cam.cares.jps.base.interfaces;

public interface CacheInterface<K, V> {

	boolean set(K key, V value);
    V get(K key);
    boolean isEmpty();
    void clear();
}