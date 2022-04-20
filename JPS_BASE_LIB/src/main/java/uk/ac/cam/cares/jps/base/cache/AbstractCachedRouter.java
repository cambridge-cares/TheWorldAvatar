package uk.ac.cam.cares.jps.base.cache;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.interfaces.CacheInterface;

/**
 * This defines an abstract CachedRouter class. 
 * The extending class must initialize the cache in 
 * its constructor and implement the getFromStore 
 * method to fetch data from the router triple store.
 * The get method should be used to retrieve routing data.
 * @author csl37
 *
 * @param <K> key type
 * @param <V> value type
 */
public abstract class AbstractCachedRouter<K, V> {

	private static final Logger LOGGER = LogManager.getLogger(AbstractCachedRouter.class);
	
	private CacheInterface<K, V> cache;
	
	public AbstractCachedRouter(CacheInterface<K,V> cache){
		this.cache = cache;		
	}

	/**
	 * Get value mapped to the specified key. If the key is not in the cache 
	 * then get the value from the triple store and add it to the cache.
	 * @param key
	 * @return value
	 */
	public V get(K key) {
		
		V value;
		if(!cache.contains(key)) {
			LOGGER.info("Key= "+key.toString()+" not in cache. Get from store.");
			value = getFromStore(key);
			cache.put(key, value);
		}else {
			LOGGER.info("Key= "+key.toString()+" found in cache.");
			value = cache.get(key);
		}
		LOGGER.info("Key= "+key.toString()+", Value="+value.toString());
		return value;
	}
	
	abstract protected V getFromStore(K key);	
}
