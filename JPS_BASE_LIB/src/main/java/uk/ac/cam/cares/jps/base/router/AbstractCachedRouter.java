package uk.ac.cam.cares.jps.base.router;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.interfaces.CacheInterface;
import uk.ac.cam.cares.jps.base.interfaces.TripleStoreClientInterface;

/**
 * This abstract class defines a cached router. The extending router class must 
 * initialize the {@link uk.ac.cam.cares.jps.base.interfaces.CacheInterface cache} 
 * in its constructor and implement the 
 * {@link uk.ac.cam.cares.jps.base.router.AbstractCachedRouter#getStoreClient getStoreClient}
 * and {@link uk.ac.cam.cares.jps.base.router.AbstractCachedRouter#getFromStore getFromStore} 
 * methods to initialize the {@link  uk.ac.cam.cares.jps.base.interfaces.TripleStoreClientInterface StoreClient}
 * object and fetch data from the router triple store.
 * The {@link uk.ac.cam.cares.jps.base.router.AbstractCachedRouter#get get} method 
 * should be used to retrieve routing data e.g.
 * <pre>
 * cachedRouter.get(Key)
 * </pre>
 *
 * @see uk.ac.cam.cares.jps.base.interfaces.CacheInterface CacheInterface
 * @see uk.ac.cam.cares.jps.base.cache.LRUCache LRUCache
 * 
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
	 * Clear cache
	 */
	protected void clearCache() {
		LOGGER.info("Clearing cache!");
		cache.clear();
	}

	/**
	 * Get value mapped to the specified key. If the key is not in the cache 
	 * then get the value from the triple store and add it to the cache.
	 * 
	 * @param key
	 * 
	 * @return value
	 */
	public synchronized V get(K key) {
		
		V value;
		if(!cache.contains(key)) {
			LOGGER.info("Key= "+key.toString()+" not in cache. Get from store.");
			TripleStoreClientInterface storeClient = getStoreClient();
			value = getFromStore(key, storeClient);
			if(validate(value)) {
				cache.put(key, value);
			}
		}else {
			LOGGER.info("Key= "+key.toString()+" found in cache.");
			value = cache.get(key);
		}
		LOGGER.info("Key= "+key.toString()+", Value="+String.valueOf(value));
		return value;
	}
	
	/**
	 * Validate value returned from triple store.
	 * If TRUE, the key-value pair will be put into the cache.
	 * If FALSE, the key-value pair will NOT be put into the cache.
	 * Override this method for different behaviour. 
	 *  
	 * @param value
	 */
	protected boolean validate(V value) {
		if(value==null) {
			LOGGER.debug("Invalid value");
			return false;
		}else {
			return true;
		}
	}
	
	/**
	 * Extending class to implement logic for instantiating a 
	 * {@link  uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface StoreClient}
	 * 
	 * @return storeClient
	 */
	abstract public TripleStoreClientInterface getStoreClient();
	
	/**
	 * Extending class to implement logic for getting value(s) from triple store
	 * 
	 * @param key
	 * @param storeClient see {@link uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface StoreClientInterface} 
	 * 
	 * @return value
	 */
	abstract public V getFromStore(K key, TripleStoreClientInterface storeClient);
}
