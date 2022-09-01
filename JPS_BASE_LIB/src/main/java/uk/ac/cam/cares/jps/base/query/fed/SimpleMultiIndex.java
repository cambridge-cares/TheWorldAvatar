package uk.ac.cam.cares.jps.base.query.fed;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

/**
 * A very simple implementation of a multi-index with postings list. 
 * An index is a map from an index key (of type K) to a set of entries (of type V). 
 * The class can manage more than one index, e.g. an schema-level and an index-level index. 
 * Different indexes are accessed by different index names.
 *
 * @param <K> the type of the index, e.g. String for instance IRIs
 * @param <V> the type of the entries in the postings list, e.g. String for endpoint URLs
 */
public class SimpleMultiIndex<K,V> {

	private Map<String, Map<K, Set<V>>> indexNameMap = new HashMap<String, Map<K, Set<V>>>();
	
	public void add(String indexName, K key, V value) {
		Map<K, Set<V>> index = indexNameMap.get(indexName);
		if (index == null) {
			index = new HashMap<K, Set<V>>();
			indexNameMap.put(indexName, index);
		}
		Set<V> postingsList = index.get(key);
		if (postingsList == null) {
			postingsList = new TreeSet<V>();
			index.put(key, postingsList);
		}
		postingsList.add(value);
	}
	
	public Map<K, Set<V>> getIndex(String indexName) {
		return indexNameMap.get(indexName);
	}
	
	public Set<V> getPostingsList(String indexName, K key) {
		Map<K, Set<V>> index = getIndex(indexName);
		if (index == null) {
			return null;
		}
		return index.get(key);
	}
}
