package uk.ac.cam.cares.jps.base.query.fed;

import static org.junit.jupiter.api.Assertions.assertIterableEquals;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;

import junit.framework.TestCase;

public class SimpleMultiIndexTest extends TestCase {
	
	public void testOneIndexWithThreePostingLists() {
		SimpleMultiIndex<String, Integer> index = new SimpleMultiIndex<String, Integer>();
		index.add("properties", "rdfs:label", 12);
		index.add("properties", "rdfs:label", 5);
		index.add("properties", "foaf:friend", 50);
		index.add("properties", "skos:broader", 500);
		index.add("properties", "rdfs:label", 4);
		index.add("properties", "foaf:friend", 40);
		index.add("properties", "skos:broader", 400);
		index.add("properties", "rdfs:label", 9);
		
		Map<String, Set<Integer>> propIndex = index.getIndex("properties");
		assertEquals(3, propIndex.keySet().size());
		Set<Integer> postingsList = index.getPostingsList("properties", "rdfs:label");
		assertEquals(4, postingsList.size());
		// check that the postings list is sorted by id
		List<Integer> expected = Arrays.asList( new Integer[] {4, 5, 9, 12});
		assertIterableEquals(expected, postingsList);
	}
}
