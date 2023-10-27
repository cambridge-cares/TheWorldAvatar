package uk.ac.cam.cares.jps.base.util;

import static org.junit.Assert.*;

import org.junit.Test;
import java.util.List;
import java.util.Map;
import java.util.Arrays;
import java.util.HashMap;

import uk.ac.cam.cares.jps.base.util.MatrixConverter;

/**
 * This class contains unit tests for MatrixConverter.
 * 
 * @author CSL
 *
 */
public class MatrixConverterTest {
	 
	/**
	 * Tests putColumn and getList methods.
	 * Put a list to map and then return the list.
	 */
	@Test
	public void testColumn() {
		
		List<Object> testList = Arrays.asList("A","B","C");
		
		MatrixConverter testMC = new MatrixConverter();
		testMC.putColumn("testKey", testList);
		
		assertEquals(testList, testMC.getList("testKey"));
	}
	
	/**
	 * Test conversion to JSON by toJson method.
	 * Put a list to map and convert to Json.
	 */
	@Test
	public void testToJson() {
		
		List<Object> testList = Arrays.asList("A","B","C");
		String expectedJson = "{\"testKey\":[\"A\",\"B\",\"C\"]}";
		
		MatrixConverter testMC = new MatrixConverter();
		testMC.putColumn("testKey", testList);
		
		String result = testMC.toJson();
		
		assertEquals(expectedJson,result);
	}
	
	/**
	 * Test conversion from JSON to Map.
	 */
	@Test
	public void testFromJson() {
		
		// expected result
		Map<String, List<String>> expectedMap = new HashMap<String, List<String>>();
		List<String> testList = Arrays.asList("A","B","C");
		expectedMap.put("testKey", testList);
		
		String testSJson = "{\"testKey\":[\"A\",\"B\",\"C\"]}";
		MatrixConverter testMC = new MatrixConverter();
		
		assertEquals(expectedMap, testMC.fromJson(testSJson));
	}
	
	/**
	 * Test conversion of CSV string to Map.
	 */
	@Test
	public void testFromCsv() {
		
		String testCSVString = "Column1,Column2\r\nData11,Data12\r\nData21,Data22\r\n";
		
		// expected results
		Map<String, List<String>> expectedMap = new HashMap<String, List<String>>();
		List<String> column1 = Arrays.asList("Data11","Data21");
		List<String> column2 = Arrays.asList("Data12","Data22");
		expectedMap.put("Column1", column1);
		expectedMap.put("Column2", column2);
		
		Map<String, List<String>> resultMap = MatrixConverter.fromCsv(testCSVString);
		
		assertEquals(expectedMap, resultMap);
	}
	
	/**
	 * Test conversion of array to CSV.
	 */
	@Test	
	public void testFromArraytoCsv() {
		
		// expected result
		String testCSVString = "Column1,Column2\r\nData11,Data12\r\nData21,Data22\r\n";
		
		List<String[]> testRows = Arrays.asList(new String[]{"Column1", "Column2"},new String[]{"Data11", "Data12"},new String[]{"Data21","Data22"});
		
		assertEquals(testCSVString, MatrixConverter.fromArraytoCsv(testRows));
	}
	
	/**
	 * Test conversion of CSV string to array.
	 */
	@Test
	public void testFromCsvToArray() {
		
		// expected result
		List<String[]> testRows = Arrays.asList(new String[]{"Column1", "Column2"},new String[]{"Data11", "Data12"},new String[]{"Data21","Data22"});
		
		String testCSVString = "Column1,Column2\r\nData11,Data12\r\nData21,Data22\r\n";
		List<String[]> resultRows = MatrixConverter.fromCsvToArray(testCSVString);
		
		//assert both lists are the same length
		assertEquals(testRows.size(), resultRows.size());
		//then compare rows
		for(int i=0; i<testRows.size(); i++){
			assertArrayEquals(testRows.get(i),resultRows.get(i));
		}
	}
	
	/**
	 * Test conversion of list to array.
	 */
	@Test
	public void testAsArray() {
		List<String> testList = Arrays.asList("A","B","C");
		String[] expectedArray = new String[] {"A","B","C"};
		
		assertArrayEquals(expectedArray, MatrixConverter.asArray(testList));
 	}
}