package uk.ac.cam.cares.jps.base.util.test;

import static org.junit.Assert.*;

import org.junit.Test;
import java.util.List;
import java.util.Map;
import java.util.Arrays;
import java.util.HashMap;

import uk.ac.cam.cares.jps.base.util.MatrixConverter;

public class MatrixConverterTest {
	
	// Put list to map and get list 
	@Test
	public void testColumn() {
		
		List<Object> testList = Arrays.asList("A","B","C");
		
		MatrixConverter testMC = new MatrixConverter();
		testMC.putColumn("testKey", testList);
		
		assertEquals(testList, testMC.getList("testKey"));
	}
	
	// Put list to map and convert to Json
	@Test
	public void testToJson() {
		
		List<Object> testList = Arrays.asList("A","B","C");
		String expectedJson = "{\"testKey\":[\"A\",\"B\",\"C\"]}";
		
		MatrixConverter testMC = new MatrixConverter();
		testMC.putColumn("testKey", testList);
		
		String result = testMC.toJson();
		
		assertEquals(expectedJson,result);
	}
	
	// Json to map
	@Test
	public void testFromJson() {
		Map<String, List<String>> expectedMap = new HashMap<String, List<String>>();
		List<String> testList = Arrays.asList("A","B","C");
		expectedMap.put("testKey", testList);
		
		String testSJson = "{\"testKey\":[\"A\",\"B\",\"C\"]}";
		MatrixConverter testMC = new MatrixConverter();
		
		assertEquals(expectedMap, testMC.fromJson(testSJson));
	}
	
	// CSV to map
	@Test
	public void testFromCsv() {
		
		String testCSVString = "Column1,Column2\r\nData11,Data12\r\nData21,Data22\r\n";
				
		Map<String, List<String>> expectedMap = new HashMap<String, List<String>>();
		List<String> column1 = Arrays.asList("Data11","Data21");
		List<String> column2 = Arrays.asList("Data12","Data22");
		expectedMap.put("Column1", column1);
		expectedMap.put("Column2", column2);
		
		Map<String, List<String>> resultMap = MatrixConverter.fromCsv(testCSVString);
		
		assertEquals(expectedMap, resultMap);
	}
	
	// Array to CSV
	@Test	
	public void testFromArraytoCsv() {
		
		String testCSVString = "Column1,Column2\r\nData11,Data12\r\nData21,Data22\r\n";
		
		List<String[]> testRows = Arrays.asList(new String[]{"Column1", "Column2"},new String[]{"Data11", "Data12"},new String[]{"Data21","Data22"});
		
		assertEquals(testCSVString, MatrixConverter.fromArraytoCsv(testRows));
	}
	
	// CSV to array
	@Test
	public void testFromCsvToArray() {
		
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
	
	// List to array
	@Test
	public void testAsArray() {
		List<String> testList = Arrays.asList("A","B","C");
		String[] expectedArray = new String[] {"A","B","C"};
		
		assertArrayEquals(expectedArray, MatrixConverter.asArray(testList));
 	}
}