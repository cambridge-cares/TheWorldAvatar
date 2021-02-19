package uk.ac.cam.cares.jps.base.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import com.google.gson.Gson;

public class MatrixConverter {

	private Map<String, List<Object>> map = new HashMap<String, List<Object>>();
	
	public void putColumn(String key, List<Object> list) {
		map.put(key, list);
	}
	
	public String toJson() {
		return new Gson().toJson(map);
	}
	
	@SuppressWarnings("unchecked")
	public Map<String, List<Object>> fromJson(String s) {
		 map = new Gson().fromJson(s, HashMap.class);
		 return map;
	}
	
	public List<Object> getList(String key) {
		return map.get(key);
	}
	
	/**
	 * This method removes all chars '\r' from the given string, 
	 * splits it into several rows according to '\n'. The first rows
	 * is expected to contain the column names. Finally, the columns are extracted
	 * from all other rows. 
	 * 
	 * @param s
	 * @return
	 */
	public static Map<String, List<String>> fromCsv(String s) {
		Map<String, List<String>> smap = new HashMap<String, List<String>>();
		
		String sClean = s.replace("\r", "");
		StringTokenizer tokenizer = new StringTokenizer(sClean, "\n");
		List<String> columnNames = fromCsvRow(tokenizer.nextToken());
		
		List<List<String>> rows = new ArrayList<List<String>>();
		while (tokenizer.hasMoreTokens()) {
			rows.add(fromCsvRow(tokenizer.nextToken()));
		}
		
		for (int columnIndex = 0; columnIndex < columnNames.size(); columnIndex++) {
			List<String> column = new ArrayList<String>();
			for (List<String> currentRow : rows) {
				column.add(currentRow.get(columnIndex));
			}
			smap.put(columnNames.get(columnIndex), column);
		}
		
		return smap;
	}
	
	private static List<String> fromCsvRow(String s) {
		List<String> result = new ArrayList<String>();
		StringTokenizer tokenizer = new StringTokenizer(s, ",");
		while (tokenizer.hasMoreTokens()) {
			result.add(tokenizer.nextToken());
		}
		return result;
	}
	
	public static String fromArraytoCsv(List<String[]> rows) {
		
		StringBuffer b = new StringBuffer();
		for (String[] current : rows) {
			appendRow(b, current);
		}
		return b.toString();
	}
	
	private static void appendRow(StringBuffer b, String[] row) {
		int length = row.length;
		for (int i=0; i<length; i++) {
			b.append(row[i]);
			if (i<length-1) {
				b.append(",");
			} else {
				b.append("\r\n");
			}
		}
	}
	
	public static List<String[]> fromCsvToArray(String s) {
		
		List<String[]> result = new ArrayList<String[]>();
		
		String sClean = s.replace("\r", "");
		StringTokenizer tokenizer = new StringTokenizer(sClean, "\n");
		
		while (tokenizer.hasMoreTokens()) {
			// Changed from String[] row = asArray(fromCsvRow(tokenizer.nextToken()));
			// Previously there is an issue if there are empty cells, e.g. two commas in a row
			// Note that this still does not work if there is a string with comma in between
			// e.g. ,"abc, abc",
			String[] row = tokenizer.nextToken().split(",");
			result.add(row);
		}
		
		return result;
	}
	
	public static String[] asArray(List<String> l) {
		String[] a = new String[l.size()];
		for (int i=0; i<l.size(); i++) {
			a[i] = l.get(i);
		}
		return a;
 	}
} 
