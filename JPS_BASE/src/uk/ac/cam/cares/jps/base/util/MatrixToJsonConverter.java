package uk.ac.cam.cares.jps.base.util;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.gson.Gson;

public class MatrixToJsonConverter {

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
}
