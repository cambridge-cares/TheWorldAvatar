package uk.ac.cam.cares.jps.scenario;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.query.QueryBroker;

public class ScenarioLog {

	public class ScenarioLogEntry implements Comparable {
		String timestamp = null;
		JSONObject message = null;
		
		@Override
		public int compareTo(Object o) {
			return timestamp.compareTo(((ScenarioLogEntry) o).timestamp);
		}
	}
	
	private static SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS-z");
	private List<ScenarioLogEntry> entries = new ArrayList<ScenarioLog.ScenarioLogEntry>();
	private String filePath = null;
	
	public ScenarioLog() {
		create();
	}
	
	public ScenarioLog(String filePath) {
		this.filePath = filePath;
		
		if (!new File(filePath).exists()) {
			create();
		} else {
			read();
		}
	}
	
	public void create() {
		JSONObject message = new JSONObject().put("extendsagent", "http://www.theworldavatar.com/kb/agents/Service__ScenarioAgent.owl#Service");
		addMessage(message);
		write();
	}
	
	private void read() {
		String content = new QueryBroker().readFile(filePath);	
		JSONObject jo = new JSONObject(content);
		JSONArray joarray = jo.getJSONArray("entries");
		for (int i=0; i<joarray.length(); i++) {
			ScenarioLogEntry entry = new ScenarioLogEntry();
			entry.timestamp = joarray.getJSONObject(i).getString("timestamp");
			entry.message = joarray.getJSONObject(i).getJSONObject("message");
			entries.add(entry);
		}
	}

	private void addMessage(JSONObject message) {
		ScenarioLogEntry entry = new ScenarioLogEntry();
		Date date = new Date(System.currentTimeMillis());
		entry.timestamp = sdf.format(date);;
		entry.message = message;
		entries.add(entry);
	}
	
	private void write() {
		if (filePath != null) {
			String content = toJson().toString();
			QueryBroker.writeFileLocally2(filePath, content);
		}
	}
	
	public void logMessage(JSONObject message) {
		addMessage(message);
		write();
	}
	
	public JSONObject toJson() {
		JSONArray joarray = new JSONArray();
		for (ScenarioLogEntry current : entries) {
			JSONObject joentry = new JSONObject();
			joentry.put("timestamp", current.timestamp);
			joentry.put("message", current.message);
			joarray.put(joentry);
		}
		
		JSONObject jo = new JSONObject().put("entries",joarray);
		return jo;
	}
	
	public List<ScenarioLogEntry> search(String key, String value) {
		List<ScenarioLogEntry> result = new ArrayList<ScenarioLogEntry>();
		
		for (ScenarioLogEntry current : entries) {
			if (current.message.has(key)) {
				if ((value == null) || value.equals(current.message.getString(key))) {
					result.add(current);
				} 
			}
		}
		
		Collections.sort(result);
		
		return result;
	}
}
