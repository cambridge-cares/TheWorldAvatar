package uk.ac.cam.cares.jps.scenario;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.util.FileUtil;

public class ScenarioLog {

	public class ScenarioLogEntry implements Comparable {
		public String scenario = null;
		public String timestamp = null;
		public JSONObject message = null;
		
		@Override
		public int compareTo(Object o) {
			return timestamp.compareTo(((ScenarioLogEntry) o).timestamp);
		}
	}
	
	private static SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS-z");
	private List<ScenarioLogEntry> entries = new ArrayList<ScenarioLog.ScenarioLogEntry>();
	private String filePath = null;
	
	public ScenarioLog(String scenarioName) {
		create(scenarioName);
	}
	
	public ScenarioLog(String scenarioName, String filePath) {
		this.filePath = filePath;
		
		if (!new File(filePath).exists()) {
			create(scenarioName);
		} else {
			String content = getLogAsString();
			read(content);
		}
	}
		
	public void create(String scenarioName) {
		JSONObject message = new JSONObject().put("extendsagent", "http://www.theworldavatar.com/kb/agents/Service__ScenarioAgent.owl#Service");
		addMessage(scenarioName, message);
		write();
	}
	
	public void read(String json) {	
		getEntries().clear();
		JSONObject jo = new JSONObject(json);
		JSONArray joarray = jo.getJSONArray("entries");
		for (int i=0; i<joarray.length(); i++) {
			ScenarioLogEntry entry = new ScenarioLogEntry();
			entry.scenario = joarray.getJSONObject(i).getString("scenario");
			entry.timestamp = joarray.getJSONObject(i).getString("timestamp");
			entry.message = joarray.getJSONObject(i).getJSONObject("message");
			entries.add(entry);
		}
	}

	public String getLogAsString() {
		return FileUtil.readFileLocally(filePath);
	}

	
	private void addMessage(String scenarioName, JSONObject message) {
		ScenarioLogEntry entry = new ScenarioLogEntry();
		entry.scenario = scenarioName;
		Date date = new Date(System.currentTimeMillis());
		entry.timestamp = sdf.format(date);;
		entry.message = message;
		entries.add(entry);
	}
	
	private void write() {
		if (filePath != null) {
			String content = toJson().toString();
			FileUtil.writeFileLocally(filePath, content);
		}
	}
	
	public void logMessage(String scenarioName, JSONObject message) {
		addMessage(scenarioName, message);
		write();
	}
	
	public JSONObject toJson() {
		JSONArray joarray = new JSONArray();
		for (ScenarioLogEntry current : entries) {
			JSONObject joentry = new JSONObject();
			joentry.put("scenario", current.scenario);
			joentry.put("timestamp", current.timestamp);
			joentry.put("message", current.message);
			joarray.put(joentry);
		}
		
		JSONObject jo = new JSONObject().put("entries",joarray);
		return jo;
	}
	
	public List<ScenarioLogEntry> getEntries() {
		return entries;
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
	
	/**
	 * Adds and merge entry and copies all entries from sourceLog into the scenario log.
	 * 
	 * @param sourceLog
	 */
	public void merge(ScenarioLog sourceLog) {
		JSONObject message = new JSONObject();
		message.put("operation", "merge");
		List<ScenarioLogEntry> mergeEntries = sourceLog.getEntries();
		message.put("mergedscenario", mergeEntries.get(0).scenario);
		String thisScenario = entries.get(0).scenario;
		logMessage(thisScenario, message);
		
		entries.addAll(mergeEntries);
		write();
	}
}
