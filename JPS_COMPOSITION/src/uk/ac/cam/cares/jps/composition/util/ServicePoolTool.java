package uk.ac.cam.cares.jps.composition.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.AgentLocator;

public class ServicePoolTool {

	public static String filename = "service_pool.txt";
	public String fullHostName = "";

	public ServicePoolTool(String fullHostName) {
		this.fullHostName = fullHostName;
	}

	public JSONObject readTheServicePool() throws Exception {

		FilePathManager m = new FilePathManager();
		String filepath = m.getFilePath(this.fullHostName) + '/' +filename;
		if(m.getFilePath(this.fullHostName) == null) { // It means the system is running without a server
			filepath = "E:" + filename;
		}
		String wholeContent = "";
		File thefile = new File(filepath);
		if (!thefile.exists()) {
			writeToTheServicePool("");
		}

		try (BufferedReader br = new BufferedReader(new FileReader(filepath))) {
			String sCurrentLine;
			while ((sCurrentLine = br.readLine()) != null) {
				wholeContent = wholeContent + sCurrentLine;
			}
		} catch (IOException e) {
			e.printStackTrace();
		}

		if (wholeContent == null || wholeContent == "") {
			return new JSONObject();
		} else {
			return new JSONObject(wholeContent);
		}
	}

	public void writeToTheServicePool(String content) throws Exception {
		
		FilePathManager m = new FilePathManager();
		String filepath = m.getFilePath(this.fullHostName) + '/' +filename;
		FileWriter fw = new FileWriter(filepath);
		fw.write(content);
		fw.close();
	}

}
