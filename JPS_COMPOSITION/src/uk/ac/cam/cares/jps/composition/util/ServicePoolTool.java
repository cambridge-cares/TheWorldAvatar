package uk.ac.cam.cares.jps.composition.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import org.json.JSONObject;

public class ServicePoolTool {

	public static String filename = "service_pool.txt";
	public String fullHostName = "";

	public ServicePoolTool(String fullHostName) {
		this.fullHostName = fullHostName;
	}

	public JSONObject readTheServicePool() throws Exception {

		FilePathManager m = new FilePathManager();
		String filepath = m.getFilePath(this.fullHostName) + filename;
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
		FileWriter fw = new FileWriter(
				"/home/zhouxiaochi/Documents/JPS/JParkSimulator-git/JPS_COMPOSITION/WebContent/service_pool.txt");
		fw.write(content);
		fw.close();
	}

}
