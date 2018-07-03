package uk.ac.cam.cares.jps.composition.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import org.json.JSONException;
import org.json.JSONObject;

public class ServicePoolTool {
	
	public static final String filename = "/home/zhouxiaochi/Documents/service_pool.txt"; 
	
	public static JSONObject readTheServicePool() throws JSONException, IOException {
		String wholeContent = "";
		File thefile = new File(filename);
		if (!thefile.exists())
		{
			writeToTheServicePool("");
		}

		try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
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
	
	public static void writeToTheServicePool(String content) throws IOException {
		FileWriter fw = new FileWriter(filename);
		fw.write(content);
		fw.close();
	}
	
}
