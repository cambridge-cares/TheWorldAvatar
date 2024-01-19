package uk.ac.cam.cares.jps.agent.http;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;

public class Request {
	/**
	 * Enables to perform an HTTP get requests.
	 * 
	 * @param query
	 * @return
	 * @throws MalformedURLException
	 * @throws IOException
	 */
	public static String get(String query) throws MalformedURLException, IOException {
		URL httpURL = new URL(query);
		URLConnection httpURLConnection = httpURL.openConnection();
		BufferedReader in = new BufferedReader(new InputStreamReader(httpURLConnection.getInputStream()));
		String inputLine;
		String fileContent = "";
		while ((inputLine = in.readLine())!= null) {
			fileContent = fileContent.concat(inputLine);
		}
		in.close();
		System.out.println("fileContent:\n"+fileContent);
		return fileContent;
	}
	
}
