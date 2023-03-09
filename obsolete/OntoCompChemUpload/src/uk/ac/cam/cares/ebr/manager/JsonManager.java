package uk.ac.cam.cares.ebr.manager;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.nio.charset.Charset;

import org.json.JSONException;
import org.json.JSONObject;

import com.jayway.jsonpath.JsonPath;

public class JsonManager {


	/**
	 * 
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param jsonUrl the URL of json file
	 * @return Json object
	 * @throws IOException
	 * @throws JSONException
	 * 
	 *                       Reads the content of json file from URL and prints it
	 *                       out on console or in browser.
	 * 
	 */

	public static JSONObject readsJsonFileFromUrl(String jsonUrl) throws IOException, JSONException {

		InputStream inputStream = new URL(jsonUrl).openStream();

		try {

			BufferedReader rd = new BufferedReader(new InputStreamReader(inputStream, Charset.forName("UTF-8")));

			StringBuilder stringBuilder = new StringBuilder();

			int line;

			while ((line = rd.read()) != -1) {

				stringBuilder.append((char) line);

			}

			JSONObject jsonObject = new JSONObject(stringBuilder.toString());

			return jsonObject;

		} finally {

			inputStream.close();
		}
	}
	
	/**
	 * These two method below are recommended by Feroz Farazi (msff2@cam.ac.uk)
	 * 
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 *
	 * Parses json content on http request.
	 */


	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param jsonString the content of http request
	 * @return the reference species folder path
	 */
		public static String getReferenceSpeciesFolderPath(String jsonString) {
			
			return JsonPath.read(jsonString, "$.referenceSpecies");
					
		}
		
		/**
		 * @author NK510 (caresssd@hermes.cam.ac.uk)
		 * 
		 * @param jsonString the content of http request.
		 * @return unique species IRI.
		 */
		public static String getSpeciesIRI(String jsonString) {
			
			return JsonPath.read(jsonString, "$.uniqueSpeciesIRI");
		}
		
	
}
