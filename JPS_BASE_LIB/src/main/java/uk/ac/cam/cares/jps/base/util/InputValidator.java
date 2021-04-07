package uk.ac.cam.cares.jps.base.util;
import java.io.File;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;

import org.apache.jena.query.Query;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.QueryParseException;
import org.apache.jena.riot.RiotException;
import org.apache.jena.riot.system.IRIResolver;
import org.apache.jena.update.UpdateException;
import org.apache.jena.update.UpdateFactory;
import org.apache.jena.update.UpdateRequest;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class InputValidator {

	public InputValidator() {
	}
	/** method to check if it's a valid true or false
	 * 
	 * @param boolCheck
	 * @return
	 */
	public static boolean checkBoolean(Object boolCheck) {
		String checkBool = boolCheck.toString().toLowerCase();
    	return "true".equals(checkBool) || "false".equals(checkBool);
	}
	/** because resolveIRI, checkIRI would return false 
	 * when IRI is valid, this is used to avoid confusion
	 * @param iri
	 * @return
	 */
	public static boolean checkIfValidIRI(String iriStr) {
		boolean f = true;
		try {
			//TODO-LO: There is something wrong with IRIResolver.checkIRI, because "abcd" passes just as well as irradiation sensor IRI
			f = IRIResolver.checkIRI(iriStr);
			
			}catch (RiotException ex) {
				throw new RiotException();
			}
			catch (Exception ex) {
				ex.printStackTrace();
			}
		return (!f& checkIfURLpattern(iriStr));
		}
	/** check if it fits a URL format
	 * 
	 * @param iriStr
	 * @return
	 */
	public static boolean checkIfURLpattern(String iriStr) {
		try {
			URL url = new URL(iriStr); 
			url.toURI(); 
			return true;
			} catch (MalformedURLException | URISyntaxException e) {
				return false;
				}
	}
	/** Check if String represents a file
	 * 
	 * @param filePath
	 * @return
	 */
	public static boolean checkIfFilePath(String filePath) {
		File file = new File(filePath);
		return file.isFile();
		
	}
	/** check if file exists in computer
	 * Can't be used if the directory is not established (aka created)
	 * @param iri
	 * @return
	 */
	public static boolean checkIfValidFile(String filePath) {
		File file = new File(filePath);
		return file.exists();
	}
	/** check if file was recently modified. 
	 * Second parameter should be time before simulation run
	 * 
	 * @param filePath location of file
	 * @param timeLast the previous time it was modified (if applicable)
	 * @return
	 */
	public static boolean checkIfFileGotUpdated(String filePath, long timeLast) {
		if (checkIfValidFile(filePath)) {
			
			File file = new File(filePath);
			long timeModified = file.lastModified();
			if (timeModified > timeLast ) {
				return true;
			}else return false;
		}return false;
	}
	/** checks if Integer by throwing Exception otherwise
	 * 
	 * @param str
	 * @return
	 */
	public static boolean checkIfInteger(String str)	 {
	     try{
	         Integer.parseInt(str);
	         return true;
	     }catch(NumberFormatException e){
	         return false;
	     }
	 }
	/** checks if JSONOBject by throwing Exception otherwise. 
	 * 
	 * @param str
	 * @return
	 */
	public static boolean checkIfValidJSONObject(String str) {
		try {
	        new JSONObject(str);
	    } catch (JSONException ex) {
	            return false;
	    }
	    return true;
	}
	/** checks if SPARQL Query by throwing Exception otherwise. 
	 * 
	 * @param str
	 * @return
	 */
	public static boolean checkIfValidQuery(String str) {
		try{
			Query query = QueryFactory.create(str);
			return true;
		}catch (QueryParseException e) {
			return false;
		}
	}
	/** checks if SPARQL Query by throwing Exception otherwise. 
	 * 
	 * @param str
	 * @return
	 */
	public static boolean checkIfValidUpdate(String str) {
		try{
			UpdateRequest update = UpdateFactory.create(str);
			return true;
		}catch (UpdateException e) {
			return false;
		}catch (QueryParseException e) {
			return false;
		}catch (Exception e) { //Still not sure what the updateException is called. 
			return false;
		}
	}
}
