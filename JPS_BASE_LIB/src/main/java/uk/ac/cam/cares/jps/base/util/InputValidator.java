package uk.ac.cam.cares.jps.base.util;
import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.apache.jena.riot.RiotException;
import org.apache.jena.riot.system.IRIResolver;

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
			f = IRIResolver.checkIRI(iriStr);
			}catch (RiotException ex) {
				throw new RiotException();
			}
			catch (Exception ex) {
				ex.printStackTrace();
			}
		return !f;
		}
	
	
}
