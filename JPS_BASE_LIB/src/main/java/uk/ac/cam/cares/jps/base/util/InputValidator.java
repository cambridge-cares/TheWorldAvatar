package uk.ac.cam.cares.jps.base.util;

public class InputValidator {

	public InputValidator() {
		// TODO Auto-generated constructor stub
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
}
