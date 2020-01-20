package uk.ac.cam.ceb.como.paper.enthalpy.utils;

import java.io.UnsupportedEncodingException;
import java.util.Date;
import java.util.UUID;

public class FolderUtils {

	/**
	 * 
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param fileName a String that represents file name.
	 * @return String as a unique folder name.
	 * @throws UnsupportedEncodingException the exception.
	 * 
	 */
	public String generateUniqueFolderName(String fileName) throws UnsupportedEncodingException {

		long milliseconds = System.currentTimeMillis();

		String datetime = new Date().toString();

		datetime = datetime.replace(" ", "");
		datetime = datetime.replace(":", "");

		/**
		 * 
		 * @author nk510 (caresssd@hermes.cam.ac.uk)
		 * 
		 *         Generates source for universally unique identifier (uuid) based on
		 *         file name, date, time, and cpu milliseconds.
		 * 
		 */

		String source = fileName + datetime + milliseconds;

		byte[] bytes = source.getBytes("UTF-8");

		UUID uuid = UUID.nameUUIDFromBytes(bytes);

		return uuid.toString();

	}

}