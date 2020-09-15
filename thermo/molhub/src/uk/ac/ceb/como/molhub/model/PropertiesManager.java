package uk.ac.ceb.como.molhub.model;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

/**
 * The Properties Manager class. Implements method for reading data from .properties files.
 * 
 * @author Nenad Krdzavac (caresssd@hermes.cam.ac.uk)
 *  @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class PropertiesManager {

	public static void main(String[] args) {

	}

	/**
	 * @author NK510
	 * @param inputStream the Input Stream.
	 * @return Properties (key, value) given in .properties file of molhub project
	 */
	public static Properties loadProperties(InputStream inputStream) {

		Properties properties = new Properties();

		try {

			if (inputStream == null) {

				return properties;
			}

			// load a properties file from class path.
			properties.load(inputStream);

		} catch (IOException ex) {

			ex.printStackTrace();

		} finally {

			if (inputStream != null) {

				try {

					inputStream.close();

				} catch (IOException e) {

					e.printStackTrace();

				}
			}
		}

		return properties;

	}

}