package com.cmclinnovations.ship;

import java.net.URISyntaxException;

import org.apache.http.client.utils.URIBuilder;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class Config {
    // directory containing data files
    public static String DATA_DIR = System.getenv("DATA_DIR");

	// file containing last read file name
	public static String LAST_READ_FILE = System.getenv("LAST_READ_FILE");

    // postgres for time series
    public static String POSTGRES_URL;
    static String POSTGRES_HOST = System.getenv("POSTGRES_HOST");
    static String POSTGRES_PORT = System.getenv("POSTGRES_PORT");
    static String POSTGRES_DB = System.getenv("POSTGRES_DB");
    static String POSTGRES_USER = System.getenv("POSTGRES_USER");
    static String POSTGRES_PASSWORD = System.getenv("POSTGRES_PASSWORD");

    // kg
    public static String KG_URL;
    static String KG_HOST = System.getenv("KG_HOST");
    static String KG_PORT = System.getenv("KG_PORT");
    static String KG_PATH = System.getenv("KG_PATH");
    static String KG_PROTOCOL = System.getenv("KG_PROTOCOL");

    private static final Logger LOGGER = LogManager.getLogger(Config.class);
    
    public static void initURLs() {
        // KG URL
		if (KG_HOST != null && KG_PATH != null && KG_PROTOCOL != null) {
			URIBuilder kgurl_builder = new URIBuilder();
			kgurl_builder.setHost(KG_HOST);
			kgurl_builder.setPath(KG_PATH);
			kgurl_builder.setScheme(KG_PROTOCOL);
			if (KG_PORT != null) {
				kgurl_builder.setPort(Integer.parseInt(KG_PORT));
			}
			try {
				KG_URL = kgurl_builder.build().toString();
				LOGGER.info("Constructed KG_URL = " + KG_URL);
			} catch (URISyntaxException e) {
				LOGGER.error("Error building KG_URL");
				LOGGER.error(e.getMessage());
				throw new RuntimeException(e);
			}
		} else {
			String errmsg = "KG_HOST, KG_PATH and KG_PROTOCOL must be present as environment variables";
			LOGGER.error(errmsg);
			throw new RuntimeException(errmsg);
		}

        // POSTGRES
		if (POSTGRES_DB != null) {
			if (POSTGRES_USER == null || POSTGRES_PASSWORD == null) {
				throw new RuntimeException("The environment variable POSTGRES_USER/POSTGRES_PASSWORD is not set");
			}
			if (POSTGRES_HOST != null) {
				if (POSTGRES_PORT != null) {
					POSTGRES_URL = "jdbc:postgresql://" + POSTGRES_HOST + ":" + POSTGRES_PORT + "/" + POSTGRES_DB;
				} else {
					POSTGRES_URL = "jdbc:postgresql://" + POSTGRES_HOST + "/" + POSTGRES_DB;
				}
			} else {
				POSTGRES_URL = "jdbc:postgresql:" + POSTGRES_DB;
			}

			LOGGER.info("Time series will be queried from " + POSTGRES_URL);
		} else {
			String errmsg = "The environment variable TIMESERIES_DB is not set";
			LOGGER.error(errmsg);
			throw new RuntimeException(errmsg);
		}
    }
}
