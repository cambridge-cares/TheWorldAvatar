package com.cmclinnovations.featureinfo;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;
import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.ontop.OntopEndpointConfig;

/**
 * Copied from some other agent, needs refactoring
 */
public class Config extends ContainerClient {

    private static final Logger LOGGER = LogManager.getLogger(Config.class);

	private static BlazegraphEndpointConfig blazegraphEndpointConfig;
	public static String bg_url;
	public static String bg_user;
	public static String bg_password;

    private static OntopEndpointConfig ontopEndpointConfig;
	public static String ot_url;
	public static String ot_user;
	public static String ot_password;
	
    private static boolean initialised = false;

	public  void initProperties() {

		if (!initialised) {
			try {

				blazegraphEndpointConfig = this.readEndpointConfig("blazegraph", BlazegraphEndpointConfig.class);
				Config.bg_url = blazegraphEndpointConfig.getUrl("kg");
				Config.bg_user = blazegraphEndpointConfig.getUsername();
				Config.bg_password = blazegraphEndpointConfig.getPassword();

                ontopEndpointConfig = this.readEndpointConfig("ontop", OntopEndpointConfig.class);
				Config.ot_url = ontopEndpointConfig.getUrl();
				Config.ot_user = ontopEndpointConfig.getUsername();
				Config.ot_password = ontopEndpointConfig.getPassword();

				initialised = true;
                
			} catch (Exception e) {
				LOGGER.error(e.getMessage());
			}
		}
	}
}