package com.cmclinnovations.featureinfo;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;
import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.ontop.OntopEndpointConfig;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;

/**
 * Copied from some other agent, needs refactoring
 */
public class Config {

    private static final Logger LOGGER = LogManager.getLogger(Config.class);

    private static BlazegraphEndpointConfig blazegraphEndpointConfig;
    public static String bg_url;
    public static String bg_user;
    public static String bg_password;

    private static OntopEndpointConfig ontopEndpointConfig;
    public static String ot_url;
    public static String ot_user;
    public static String ot_password;

    private static PostGISEndpointConfig postgisEndpointConfig;
    public static String pg_url;
    public static String pg_user;
    public static String pg_password;

    private static boolean initialised = false;

    private Config() {
    }

    static {
        if (!initialised) {
            ContainerClient client = new ContainerClient();

            blazegraphEndpointConfig = client.readEndpointConfig("blazegraph", BlazegraphEndpointConfig.class);
            Config.bg_url = blazegraphEndpointConfig.getUrl("kb");
            Config.bg_user = blazegraphEndpointConfig.getUsername();
            Config.bg_password = blazegraphEndpointConfig.getPassword();

            ontopEndpointConfig = client.readEndpointConfig("ontop", OntopEndpointConfig.class);
            Config.ot_url = ontopEndpointConfig.getUrl();
            Config.ot_user = ontopEndpointConfig.getUsername();
            Config.ot_password = ontopEndpointConfig.getPassword();

            postgisEndpointConfig = client.readEndpointConfig("postgis", PostGISEndpointConfig.class);
            Config.pg_url = postgisEndpointConfig.getJdbcURL("timeseries");
            Config.pg_user = postgisEndpointConfig.getUsername();
            Config.pg_password = postgisEndpointConfig.getPassword();

            initialised = true;

        }
    }
}