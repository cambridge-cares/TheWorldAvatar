package uk.ac.cam.cares.jps.agent.trajectoryqueryagent;

import org.apache.http.client.utils.URIBuilder;
import java.net.URISyntaxException;
import java.net.MalformedURLException;
import java.util.concurrent.TimeUnit;

import com.auth0.jwk.JwkProvider;
import com.auth0.jwk.JwkProviderBuilder;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class JwkProviderSingleton {

    private static JwkProvider jwkProvider;
    private static final Logger LOGGER = LogManager.getLogger(JwkProviderSingleton.class);

    private JwkProviderSingleton() {
        throw new IllegalStateException("JwkProviderSingleton");
    }

    public static JwkProvider getInstance() {
        if (jwkProvider == null) {
            URIBuilder keyCloakBuilder;
            try {
                keyCloakBuilder = new URIBuilder(System.getenv("KEYCLOAK_SERVER"));
            } catch (URISyntaxException e) {
                String errmsg = "Failed to parse KEYCLOAK_SERVER";
                LOGGER.error(errmsg);
                LOGGER.error(e.getMessage());
                throw new RuntimeException(errmsg, e);
            }

            String existingPath;

            if (keyCloakBuilder.getPath() != null) {
                existingPath = keyCloakBuilder.getPath();
            } else {
                existingPath = "";
            }

            keyCloakBuilder.setPath(
                    existingPath + "/realms/" + System.getenv("KEYCLOAK_REALM") + "/protocol/openid-connect/certs");

            try {
                jwkProvider = new JwkProviderBuilder(keyCloakBuilder.build().toURL())
                        .cached(10, 24, TimeUnit.HOURS) // Caching keys for efficiency
                        .build();
            } catch (MalformedURLException | URISyntaxException e) {
                String errmsg = "Failed to build URL for JWK provider";
                LOGGER.error(e.getMessage());
                LOGGER.error(errmsg);
                throw new RuntimeException(errmsg, e);
            }
        }
        return jwkProvider;
    }
}
