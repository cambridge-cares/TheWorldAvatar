package uk.ac.cam.cares;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Iterator;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.auth0.jwk.InvalidPublicKeyException;
import com.auth0.jwk.Jwk;
import com.auth0.jwk.JwkException;
import com.auth0.jwk.JwkProvider;
import com.auth0.jwt.JWT;
import com.auth0.jwt.interfaces.DecodedJWT;
import com.auth0.jwt.algorithms.Algorithm;
import com.auth0.jwt.exceptions.JWTVerificationException;
import com.auth0.jwt.interfaces.JWTVerifier;

import java.security.interfaces.RSAPublicKey;

@WebServlet(urlPatterns = { "/" })
public class GeoServerJwtProxy extends HttpServlet {
    private static final Logger LOGGER = LogManager.getLogger(GeoServerJwtProxy.class);
    private static final String TARGET_URL = "http://" + System.getenv("STACK_NAME") + "-geoserver:8080";

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        String token = null;

        Iterator<String> headerIterator = request.getHeaders("Authorization").asIterator();
        while (headerIterator.hasNext() && token == null) {
            String headerValue = headerIterator.next();
            if (headerValue.startsWith("Bearer ")) {
                token = headerValue.substring(7);
            }
        }

        if (token == null) {
            String errmsg = "Valid token cannot be obtained.";
            LOGGER.error(errmsg);
            throw new RuntimeException(errmsg);
        }

        DecodedJWT verifiedJwt = validateSignature(token);
        String userId = verifiedJwt.getSubject();

        // Build the URL to forward the request
        String targetUrl = TARGET_URL + request.getServletPath() + "?" + request.getQueryString();

        URIBuilder uriBuilder;
        try {
            uriBuilder = new URIBuilder(targetUrl);
        } catch (URISyntaxException e) {
            String errmsg = "Failed to initialise URIBuilder";
            LOGGER.error(errmsg);
            throw new RuntimeException(errmsg, e);
        }

        String viewParamName = System.getenv("VIEWPARAM_NAME");
        String viewParams = "";
        if (viewParamName != null) {
            String existingViewParams = request.getParameter("viewparams");
            if (existingViewParams != null) {
                if (existingViewParams.endsWith(";")) {
                    viewParams = existingViewParams + String.format("%s:%s", viewParamName, userId);
                } else {
                    viewParams = existingViewParams + ";" + String.format("%s:%s", viewParamName, userId);
                }
            } else {
                viewParams = String.format("%s:%s", viewParamName, userId);
            }
        }

        uriBuilder.setParameter("viewparams", viewParams);

        URI uri;
        try {
            uri = uriBuilder.build();
        } catch (URISyntaxException e) {
            throw new RuntimeException("Failed to build URI", e);
        }
        // Create a connection to the target URL
        HttpGet get = new HttpGet(uri);

        try (CloseableHttpClient httpClient = HttpClients.createDefault();
                CloseableHttpResponse httpResponse = httpClient.execute(get)) {
            response.setStatus(httpResponse.getStatusLine().getStatusCode());

            httpResponse.getEntity().getContent().transferTo(response.getOutputStream());
        }
    }

    private DecodedJWT validateSignature(String token) {
        // check signature using public key from KeyCloak server
        JwkProvider provider = JwkProviderSingleton.getInstance();

        DecodedJWT unverifiedDecodedJWT = JWT.decode(token);

        String keyId = unverifiedDecodedJWT.getKeyId();
        Jwk jwk;
        try {
            jwk = provider.get(keyId);
        } catch (JwkException e) {
            String errmsg = "Cannot find key ID from token";
            LOGGER.error(e.getMessage());
            LOGGER.error(errmsg);
            throw new RuntimeException(errmsg, e);
        }

        RSAPublicKey publicKey;
        try {
            publicKey = (RSAPublicKey) jwk.getPublicKey();
        } catch (InvalidPublicKeyException e) {
            String errmsg = "Cannot get public key from provider";
            LOGGER.error(e.getMessage());
            LOGGER.error(errmsg);
            throw new RuntimeException(errmsg, e);
        }

        Algorithm algorithm = Algorithm.RSA256(publicKey);

        // Create the JWT verifier
        JWTVerifier verifier = JWT.require(algorithm).build();

        DecodedJWT verifiedDecodedJWT;

        try {
            verifiedDecodedJWT = verifier.verify(unverifiedDecodedJWT);
        } catch (JWTVerificationException e) {
            String errmsg = "Failed to verify token";
            LOGGER.error(e.getMessage());
            LOGGER.error(errmsg);
            throw new RuntimeException(errmsg, e);
        }

        return verifiedDecodedJWT;
    }
}
