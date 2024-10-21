package uk.ac.cam.cares;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.time.Instant;
import java.util.Iterator;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.auth0.jwt.JWT;
import com.auth0.jwt.interfaces.DecodedJWT;

@WebServlet(urlPatterns = { "/" })
public class GeoServerJwtProxy extends HttpServlet {
    private static final Logger LOGGER = LogManager.getLogger(GeoServerJwtProxy.class);
    private static final String TARGET_URL = "http://" + System.getenv("STACK_NAME") + "-geoserver:8080";

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        String userId = null;
        Iterator<String> headerIterator = request.getHeaders("Authorization").asIterator();
        while (headerIterator.hasNext() && userId == null) {
            String headerValue = headerIterator.next();
            if (headerValue.startsWith("Bearer ")) {
                DecodedJWT decodedJWT = JWT.decode(headerValue.substring(7));
                if (decodedJWT.getExpiresAtAsInstant().isAfter(Instant.now())) {
                    userId = decodedJWT.getSubject();
                }
            }
        }

        if (userId == null) {
            String errmsg = "Authorization header with Bearer not found";
            LOGGER.error(errmsg);
            throw new RuntimeException(errmsg);
        }

        // Build the URL to forward the request
        String targetUrl = TARGET_URL + request.getServletPath() + "?" + request.getQueryString();

        URIBuilder uriBuilder;
        try {
            uriBuilder = new URIBuilder(targetUrl);
        } catch (URISyntaxException e) {
            String errmsg = "Failed to build targetUrl";
            LOGGER.error(errmsg);
            throw new RuntimeException(errmsg, e);
        }

        String viewParamName = System.getenv("VIEWPARAM_NAME");
        if (userId != null && viewParamName != null) {
            uriBuilder.addParameter("viewparams", String.format("%s:%s", viewParamName, userId));
        }

        URI uri;
        try {
            uri = uriBuilder.build();
        } catch (URISyntaxException e) {
            throw new RuntimeException("Failed to build URI", e);
        }
        // Create a connection to the target URL
        HttpGet get = new HttpGet(uri);

        CloseableHttpClient httpClient = HttpClients.createDefault();

        try (CloseableHttpResponse httpResponse = httpClient.execute(get)) {
            response.setStatus(httpResponse.getStatusLine().getStatusCode());

            // Copy headers
            for (org.apache.http.Header header : httpResponse.getAllHeaders()) {
                response.setHeader(header.getName(), header.getValue());
            }

            httpResponse.getEntity().getContent().transferTo(response.getOutputStream());
        }
    }
}
