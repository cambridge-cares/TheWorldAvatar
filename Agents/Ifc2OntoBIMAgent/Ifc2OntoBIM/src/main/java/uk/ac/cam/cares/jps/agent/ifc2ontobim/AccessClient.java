package uk.ac.cam.cares.jps.agent.ifc2ontobim;

import org.apache.jena.rdfconnection.RDFConnection;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;

/**
 * A client that performs network requests such as sending POST request
 * to other urls or uploading a TTL file to a SPARQL endpoint.
 *
 * @author qhouyee
 */
class AccessClient {

    /**
     * Sends a POST request with requested parameters to the specified url.
     *
     * @param url        The specified url.
     * @param jsonParams Request parameters.
     */
    protected static void sendPostRequest(String url, String jsonParams) {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = null;
        try {
            request = HttpRequest.newBuilder()
                    .header("Content-Type", "application/json")
                    .uri(URI.create(url))
                    .POST(HttpRequest.BodyPublishers.ofString(jsonParams))
                    .timeout(Duration.ofSeconds(3600))
                    .build();
            // Await response before continue executing the rest of the code
            client.send(request, HttpResponse.BodyHandlers.ofString());
        } catch (IOException e) {
            throw new JPSRuntimeException(e.getMessage() + " If connection is refused, the url is likely invalid!");
        } catch (InterruptedException e) {
            throw new RuntimeException("Thread has been interrupted!" + e.getMessage());
        }
    }

    /**
     * Uploads TTL to a specified SPARQL url.
     *
     * @param endpoint The specified url.
     * @param ttlFile  File path to ttl file
     */
    protected static void uploadTTL(String endpoint, String ttlFile) {
        try (RDFConnection conn = RDFConnection.connect(endpoint)) {
            conn.load(ttlFile);
        }
    }
}
