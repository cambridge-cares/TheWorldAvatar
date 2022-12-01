package com.cmclinnovations.stack.clients.blazegraph;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import org.apache.commons.io.FilenameUtils;

import com.bigdata.rdf.sail.webapp.client.HttpException;
import com.bigdata.rdf.sail.webapp.client.RemoteRepositoryManager;
import com.cmclinnovations.stack.clients.core.ClientWithEndpoint;
import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.ontop.OntopEndpointConfig;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class BlazegraphClient extends ContainerClient implements ClientWithEndpoint {

    private static final Pattern SERVICE_IRI_PATTERN = Pattern.compile("SERVICE\\s*<([a-z]+)>",
            Pattern.CASE_INSENSITIVE);

    private static BlazegraphClient instance = null;

    public static BlazegraphClient getInstance() {
        if (null == instance) {
            instance = new BlazegraphClient();
        }
        return instance;
    }

    private final BlazegraphEndpointConfig blazegraphEndpoint;

    private BlazegraphClient() {
        blazegraphEndpoint = readEndpointConfig(EndpointNames.BLAZEGRAPH, BlazegraphEndpointConfig.class);
    }

    @Override
    public BlazegraphEndpointConfig getEndpoint() {
        return blazegraphEndpoint;
    }

    public void createNamespace(String namespace) {
        createNamespace(namespace, new Properties());
    }

    public void createNamespace(String namespace, Properties properties) {
        String serviceUrl = blazegraphEndpoint.getServiceUrl();
        try (RemoteRepositoryManager manager = new RemoteRepositoryManager(serviceUrl)) {
            manager.createRepository(namespace, properties);
        } catch (Exception ex) {
            if ((ex instanceof HttpException) && (409 == ((HttpException) ex).getStatusCode())) {
                // Namespace already exists error
            } else {
                throw new RuntimeException(
                        "Failed to create new namespace '" + namespace + "' at endpoint '" + serviceUrl + "'.", ex);
            }
        }
    }

    public void uploadRDFFiles(Path dirPath, String namespace) {
        String url = blazegraphEndpoint.getUrl(namespace);
        RemoteStoreClient remoteStoreClient = new RemoteStoreClient(url, url,
                blazegraphEndpoint.getUsername(),
                blazegraphEndpoint.getPassword());
        try (Stream<Path> files = Files.list(dirPath)) {
            files.filter(Files::isRegularFile)
                    .filter(file -> null != remoteStoreClient
                            .getRDFContentType(FilenameUtils.getExtension(file.toString())))
                    .forEach(path -> remoteStoreClient.uploadFile(path.toFile()));
        } catch (IOException ex) {
            throw new RuntimeException("Failed to load RDF files stored in the directory '" + dirPath + "'.", ex);
        }
    }

    /**
     * Method for replacing placeholders with real values
     * This is currently restricted to replacing endpoint names with their URL in
     * SPARQL SERVICE patterns, and specifically just for Ontop endpoints.
     * 
     * @param query query to be filtered
     * @return the query after the appropriate substitutions have been made
     */
    public String filterQuery(String query) {
        Matcher matcher = SERVICE_IRI_PATTERN.matcher(query);
        if (matcher.find()) {
            String serviceName = matcher.group(1).toLowerCase();
            return matcher.replaceAll("SERVICE <" +
                    readEndpointConfig(serviceName, OntopEndpointConfig.class).getUrl()
                    + ">");
        } else {
            return query;
        }
    }

}
