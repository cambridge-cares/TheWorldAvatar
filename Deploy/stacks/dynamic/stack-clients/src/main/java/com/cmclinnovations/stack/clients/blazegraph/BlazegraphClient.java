package com.cmclinnovations.stack.clients.blazegraph;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Properties;
import java.util.stream.Stream;

import org.apache.commons.io.FilenameUtils;

import com.bigdata.rdf.sail.webapp.client.HttpException;
import com.bigdata.rdf.sail.webapp.client.RemoteRepositoryManager;
import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.clients.docker.ContainerClient;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class BlazegraphClient extends ContainerClient {

    private final BlazegraphEndpointConfig blazegraphEndpoint;

    private static BlazegraphClient instance = null;

    public static BlazegraphClient getInstance() {
        if (null == instance) {
            instance = new BlazegraphClient();
        }
        return instance;
    }

    private BlazegraphClient() {
        blazegraphEndpoint = readEndpointConfig(EndpointNames.BLAZEGRAPH, BlazegraphEndpointConfig.class);
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

}
