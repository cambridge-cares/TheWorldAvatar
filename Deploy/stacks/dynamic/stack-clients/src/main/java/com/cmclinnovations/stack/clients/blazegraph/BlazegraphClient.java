package com.cmclinnovations.stack.clients.blazegraph;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.List;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import org.apache.commons.io.FilenameUtils;
import org.eclipse.jetty.client.util.BasicAuthentication;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.bigdata.rdf.sail.webapp.client.AutoCloseHttpClient;
import com.bigdata.rdf.sail.webapp.client.HttpClientConfigurator;
import com.bigdata.rdf.sail.webapp.client.HttpException;
import com.bigdata.rdf.sail.webapp.client.RemoteRepositoryManager;
import com.cmclinnovations.stack.clients.core.ClientWithEndpoint;
import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.clients.core.datasets.CopyDatasetQuery;
import com.cmclinnovations.stack.clients.ontop.OntopEndpointConfig;
import com.cmclinnovations.stack.clients.utils.SparqlRulesFile;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class BlazegraphClient extends ClientWithEndpoint<BlazegraphEndpointConfig> {

    private static final Logger logger = LoggerFactory.getLogger(BlazegraphClient.class);

    private static final Pattern SERVICE_IRI_PATTERN = Pattern.compile("SERVICE\\s*<ontop>",
            Pattern.CASE_INSENSITIVE);

    private static BlazegraphClient instance = null;

    public static BlazegraphClient getInstance() {
        if (null == instance) {
            instance = new BlazegraphClient();
        }
        return instance;
    }

    private BlazegraphClient() {
        super(EndpointNames.BLAZEGRAPH, BlazegraphEndpointConfig.class);
    }

    public void createNamespace(String namespace) {
        createNamespace(namespace, new Properties());
    }

    public void createNamespace(String namespace, Properties properties) {
        sendCommandToBlazegraph(namespace, new CreateRepositoryCmd(namespace, properties));
    }

    public void removeNamespace(String namespace) {
        sendCommandToBlazegraph(namespace, new RemoveRepositoryCmd(namespace));
    }

    private void sendCommandToBlazegraph(String namespace, BaseCmd command) {
        BlazegraphEndpointConfig endpointConfig = readEndpointConfig();
        String serviceUrl = endpointConfig.getServiceUrl();

        try (AutoCloseHttpClient httpClient = (AutoCloseHttpClient) HttpClientConfigurator.getInstance()
                .newInstance()) {
            configureAuthentication(endpointConfig, serviceUrl, httpClient);
            callRemoteRepositoryManager(namespace, command, serviceUrl, httpClient);
        } catch (URISyntaxException ex) {
            throw new RuntimeException("The Blazegraph service URL '" + serviceUrl + "' is not a valid URI.", ex);
        } catch (Exception ex) {
            throw new RuntimeException(generateMessage(namespace, command, serviceUrl), ex);
        }
    }

    private void configureAuthentication(BlazegraphEndpointConfig endpointConfig, String serviceUrl,
            AutoCloseHttpClient httpClient) throws URISyntaxException {
        if (!endpointConfig.getPassword().isEmpty()) {
            // If authentication is enabled then create an HttpClient explicitly and add the
            // authentication details to its AuthenticationStore.

            httpClient.getAuthenticationStore()
                    .addAuthentication(new BasicAuthentication(new URI(serviceUrl), "Blazegraph",
                            endpointConfig.getUsername(), endpointConfig.getPassword()));
        }
    }

    private void callRemoteRepositoryManager(String namespace, BaseCmd command, String serviceUrl,
            AutoCloseHttpClient httpClient) throws Exception {
        try (RemoteRepositoryManager manager = new RemoteRepositoryManager(serviceUrl, httpClient, null)) {
            command.execute(manager);
        } catch (HttpException ex) {
            switch (ex.getStatusCode()) {
                case 409: // Namespace already exists error
                    logger.warn("Namespace '{}' already exists.", namespace);
                    break;
                case 404: // Namespace does not exist error
                    logger.warn("Namespace '{}' does not exist.", namespace);
                    break;
                default:
                    throw ex;
            }
        }
    }

    public void runRules(RemoteStoreClient remoteStoreClient, List<Path> ruleFiles) {
        SparqlRulesFile sparqlRules = new SparqlRulesFile(ruleFiles);
        sparqlRules.getRules().forEach(remoteStoreClient::executeUpdate);
    }

    private String generateMessage(String namespace, BaseCmd command, String serviceUrl) {
        return "Failed to " + command.getText() + " namespace '" + namespace + "' at endpoint '" + serviceUrl
                + "'.";
    }

    private abstract static class BaseCmd {

        private final String namespace;
        private final String text;

        BaseCmd(String namespace, String text) {
            this.namespace = namespace;
            this.text = text;
        }

        String getNamespace() {
            return namespace;
        }

        String getText() {
            return text;
        }

        abstract void execute(RemoteRepositoryManager manager) throws Exception;

    }

    private static class CreateRepositoryCmd extends BaseCmd {

        private final Properties properties;

        CreateRepositoryCmd(String namespace, Properties properties) {
            super(namespace, "create new");
            this.properties = properties;
        }

        void execute(RemoteRepositoryManager manager) throws Exception {
            manager.createRepository(getNamespace(), properties);

        }
    }

    private static class RemoveRepositoryCmd extends BaseCmd {

        RemoveRepositoryCmd(String namespace) {
            super(namespace, "remove");
        }

        void execute(RemoteRepositoryManager manager) throws Exception {
            manager.deleteRepository(getNamespace());

        }
    }

    public void uploadRDFFiles(Path dirPath, String namespace) {
        RemoteStoreClient remoteStoreClient = getRemoteStoreClient(namespace);
        try (Stream<Path> files = Files.list(dirPath)) {
            files.filter(Files::isRegularFile)
                    .filter(file -> null != remoteStoreClient
                            .getRDFContentType(FilenameUtils.getExtension(file.toString())))
                    .forEach(path -> remoteStoreClient.uploadFile(path.toFile()));
        } catch (IOException ex) {
            throw new RuntimeException("Failed to load RDF files stored in the directory '" + dirPath + "'.", ex);
        }
    }

    public RemoteStoreClient getRemoteStoreClient(String namespace) {
        BlazegraphEndpointConfig endpointConfig = readEndpointConfig();
        String url = endpointConfig.getUrl(namespace);
        return new RemoteStoreClient(url, url,
                endpointConfig.getUsername(),
                endpointConfig.getPassword());
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
        return filterQuery(query, readEndpointConfig("ontop", OntopEndpointConfig.class).getUrl());
    }

    public String filterQuery(String query, String ontopEndpoint) {
        Matcher matcher = SERVICE_IRI_PATTERN.matcher(query);
        if (matcher.find()) {
            return matcher.replaceAll("SERVICE <" + ontopEndpoint + ">");
        } else {
            return query;
        }
    }

    public void cloneDatasets(String targetNamespace, Collection<String> datasetNames, String catalogNamespace) {
        String catalogServiceURL = readEndpointConfig().getUrl(catalogNamespace);
        ModifyQuery query = CopyDatasetQuery.getInsertQuery(datasetNames, Rdf.iri(catalogServiceURL));
        getRemoteStoreClient(targetNamespace).executeUpdate(query.getQueryString());
    }
}
