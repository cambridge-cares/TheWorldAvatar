package uk.ac.cam.cares.jps.base.query;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Base64;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.commons.io.FilenameUtils;
import org.apache.http.HttpEntity;
import org.apache.http.HttpHeaders;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.FileEntity;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.jdbc.JenaDriver;
import org.apache.jena.jdbc.remote.RemoteEndpointDriver;
import org.apache.jena.query.Query;
import org.apache.jena.query.TxnType;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdfconnection.RDFConnection;
import org.apache.jena.rdfconnection.RDFConnectionRemote;
import org.apache.jena.rdfconnection.RDFConnectionRemoteBuilder;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFLanguages;
import org.apache.jena.sparql.core.Var;
import org.apache.jena.update.UpdateRequest;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.federated.FedXFactory;
import org.eclipse.rdf4j.federated.endpoint.Endpoint;
import org.eclipse.rdf4j.federated.endpoint.EndpointFactory;
import org.eclipse.rdf4j.federated.repository.FedXRepository;
import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.TupleQuery;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.query.TupleQueryResultHandlerException;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;
import org.eclipse.rdf4j.repository.sparql.SPARQLRepository;
import org.json.JSONArray;
import org.json.JSONObject;

import com.google.common.base.Preconditions;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.TripleStoreClientInterface;
import uk.ac.cam.cares.jps.base.util.StoreClientHelper;

/**
 * This class allows to establish connection with remote knowledge
 * repositories to perform SPARQL query and update operations. It supports many
 * triple stores such as Blazegraph and RDF4J. It requires to set the end point
 * URL for the intended type of operation. See some example end point URLS:
 * <p>
 * Blazegraph query end point URL:
 * http://localhost:8080/blazegraph/namespace/kb/sparql
 * <p>
 * Note that this is for the namespace called "kb". If you have a different
 * namespace, e.g, "ontokin", replace "kb" with "ontokin" in the above URL
 * <p>
 * RDF4J query end point URL:
 * http://localhost:8080/rdf4j-server/repositories/ontospecies
 * <p>
 * Note that this is for the repository called ontospecies.
 * <p>
 * Namespace in Blazegraph and repository in RDF4J refer to the same thing.
 *
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class RemoteStoreClient implements TripleStoreClientInterface {

    public static class RemoteStore {
        private final String name;

        private String queryEndpoint;
        private String updateEndpoint;
        // Authentication properties
        private String userName;
        private String password;

        public RemoteStore(String name) {
            Preconditions.checkNotNull(name, "RemoteStore must have a name");
            this.name = name;
        }

        public RemoteStore(String name, String queryEndpoint) {
            this(name);
            this.queryEndpoint = queryEndpoint;
        }

        public RemoteStore(String name, String queryEndpoint, String updateEndpoint) {
            this(name, queryEndpoint);
            this.updateEndpoint = updateEndpoint;
        }

        public RemoteStore(String name, String queryEndpoint, String updateEndpoint, String userName, String password) {
            this(name, queryEndpoint, updateEndpoint);
            this.userName = userName;
            this.password = password;
        }

        public String getName() {
            return name;
        }

        public String getQueryEndpoint() {
            return queryEndpoint;
        }

        public void setQueryEndpoint(String queryEndpoint) {
            this.queryEndpoint = queryEndpoint;
        }

        public String getUpdateEndpoint() {
            return updateEndpoint;
        }

        public void setUpdateEndpoint(String updateEndpoint) {
            this.updateEndpoint = updateEndpoint;
        }

        public String getUserName() {
            return userName;
        }

        public void setUserName(String userName) {
            this.userName = userName;
        }

        public String getPassword() {
            return password;
        }

        public void setPassword(String password) {
            this.password = password;
        }

    }

    private static final Logger LOGGER = LogManager.getLogger(RemoteStoreClient.class);
    private static final String DEFAULT_REMOTE_STORE_NAME = "default";
    private static final String HTTP_PROTOCOL = "http:";

    private static final String HTTPS_PROTOCOL = "https:";

    private String query;

    Map<String, RemoteStore> remoteStores = new HashMap<>();
    {
        remoteStores.put(DEFAULT_REMOTE_STORE_NAME, new RemoteStore(DEFAULT_REMOTE_STORE_NAME));
    }

    ///////////////////////////
    // Constructors
    ///////////////////////////
    /**
     * The default constructor.
     */
    public RemoteStoreClient() {

    }

    /**
     * A constructor defined to initialise only the query EndPoint URL.
     *
     * @param queryEndpoint
     */
    public RemoteStoreClient(String queryEndpoint) {
        getDefaultRemoteStore().setQueryEndpoint(queryEndpoint);
    }

    /**
     * A constructor defined to initialise the query EndPoint URL, update EndPoint
     * URL and a set of graphs to send a data retrieval or update query.
     *
     * @param queryEndpoint
     * @param updateEndpoint
     */
    public RemoteStoreClient(String queryEndpoint, String updateEndpoint) {
        RemoteStore defaultRemoteStore = getDefaultRemoteStore();
        defaultRemoteStore.setQueryEndpoint(queryEndpoint);
        defaultRemoteStore.setUpdateEndpoint(updateEndpoint);
    }

    /**
     * A constructor defined to initialise the query EndPoint URL, user name and
     * password.
     *
     * @param queryEndpoint
     * @param user
     * @param password
     */
    public RemoteStoreClient(String queryEndpoint, String user, String password) {
        RemoteStore defaultRemoteStore = getDefaultRemoteStore();
        defaultRemoteStore.setQueryEndpoint(queryEndpoint);
        defaultRemoteStore.setUserName(user);
        defaultRemoteStore.setPassword(password);
    }

    /**
     * A constructor defined to initialise the query EndPoint URL, update EndPoint
     * URL, user name and password.
     *
     * @param queryEndpoint
     * @param updateEndpoint
     * @param user
     * @param password
     */
    public RemoteStoreClient(String queryEndpoint, String updateEndpoint, String user, String password) {
        RemoteStore defaultRemoteStore = getDefaultRemoteStore();
        defaultRemoteStore.setQueryEndpoint(queryEndpoint);
        defaultRemoteStore.setUpdateEndpoint(updateEndpoint);
        defaultRemoteStore.setUserName(user);
        defaultRemoteStore.setPassword(password);
    }

    /**
     * A constructor defined to initialise the query EndPoint URL, update EndPoint
     * URL, user name and password and a data retrieval or update query.
     *
     * @param queryEndpoint
     * @param updateEndpoint
     * @param query
     * @param user
     * @param password
     */
    public RemoteStoreClient(String queryEndpoint, String updateEndpoint, String query, String user, String password) {
        this(queryEndpoint, updateEndpoint, user, password);
        this.query = query;
    }

    /**
     * Returns the available query.
     *
     * @return
     */
    @Override
    public String getQuery() {
        return query;
    }

    /**
     * Sets a query if provided.
     *
     * @param query
     * @return
     */
    @Override
    public String setQuery(String query) {
        this.query = query;
        return this.query;
    }

    /**
     * Can return the URL of the query EndPoint.
     *
     * @return
     */
    @Override
    public String getQueryEndpoint() {
        return getDefaultRemoteStore().getQueryEndpoint();
    }

    /**
     * Sets the URL of the query EndPoint if provided.
     *
     * @param queryEndpoint
     * @return
     */
    @Override
    public String setQueryEndpoint(String queryEndpoint) {
        getDefaultRemoteStore().setQueryEndpoint(queryEndpoint);
        return queryEndpoint;
    }

    /**
     * Returns the URL of the update EndPoint if available.
     *
     * @return
     */
    @Override
    public String getUpdateEndpoint() {
        return getDefaultRemoteStore().getUpdateEndpoint();
    }

    /**
     * Set the URL of the update EndPoint if provided.
     *
     * @param updateEndpoint
     * @return
     */
    @Override
    public String setUpdateEndpoint(String updateEndpoint) {
        getDefaultRemoteStore().setUpdateEndpoint(updateEndpoint);
        return updateEndpoint;
    }

    /**
     * Returns the user name to access the current EndPoint.
     *
     * @return
     */
    @Override
    public String getUser() {
        return getDefaultRemoteStore().getUserName();
    }

    /**
     * Sets the user name to access the current EndPoint.
     *
     * @param userName
     */
    @Override
    public void setUser(String userName) {
        getDefaultRemoteStore().setUserName(userName);
    }

    /**
     * Returns the user password to access the current EndPoint.
     *
     * @return
     */
    @Override
    public String getPassword() {
        return getDefaultRemoteStore().getPassword();
    }

    /**
     * Sets the user password to access the current EndPoint.
     *
     * @param password
     */
    @Override
    public void setPassword(String password) {
        getDefaultRemoteStore().setPassword(password);
    }

    /**
     * This should be used with the
     * {@link #executeFederatedQuery(String)} for federated
     * queries where one or more of the endpoints requires credentials.
     * 
     * @param remoteStore
     */
    public void addRemoteStore(RemoteStore remoteStore) {
        remoteStores.put(remoteStore.getName(), remoteStore);
    }

    /**
     * Detects if the provided SPARQL update endpoint is blazegraph backended.
     * 
     * @return
     */
    public boolean isUpdateEndpointBlazegraphBackended() {
        if (!Objects.isNull(this.getUpdateEndpoint())) {
            try {
                // normalise the URI before checking if it contains the "/blazegraph/namespace/"
                // so that this works on both Linux and Windows OS
                if (new URI(this.getUpdateEndpoint().toLowerCase()).normalize().getPath()
                        .contains("/blazegraph/namespace/")) {
                    return true;
                }
            } catch (URISyntaxException e) {
                e.printStackTrace();
            }
        }
        return false;
    }

    ///////////////////////////
    // Sparql query and update
    ///////////////////////////
    /**
     * Executes the update operation that is provided through the constructors or
     * setter method.
     *
     * @return
     */
    @Override
    public int executeUpdate() {
        return executeUpdate(this.query);
    }

    /**
     * Executes the update operation supplied by the calling method and returns
     * results.
     *
     * @param update as UpdateRequest
     * @return
     */
    @Override
    public int executeUpdate(UpdateRequest update) {
        return executeUpdate(update.toString());
    }

    /**
     * Executes the update operation supplied by the calling method and returns
     * results.
     *
     * @param query
     * @return
     */
    @Override
    public int executeUpdate(String query) {
        try (Connection conn = getConnection()) {
            return executeUpdate(query, conn);
        } catch (SQLException e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }
    }

    /**
     * Executes the update operation supplied by the calling method and returns
     * results.
     *
     * @param query
     * @param conn  connection to the remote store
     * @return
     */
    public int executeUpdate(String query, Connection conn) {
        try (Statement stmt = conn.createStatement(java.sql.ResultSet.TYPE_FORWARD_ONLY,
                java.sql.ResultSet.CONCUR_READ_ONLY)) {
            return stmt.executeUpdate(query);
        } catch (SQLException e) {
            throw new JPSRuntimeException(
                    "RemoteStoreClient: error when executing SPARQL update:\n" + query, e);
        }
    }

    /**
     * Executes the update operation by HTTP POST and returns the HttpResponse
     * instance. This method was tested for blazegraph and rdf4j triple store. The
     * entity of response from blazegraph endpoint will be something look like:
     * <p>
     * {@code COMMIT: totalElapsed=157ms, commitTime=1651761749071, mutationCount=1}
     * <p>
     * So one can parse this string to determine the amount of triples got updated
     * by regex matching "{@code mutationCount=([0-9]+)}".
     * 
     * However, the rdf4j endpoint will only return HTTP 204 No Content status -
     * there is no way to know if the SPARQL update actually changed any triples.
     * 
     * @param query
     * 
     * @return
     */
    public CloseableHttpResponse executeUpdateByPost(String query) {
        HttpEntity entity = new StringEntity(query, ContentType.create("application/sparql-update"));
        return uploadByPostEntity(entity, "SPARQL update");
    }

    /**
     * Execute sparql query using the query variable
     *
     * @return JSONArray as String
     */
    @Override
    public String execute() {
        return execute(this.query);
    }

    /**
     * Execute sparql query
     *
     * @param query
     * @return JSONArray as String
     */
    @Override
    public String execute(String query) {
        JSONArray result = executeQuery(query);
        if (result == null) {
            throw new JPSRuntimeException("RemoteStoreClient: sparql query result is null.");
        } else {
            return result.toString();
        }
    }

    /**
     * Executes the query that is provided through the constructors or setter
     * method.
     *
     * @return
     */
    @Override
    public JSONArray executeQuery() {
        return executeQuery(this.query);
    }

    /**
     * Executes the query supplied by the calling method and returns results
     * as a JSONArray.
     *
     * @param query
     * @return
     */
    @Override
    public JSONArray executeQuery(String query) {
        try (Connection conn = getConnection()) {
            return executeQuery(query, conn);
        } catch (SQLException e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }
    }

    /**
     * Executes the query supplied by the calling method and returns results
     * as a JSONArray.
     *
     * @param query
     * @param conn  connection to the remote store
     * @return
     */
    public JSONArray executeQuery(String query, Connection conn) {
        try (Statement stmt = conn.createStatement(java.sql.ResultSet.TYPE_FORWARD_ONLY,
                java.sql.ResultSet.CONCUR_READ_ONLY)) {
            java.sql.ResultSet rs = stmt.executeQuery(query);
            return StoreClientHelper.convert(rs);
        } catch (Exception e) {
            throw new JPSRuntimeException(
                    "RemoteStoreClient: error when executing SPARQL query:\n" + query, e);
        }
    }

    /**
     * Perform a sparql construct query
     *
     * @return RDF model
     */
    @Override
    public Model executeConstruct(Query sparql) {
        return executeConstruct(sparql.toString());
    }

    /**
     * Perform a sparql construct query
     *
     * @return RDF model
     */
    @Override
    public Model executeConstruct(String sparql) {

        RDFConnection conn = connectQuery();

        if (conn != null) {
            conn.begin(TxnType.READ);
            try {
                return conn.queryConstruct(sparql);
            } finally {
                conn.end();
            }
        } else {
            throw new JPSRuntimeException("RemoteStoreClient: client not initialised.");
        }
    }

    /**
     * Generates the URL of the remote data repository's EndPoint, which might
     * require authentication either to perform a data retrieval or an update query.
     *
     * @return
     */
    public String getConnectionUrl() {
        StringBuilder sb = new StringBuilder();
        boolean queryFlag = false;
        boolean updateFlag = false;
        sb.append(JenaDriver.DRIVER_PREFIX);
        sb.append(RemoteEndpointDriver.REMOTE_DRIVER_PREFIX);

        String queryEndpoint = getQueryEndpoint();
        String updateEndpoint = getUpdateEndpoint();
        String userName = getUser();
        String password = getPassword();

        if (queryEndpoint != null) {
            queryFlag = true;
            sb.append(generateEndpointProperty(RemoteEndpointDriver.PARAM_QUERY_ENDPOINT, queryEndpoint));
        }
        if (updateEndpoint != null) {
            updateFlag = true;
            if (queryFlag) {
                sb.append("&");
            }
            sb.append(generateEndpointProperty(RemoteEndpointDriver.PARAM_UPDATE_ENDPOINT, updateEndpoint));
        }
        if (userName != null) {
            if (queryFlag || updateFlag) {
                sb.append("&");
            }
            sb.append(generateEndpointProperty(RemoteEndpointDriver.PARAM_USERNAME, userName));
        }
        if (password != null) {
            if (queryFlag || updateFlag) {
                sb.append("&");
            }
            sb.append(generateEndpointProperty(RemoteEndpointDriver.PARAM_PASSWORD, password));
        }
        return sb.toString();
    }

    /**
     * Establish connection to remote store
     * 
     * @return connection object to the remote store
     * @throws SQLException
     */
    public Connection getConnection() throws SQLException {
        String connectionUrl = getConnectionUrl();
        if (connectionUrl.isEmpty()) {
            throw new JPSRuntimeException("RemoteStoreClient: the URL to connect to the endpoint is empty");
        }
        if (isConnectionQueryUrlValid(connectionUrl) || isConnectionUpdateUrlValid(connectionUrl)) {
            RemoteEndpointDriver.register();
            return DriverManager.getConnection(connectionUrl);
        } else {
            throw new JPSRuntimeException("RemoteStoreClient: the URL to connect to the endpoint is not valid");
        }
    }

    /**
     * Checks the validity of the URL generated for connecting to a remote
     * repository based on user provided inputs via one of the parameterised
     * constructors or setter methods.
     *
     * @param connectionUrl provided URL that is to be used for establishing
     *                      a connection with the remote repository to perform a
     *                      query operation
     */
    public boolean isConnectionQueryUrlValid(String connectionUrl) {
        if (connectionUrl.startsWith(getQueryEndpointConnectionPrefixes()
                + HTTP_PROTOCOL)
                || connectionUrl.startsWith(getQueryEndpointConnectionPrefixes()
                        + HTTPS_PROTOCOL)) {
            return isConnectionUrlValid(connectionUrl);
        }
        return false;
    }

    /**
     * Checks the validity of the URL generated for connecting to a remote
     * repository based on user provided inputs via one of the parametrised
     * constructors or setter methods.
     *
     * @param connectionUrl provided URL that is to be used for establishing
     *                      a connection with the remote repository to perform an
     *                      update operation
     * @return
     */
    public boolean isConnectionUpdateUrlValid(String connectionUrl) {
        // One might set both the query and update endpoint URLs, but calls
        // the executeUpdate() method. This is why it is crucial to check
        // if the provided update URL is correctly encoded.
        if (isConnectionQueryUrlValid(connectionUrl)) {
            if (connectionUrl.contains(getUpdateEndpointConnectionParameter() + HTTP_PROTOCOL)
                    || connectionUrl.contains(getUpdateEndpointConnectionParameter() + HTTPS_PROTOCOL)) {
                return true;
            }
        }
        if (connectionUrl.startsWith(getUpdateEndpointConnectionPrefixes() + HTTP_PROTOCOL)
                || connectionUrl.startsWith(getUpdateEndpointConnectionPrefixes() + HTTPS_PROTOCOL)) {
            return isConnectionUrlValid(connectionUrl);
        }
        return false;
    }

    /**
     * If a list of SPARQL endpoints and a query are passed to this method, it
     * evaluates the query against all the endpoints and returns the result in JSON
     * format. The endpoints must not require credentials, if the do use the
     * {@link #executeFederatedQuery(String)} method instead.
     * <br>
     * Endpoints should be provided as shown in the following two examples:
     * <br>
     * 1. OntoSpecies KB endpoint:
     * http://www.theworldavatar.com/blazegraph/namespace/ontospecies/sparql
     * </br>
     * 2. OntoCompChem KB endpoint:
     * http://www.theworldavatar.com/blazegraph/namespace/ontocompchem/sparql
     *
     * @param endpoints a list of endpoints.
     * @param query     a SPARQL query.
     * @return the query results.
     * @throws Exception
     */
    public JSONArray executeFederatedQuery(List<String> endpoints, String query) {
        return executeFederatedQuery(query,
                endpoints.stream().map(EndpointFactory::loadSPARQLEndpoint).collect(Collectors.toList()));
    }

    /**
     * Use the {@link #addRemoteStore(RemoteStore)} method and this
     * one if your endpoints might have credentials.
     * 
     * @param query a SPARQL query.
     * @return the query results.
     */
    public JSONArray executeFederatedQuery(String query) {
        List<Endpoint> endpoints = remoteStores.values().stream()
                .filter(remoteStore -> null != remoteStore.getQueryEndpoint())
                .map(remoteStore -> {
                    Endpoint endpoint = EndpointFactory.loadSPARQLEndpoint(remoteStore.getName(),
                            remoteStore.getQueryEndpoint());
                    String userName = remoteStore.getUserName();
                    String password = remoteStore.getPassword();
                    if (null != userName && null != password) {
                        ((SPARQLRepository) endpoint.getRepository()).setUsernameAndPassword(userName, password);
                    }
                    return endpoint;
                }).collect(Collectors.toList());

        return executeFederatedQuery(query, endpoints);
    }

    /**
     * Get rdf content from store. Performs a construct query on the store and
     * returns the model as a string.
     *
     * @param resourceUrl (if any)
     * @param accept
     * @return String
     */
    @Override
    public String get(String resourceUrl, String accept) {

        Var varS = Var.alloc("s");
        Var varP = Var.alloc("p");
        Var varO = Var.alloc("o");

        ConstructBuilder builder = new ConstructBuilder()
                .addConstruct(varS, varP, varO);

        if (resourceUrl == null) {
            // Default graph
            builder.addWhere(varS, varP, varO);
        } else {
            // Named graph
            String graphURI = "<" + resourceUrl + ">";
            builder.addGraph(graphURI, varS, varP, varO);
        }

        Model model = executeConstruct(builder.build());

        Lang syntax;
        if (accept != null) {
            syntax = RDFLanguages.contentTypeToLang(accept);
        } else {
            // default to application/rdf+xml
            syntax = Lang.RDFXML;
        }

        StringWriter out = new StringWriter();
        model.write(out, syntax.getName());
        return out.toString();
    }

    /**
     * Insert rdf content into store.
     *
     * @param graphName   (if any)
     * @param content
     * @param contentType
     */
    @Override
    public void insert(String graphName, String content, String contentType) {

        Model model = ModelFactory.createDefaultModel();

        InputStream in = new ByteArrayInputStream(content.getBytes());
        if (contentType == null) {
            // RDF/XML default
            // base=null, assume all uri are absolute
            model.read(in, null);
        } else {
            Lang syntax = RDFLanguages.contentTypeToLang(contentType);
            model.read(in, null, syntax.getName());
        }

        UpdateBuilder builder = new UpdateBuilder();

        if (graphName == null) {
            builder.addInsert(model);
        } else {
            String graphURI = "<" + graphName + ">";
            builder.addInsert(graphURI, model);
        }

        executeUpdate(builder.buildRequest());
    }

    /**
     * Upload a file to the endpoint using its REST API.
     * Only tested the xml format with Blazegraph and RDF4j, this may vary between
     * stores. The update endpoint for rdf4j is
     * <p>
     * {@code <BASE_HTTP_URL\>/rdf4j-server/repositories/<REPOSITORY_NAME>/statements}.
     * <p>
     * The query and update endpoints for Blazegraph are the same.
     * Accepted extensions: rdf, rdfs, owl, xml, nt, ntx, ttl, ttlx, n3, trix, trig,
     * nq, rsj, json
     *
     * @param file
     */
    public void uploadFile(File file) {
        String extension = FilenameUtils.getExtension(file.getAbsolutePath());
        uploadFile(file, extension);
    }

    /**
     * Same method as above but receives extension as a separate argument
     * 
     * @param file
     * @param extension
     */
    public void uploadFile(File file, String extension) {
        if (!file.exists()) {
            throw new JPSRuntimeException("Provided file does not exist " + file.getAbsolutePath());
        }

        ContentType contentType = getRDFContentType(extension);
        if (null == contentType) {
            throw new JPSRuntimeException("Unsupported file extension: " + extension);
        }
        FileEntity entity = new FileEntity(file, contentType);

        uploadByPostEntity(entity, "Upload RDF file");

    }

    /**
     * Upload triple in the form of string
     * 
     * @param triples
     */

    @Override
    public void uploadTriples(String triples) {
        uploadTriples(triples, "ttl");
    }

    /**
     * Same method as above but receives extension as a separate argument
     * 
     * @param triples
     * @param extension
     */
    @Override
    public void uploadTriples(String triples, String extension) {
        HttpEntity entity = new StringEntity(triples, getRDFContentType(extension));
        uploadByPostEntity(entity, "Upload triples");
    }

    CloseableHttpResponse uploadByPostEntity(HttpEntity entity, String description) {
        String updateEndpoint = getUpdateEndpoint();
        String userName = getUser();
        String password = getPassword();

        // tried a few methods to add credentials, this seems to be the only way that
        // works
        // i.e. setting it manually in the header
        HttpPost postRequest = new HttpPost(updateEndpoint);
        if ((userName != null) && (password != null)) {
            String auth = userName + ":" + password;
            String encodedAuth = Base64.getEncoder().encodeToString(auth.getBytes());
            postRequest.setHeader(HttpHeaders.AUTHORIZATION, "Basic " + encodedAuth);
        }

        // add contents to the post request
        postRequest.setEntity(entity);

        LOGGER.info("{} to {}", description, updateEndpoint);
        // then send the post request
        try (CloseableHttpClient httpClient = HttpClients.createDefault()) {
            // the returned CloseableHttpResponse should be handled with try-with-resources
            CloseableHttpResponse response = httpClient.execute(postRequest);
            int statusCode = response.getStatusLine().getStatusCode();
            if (statusCode < 200 || statusCode > 300) {
                throw new JPSRuntimeException(
                        description + " failed. Response status code = " + statusCode);
            }
            return response;
        } catch (IOException e) {
            throw new JPSRuntimeException(description + " failed", e);
        }
    }

    public ContentType getRDFContentType(String extension) {
        final ContentType contentType;
        switch (extension) {
            case "rdf":
            case "rdfs":
            case "owl":
            case "xml":
                contentType = ContentType.create("application/rdf+xml");
                break;
            case "nt":
                contentType = ContentType.TEXT_PLAIN;
                break;
            case "ntx":
                contentType = ContentType.create("application/x-n-triples-RDR");
                break;
            case "ttl":
                contentType = ContentType.create("application/x-turtle");
                break;
            case "ttlx":
                contentType = ContentType.create("application/x-turtle-RDR");
                break;
            case "n3":
                contentType = ContentType.create("text/rdf+n3");
                break;
            case "trix":
                contentType = ContentType.create("application/trix");
                break;
            case "trig":
                contentType = ContentType.create("application/x-trig");
                break;
            case "nq":
                contentType = ContentType.create("text/x-nquads");
                break;
            case "srj":
                contentType = ContentType.create("application/sparql-results+json");
                break;
            case "json":
                contentType = ContentType.APPLICATION_JSON;
                break;
            default:
                contentType = null;
        }
        return contentType;
    }

    private RemoteStore getDefaultRemoteStore() {
        return remoteStores.get(DEFAULT_REMOTE_STORE_NAME);
    }

    /**
     * Return RDFConnection to sparql query endpoint
     *
     * @return
     */
    private RDFConnection connectQuery() {

        RDFConnectionRemoteBuilder builder = null;
        String queryEndpoint = getQueryEndpoint();
        if (queryEndpoint != null) {
            builder = RDFConnectionRemote.create().destination(queryEndpoint);
        } else {
            throw new JPSRuntimeException("RemoteStoreClient: update endpoint not specified.");
        }

        return builder.build();
    }

    /**
     * Puts the type of an endpoint (e.g. query and update), equal to symbol and end
     * point URL in a string and returns the string.
     *
     * @param endpointType
     * @param endpointURL
     * @return
     */
    private String generateEndpointProperty(String endpointType, String endpointURL) {
        StringBuilder sb = new StringBuilder();
        sb.append(endpointType);
        sb.append("=");
        sb.append(endpointURL);
        return sb.toString();
    }

    /**
     * Checks the validity of a connection URL.
     *
     * @param connectionUrl
     * @return
     */
    private boolean isConnectionUrlValid(String connectionUrl) {
        String[] tokens = new String[] { "" };
        if (connectionUrl.contains(HTTP_PROTOCOL)) {
            tokens = connectionUrl.split(HTTP_PROTOCOL);
        } else if (connectionUrl.contains(HTTPS_PROTOCOL)) {
            tokens = connectionUrl.split(HTTPS_PROTOCOL);
        }
        for (String token : tokens) {
            if (token.isEmpty()) {
                return false;
            }
            if (token.length() < 3) {
                return false;
            }
        }
        return true;
    }

    /**
     * Returns the connection prefix and query endpoint parameter literals
     * concatenated.
     *
     * @return
     */
    private String getQueryEndpointConnectionPrefixes() {
        return JenaDriver.DRIVER_PREFIX
                + RemoteEndpointDriver.REMOTE_DRIVER_PREFIX
                + RemoteEndpointDriver.PARAM_QUERY_ENDPOINT
                + "=";
    }

    /**
     * Returns the connection prefix and update endpoint parameter literals
     * concatenated.
     *
     * @return
     */
    private String getUpdateEndpointConnectionPrefixes() {
        return JenaDriver.DRIVER_PREFIX
                + RemoteEndpointDriver.REMOTE_DRIVER_PREFIX
                + RemoteEndpointDriver.PARAM_UPDATE_ENDPOINT
                + "=";
    }

    /**
     * Returns the update endpoint parameter.
     *
     * @return
     */
    private String getUpdateEndpointConnectionParameter() {
        return RemoteEndpointDriver.PARAM_UPDATE_ENDPOINT
                + "=";
    }

    private JSONArray executeFederatedQuery(String query, List<Endpoint> endpoints) {

        // Creates a federation with all provided endpoints.
        FedXRepository repository = FedXFactory.createFederation(endpoints);

        JSONArray json = new JSONArray();
        JSONObject obj;
        BindingSet bSet;
        try ( // Establishes a connection with all the endpoints.
                RepositoryConnection conn = repository.getConnection()) {

            // Prepares the query for execution against all the endpoints.
            TupleQuery tq = conn.prepareTupleQuery(query);

            // Evaluates the query against all the endpoints.
            TupleQueryResult tqRes = tq.evaluate();
            // Processes the result
            while (tqRes.hasNext()) {
                obj = new JSONObject();
                bSet = tqRes.next();
                for (String bindingName : bSet.getBindingNames()) {
                    // If the value of a variable provided in the SPARQL query is found
                    // within double quotes, they are removed as the JSON Object also adds
                    // double quotes to the value.
                    if (bSet.getValue(bindingName).toString().startsWith("\"")
                            && bSet.getValue(bindingName).toString().endsWith("\"")) {
                        obj.put(bindingName, bSet.getValue(bindingName).toString().substring(1,
                                bSet.getValue(bindingName).toString().length() - 1));
                    } // A value found without double quotes is codified as a JSON Object
                      // without modification.
                    else {
                        obj.put(bindingName, bSet.getValue(bindingName).toString());

                    }
                }
                json.put(obj);
            }
            return json;
        } catch (TupleQueryResultHandlerException | RepositoryException ex) {
            throw new RuntimeException(ex);
        } finally {
            repository.shutDown();
        }
    }
}
