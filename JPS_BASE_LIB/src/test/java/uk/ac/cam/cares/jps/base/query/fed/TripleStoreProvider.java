package uk.ac.cam.cares.jps.base.query.fed;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.apache.commons.io.FileUtils;
import org.apache.http.client.methods.HttpPut;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.testcontainers.containers.ContainerLaunchException;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.images.builder.ImageFromDockerfile;
import org.testcontainers.utility.DockerImageName;

import com.github.dockerjava.api.command.InspectContainerResponse;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.http.Http;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.query.fed.BlazegraphRepositoryWrapper;
import uk.ac.cam.cares.jps.base.util.FileUtil;

/**
 * This class creates multiple Docker test containers on-the-fly.
 * Each container hosts a triple store including SPARQL endpoints and RDF data. 
 * The created test environment can be used for integration tests for federated queries.
 * It allows to test various engines and configurations for executing federated SPARQL queries.
 * <p>
 * The class uses the org.testcontainers framework which allows to create containers easily.
 * The framework also provides 
 * <a href="https://www.testcontainers.org/features/networking/#advanced-networking">
 * communication between containers</a>.
 * However, it is not straight-forward to resolve a SPARQL endpoint URL 
 * which may appear in a SPARQL 1.1 SERVICE clause or in a SPARQL service description.
 * This may lead to HTTP 4xx errors if a request for a SPARQL endpoint is send 
 * from one container to another. The class solves this problem by converting localhost
 * IP addresses into Docker IP addresses.
 * For instance, the endpoint URL for Blazegraph has usually the following form: 
 * <pre>
 * http://localhost:8080/blazegraph/namespace/lab_1/sparql</pre>
 * 
 * where http://localhost:8080/blazegraph is the service URL and lab_1 is the namespace (dataset). The conversion would
 * replace localhost:8080 in the endpoint URL e.g. by 172.17.0.3:8080 which can be used internally for requests. 
 * <p>
 * The test environment can be extended by further containers (triple stores) and datasets by
 * extending the method {@link #init}. The test environment is created as singleton and the initialization is
 * started as soon as {@link #getInstance()} is called.
 */
public class TripleStoreProvider {
	
	/**
	 * A helper class that contains a created container and the SPARQL service URL. 
	 *
	 */
	private class ContainerInfo {
		
		GenericContainer<?> container = null;
		String serviceUrl = null;
		
		public ContainerInfo(GenericContainer<?> container, String serviceUrl) {
			this.container = container;
			this.serviceUrl = serviceUrl;
		}
	}
	
	/**
	 * A helper class that allows to construct the complete endpoint URL, e.g.
	 * <pre>
	 * http://localhost:54168/rdf4j-server/repositories/rdf4j_empty </pre>
	 * 
	 * In this example, http://localhost:54168/rdf4j-server is the service URL
	 * and /repositories/rdf4j_empty is the path.
	 */
	private class EndpointInfo {
		
		String containerId = null;
		String path = null;
		
		public EndpointInfo(String containerId, String path) {
			this.containerId = containerId;
			this.path = path;
		}
	}

	static final Logger LOGGER = LogManager.getLogger(TripleStoreProvider.class);
	public final static String DIR_TEST_RESOURCES = "./src/test/resources";
	
	// container IDs
	public final static String ID_BLAZEGRAPH_1 = "blazegraph_1";
	public final static String ID_BLAZEGRAPH_2 = "blazegraph_2";
	public final static String ID_RDF4J_1 = "rdf4j_1";
	
	// namespaces (i.e. datasets, repos, etc.)
	public final static String NAMESPACE_LAB_1 = "lab_1";
	public final static String NAMESPACE_LAB_2 = "lab_2";
	public final static String NAMESPACE_DOE_CHEMRXN = "doe_chemrxn";
	public final static String NAMESPACE_WIKIDATA_SMALL = "wikidata_small";
	public final static String NAMESPACE_BLAZEGRAPH_EMTPY = "blazegraph_empty";
	public final static String NAMESPACE_ONTOSPECIES = "ontospecies";
	public final static String NAMESPACE_ONTOCOMPCHEM = "ontocompchem";
	public static final String NAMESPACE_RDF4J_EMPTY = "rdf4j_empty";

	private static TripleStoreProvider instance = null;
	/**
	 * maps a container id to a container info object containing the created test container and the service URL
	 */
	private Map<String, ContainerInfo> id2container = new HashMap<String, ContainerInfo>();
	/**
	 * maps the namespace to an endpoint info object that allows to identify the triple store (container) and
	 * to construct the full endpoint URL
	 */
	private Map<String, EndpointInfo> namespace2endpoint = new HashMap<String, EndpointInfo>();
	/**
	 * host2host is used to convert host addresses; 
	 * conversion may necessary e.g. in a test environment with Docker containers where Blazegraph servers return endpoint URLs
	 * with wrong host addresses (such as "localhost:8080")  
	 */
	private Map<String, String> host2host = new HashMap<String, String>();
	
	private TripleStoreProvider() {
	}
	
	public synchronized static TripleStoreProvider getInstance() {
	    if (instance == null) {
	        instance = new TripleStoreProvider();
	        instance.init();
	    }
	    return instance;
	}
	
	/**
	 * Creates a container for a given image. If the image is not found in the local repository, 
	 * it is downloaded once. 
	 * <p>
	 * The image name "ghcr.io/cambridge-cares/blazegraph_for_tests:1.0.0" 
	 * requires access to the docker.cmclinnovations.com registry from the machine the test is run on.
	 * For this reason, method {@link #createBlazegraphContainerByBuilder(String)} is used instead in case of Blazegraph.
	 * For more information regarding the registry, 
	 * see {@link https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry}.
	 * 
	 * @param imageName
	 * @return
	 */
	private GenericContainer<?> createContainerByImage(String imageName) {
		return new GenericContainer<>(DockerImageName.parse(imageName));
	}
	
	/**
	 * Builds an image with a Blazegraph server by using the same commands as in the
	 * corresponding CMCL Docker file and returns a container for this image.
	 * 
	 * @param imageName if null image will be deleted after test; if not null the image can be reused when restarting the test 
	 * @return
	 */
	private GenericContainer<?> createBlazegraphContainerByBuilder(String imageName) {
		ImageFromDockerfile dockerfile = null;
		if (imageName == null) {
			dockerfile = new ImageFromDockerfile();
		} else {
			// the image is not deleted after test; this speeds up the setup when the test is restarted
			dockerfile = new ImageFromDockerfile(imageName, false);
		}
		
		dockerfile = dockerfile
		       .withDockerfileFromBuilder(builder -> builder
		               .from("tomcat:9.0-jre11-openjdk-slim-buster")
		               .add("https://github.com/blazegraph/database/releases/download/BLAZEGRAPH_2_1_6_RC/blazegraph.war", "/usr/local/tomcat/webapps/blazegraph.war")
		               .workDir("/data")
		               .build());
		
		return new GenericContainer<>(dockerfile);
	}
	
	/**
	 * Creates a container with a Blazegraph server. 
	 * 
	 * @param port exposed container port
	 * @return
	 */
	private GenericContainer<?> createBlazegraphTripleStore(int port) {
		String imageName = "blazegraphtest";
		GenericContainer<?> tripleStore = null;
		try {
			tripleStore = createContainerByImage(imageName).withExposedPorts(port);
			tripleStore.start();
			LOGGER.debug("Docker image was started, name=" + imageName + ", port=" + port);
		} catch (ContainerLaunchException exc) {
			LOGGER.debug("Docker image was not found, name=" + imageName + ", port=" + port);
			tripleStore = createBlazegraphContainerByBuilder(imageName).withExposedPorts(port);
			tripleStore.start();
			LOGGER.debug("Docker image was created and started, name=" + imageName + ", port=" + port);
		}
		return tripleStore;
	}
	
	/**
	 * Creates a container with an RDF4j server and workbench. 
	 * 
	 * @param port exposed container port
	 * @return
	 */
	private GenericContainer<?> createRDF4JServer(int port) {
		String imageName = "eclipse/rdf4j-workbench:3.7.7";
		GenericContainer<?> tripleStore = createContainerByImage(imageName).withExposedPorts(port);
		tripleStore.start();
		LOGGER.debug("Docker image was started, name=" + imageName + ", port=" + port);
		return tripleStore;
	}
	
	private String getLocalhost(GenericContainer<?> container) {
		return container.getHost() + ":" + container.getFirstMappedPort();
	}
	
	/**
	 * Returns the "IP:port" where IP it the IP address internally used for the container 
	 * in the default Docker network. 
	 * 
	 * @param container
	 * @param port
	 * @return
	 */
	private String getDockerIp(GenericContainer<?> container, int port) {
		InspectContainerResponse info = container.getContainerInfo();
		String ipAddress = info.getNetworkSettings().getIpAddress();
		return ipAddress + ":" + port; 
	}
	
	/**
	 * Returns the service URL for a given dataset (namespace)
	 * 
	 * @param namespace
	 * @return
	 */
	private static String getServiceUrlByNamespace(String namespace) {
		String id = getInstance().namespace2endpoint.get(namespace).containerId;
		return getInstance().id2container.get(id).serviceUrl;
	}
	
	public static String getServiceUrl(String containerId) {
		return getInstance().id2container.get(containerId).serviceUrl;
	}
	
	public static String getEndpointUrl(String serviceUrl, String namespace) {
		String path = getInstance().namespace2endpoint.get(namespace).path;
		return serviceUrl + path;
	}
	
	public static String getEndpointUrl(String namespace) {
		String serviceUrl = getServiceUrlByNamespace(namespace);
		return getEndpointUrl(serviceUrl, namespace);
	}
	
	/**
	 * Returns the full endpoint URL for a given namespace where the IP address
	 * is replaced by the IP address internally used by Docker. 
	 * 
	 * @param namespace
	 * @return
	 */
	public static String getDockerEndpointUrl(String namespace) {
		String serviceUrl = getServiceUrlByNamespace(namespace);
		Map<String, String> host2host = getInstance().host2host;
		for (String host : host2host.keySet()) {
			if (serviceUrl.contains(host)) {
				String newhost = host2host.get(host);
				serviceUrl = serviceUrl.replace(host, newhost);
				break;
			}
		}
		return getEndpointUrl(serviceUrl, namespace);
	}
	
	public static Map<String, String> getHostConversionMap() {
		return getInstance().host2host;
	}
	
	/**
	 * Reads the default properties required for creating a new dataset on Blazegraph.
	 * 
	 * @return default properties 
	 */
	public static Properties readStandardNamespacePropertiesForBlazegraph() {
		try (InputStream input = new FileInputStream("./src/test/resources/FedQuery/RWStore.properties")) {
            Properties props = new Properties();
            props.load(input);
            return props;
        } catch (IOException e) {
        	throw new JPSRuntimeException(e.getMessage(), e);
        }
	}
	
	/**
	 * Uploads the triples from a collection of files to the specified endpoint.
	 * 
	 * @param endpointUrl
	 * @param files
	 * @return
	 */
	public static int uploadFiles(String endpointUrl, Collection<File> files) {
		RemoteStoreClient storeClient = new RemoteStoreClient(endpointUrl, endpointUrl);
		//int count = 0;
		//LOGGER.debug("uploading files to endpointURL=" + endpointUrl + ", number of files=" + files.size());
		for (File file : files) {
			//count += 1;
			//LOGGER.debug("uploading file (" + count + "/" + files.size() + ")=" + file.getName());
			storeClient.uploadFile(file);
		}		
		LOGGER.debug("finished uploading files to endpointUrl=" + endpointUrl, ", number of files=" + files.size());
		return files.size();
	}
	
	/**
	 * If a directory is given, all contained files are added to the returned collection.
	 * 
	 * @param dataDirOrFiles a mixed list of file and directory paths
	 * @return
	 */
	public static Collection<File> getFiles(String... dataDirOrFiles) {
		Collection<File> files = new ArrayList<File>();
		for (String current : dataDirOrFiles) {
			
			File file = new File(current);
			if (file.isDirectory()) {
				files.addAll(FileUtils.listFiles(file, null, true));
			} else {
				files.add(file);
			}
		}
		return files; 
	}
	
	/**
	 * Creates the test containers and datasets (endpoints).
	 */
	private void init() {
		LOGGER.debug("initializing triple stores ...");		
		String dirTestResources = DIR_TEST_RESOURCES + "/FedQuery/datasets/";
		initBlazegraph1(dirTestResources);
		initBlazegraph2(dirTestResources);
		initRdf4j1(dirTestResources);
		initEndpointsForGeneratedDatasets();
	}
	
	private void initBlazegraph1(String dirTestResources) {
		int port = 8080;
		GenericContainer<?> container = createBlazegraphTripleStore(port);
		String host = getLocalhost(container);
		String serviceUrl = "http://" + host + "/blazegraph";
		//String dockerServiceUrl = getDockerIp(container, port) + "/blazegraph";
		String dockerHost = getDockerIp(container, port);
		host2host.put(host, dockerHost);
		ContainerInfo info = new ContainerInfo(container, serviceUrl);
		id2container.put(ID_BLAZEGRAPH_1, info);
		LOGGER.debug("created container for id=" + ID_BLAZEGRAPH_1 + ", serviceUrl=" + serviceUrl + ", dockerHost" + dockerHost);
		
		Collection<File> files = new ArrayList<File>();
		String path = BlazegraphRepositoryWrapper.getPathForBlazegraph(NAMESPACE_BLAZEGRAPH_EMTPY);
		createDatasetBlazegraph(ID_BLAZEGRAPH_1, NAMESPACE_BLAZEGRAPH_EMTPY, path, files);
		
		files = getFiles(dirTestResources + "lab_1.ttl");
		path = BlazegraphRepositoryWrapper.getPathForBlazegraph(NAMESPACE_LAB_1);
		createDatasetBlazegraph(ID_BLAZEGRAPH_1, NAMESPACE_LAB_1, path, files);
		
		files = getFiles(dirTestResources + "doe.ttl", dirTestResources + "chemrxn.ttl");
		path = BlazegraphRepositoryWrapper.getPathForBlazegraph(NAMESPACE_DOE_CHEMRXN);
		createDatasetBlazegraph(ID_BLAZEGRAPH_1, NAMESPACE_DOE_CHEMRXN, path, files);
		
		files = getFiles(dirTestResources + "wikidata_small.ttl");
		path = BlazegraphRepositoryWrapper.getPathForBlazegraph(NAMESPACE_WIKIDATA_SMALL);
		createDatasetBlazegraph(ID_BLAZEGRAPH_1, NAMESPACE_WIKIDATA_SMALL, path, files);
	}
	
	private void initBlazegraph2(String dirTestResources) {
		int port = 8080;
		GenericContainer<?> container = createBlazegraphTripleStore(port);
		String host = getLocalhost(container);
		String serviceUrl = "http://" + host + "/blazegraph";
		//String dockerServiceUrl = getDockerIp(container, port) + "/blazegraph";
		String dockerHost = getDockerIp(container, port);
		host2host.put(host, dockerHost);
		ContainerInfo info = new ContainerInfo(container, serviceUrl);
		id2container.put(ID_BLAZEGRAPH_2, info);
		LOGGER.debug("created container for id=" + ID_BLAZEGRAPH_2 + ", serviceUrl=" + serviceUrl + ", dockerHost" + dockerHost);

		Collection<File> files = getFiles(dirTestResources + "lab_2.ttl");
		String path = BlazegraphRepositoryWrapper.getPathForBlazegraph(NAMESPACE_LAB_2);
		createDatasetBlazegraph(ID_BLAZEGRAPH_2, NAMESPACE_LAB_2, path, files);
	}
	
	private void initRdf4j1(String dirTestResources) {
		int port = 8080;
		GenericContainer<?> container = createRDF4JServer(port); 
		String host = getLocalhost(container);
		String serviceUrl = "http://" + host + "/rdf4j-server";
		String dockerHost = getDockerIp(container, port);
		host2host.put(host, dockerHost);
		ContainerInfo info = new ContainerInfo(container, serviceUrl);
		id2container.put(ID_RDF4J_1, info);
		LOGGER.debug("created container for id=" + ID_RDF4J_1 + ", serviceUrl=" + serviceUrl + ", dockerHost" + dockerHost);
		
		createEmptyDatasetRdf4j(ID_RDF4J_1, serviceUrl, NAMESPACE_RDF4J_EMPTY);
	}

	public void createDatasetBlazegraph(String containerId, String namespace, String endpointPath, Collection<File> files) {
		// create namespace
		String serviceUrl = getServiceUrl(containerId);
		BlazegraphRepositoryWrapper wrapper = new BlazegraphRepositoryWrapper(serviceUrl);
		Properties props = readStandardNamespacePropertiesForBlazegraph();
		wrapper.createNamespace(namespace, props);
		wrapper.close();
		
		EndpointInfo info = new EndpointInfo(containerId, endpointPath);
		namespace2endpoint.put(namespace, info);
		
		// upload data
		String url = getEndpointUrl(serviceUrl, namespace);
		if (files != null) {
			uploadFiles(url, files);
		}
		LOGGER.debug("created endpoint with url=" + url);
	}
	
	// see https://rdf4j.org/documentation/reference/rest-api/#repository-creation
	public void createEmptyDatasetRdf4j(String containerId, String serviceUrl, String namespace) {
		String endpointPath = "/repositories/" + namespace;
		String endpointUrl = serviceUrl + endpointPath;
		String body = "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>. "
				+ "@prefix rep: <http://www.openrdf.org/config/repository#>. "
				+ "@prefix sr: <http://www.openrdf.org/config/repository/sail#>. "
				+ "@prefix sail: <http://www.openrdf.org/config/sail#>. "
				+ "@prefix ms: <http://www.openrdf.org/config/sail/memory#>. "
				+ ""
				+ "[] a rep:Repository ; "
				+ "   rep:repositoryID \"" + namespace + "\" ; "
				+ "   rdfs:label \"test memory store\" ; "
				+ "   rep:repositoryImpl [\r\n"
				+ "      rep:repositoryType \"openrdf:SailRepository\" ; "
				+ "      sr:sailImpl [ "
				+ "	 sail:sailType \"openrdf:MemoryStore\" ; "
				+ "	 ms:persist true ; "
				+ "	 ms:syncDelay 120 "
				+ "      ] "
				+ "   ].";
		
		String accept = "application/x-turtle";
		String contentType = "application/x-turtle";
		HttpPut request = Http.put(endpointUrl, body, contentType, accept);
		Http.execute(request);
		
		EndpointInfo info = new EndpointInfo(containerId, endpointPath);
		namespace2endpoint.put(namespace, info);
		
		LOGGER.debug("created endpoint with url=" + endpointUrl);
	}
	
	/**
	 * Generates datasets (instead of reading datasets from file), 
	 * creates endpoints and uploads the generated data.
	 */
	private void initEndpointsForGeneratedDatasets() {
	
		// the method for uploading data expects a file as input parameter
		// for this reason, all generated datasets have to be stored locally
		// in future, the file input parameter may be replaced by an input stream
		String pwd = "./tmp/generateddatasets";
		try {
			pwd = AgentLocator.getPathToJpsWorkingDir() + "/generateddatasets";
		} catch (Exception e){
			LOGGER.warn("JPS working directory was not found");
		}
		LOGGER.info("initializing endpoints and generating datasets, tmp directory=" + pwd);
		
		String[] datasets = DatasetProvider.generateOntoSpeciesOntoCompChem(1000, 2000, 25, 100);
		
		String namespace = NAMESPACE_ONTOSPECIES;
		String path = pwd + "/dataset_" + namespace + ".ttl";
		FileUtil.writeFileLocally(path , datasets[0]);
		Collection<File> files = getFiles(path);
		path = BlazegraphRepositoryWrapper.getPathForBlazegraph(namespace);
		createDatasetBlazegraph(ID_BLAZEGRAPH_1, namespace, path, files);
		
		
		namespace = NAMESPACE_ONTOCOMPCHEM;
		path = pwd + "/dataset_" + namespace + ".ttl";
		FileUtil.writeFileLocally(path , datasets[1]);
		files = getFiles(path);
		path = BlazegraphRepositoryWrapper.getPathForBlazegraph(namespace);
		createDatasetBlazegraph(ID_BLAZEGRAPH_2, namespace, path, files);
	}
}
