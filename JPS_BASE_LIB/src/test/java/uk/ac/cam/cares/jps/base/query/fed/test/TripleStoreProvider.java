package uk.ac.cam.cares.jps.base.query.fed.test;

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
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.http.Http;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.query.fed.BlazegraphRepositoryWrapper;

public class TripleStoreProvider extends TestCase {
	
	private class ContainerInfo {
		
		GenericContainer<?> container = null;
		String serviceUrl = null;
		
		public ContainerInfo(GenericContainer<?> container, String serviceUrl) {
			this.container = container;
			this.serviceUrl = serviceUrl;
		}
	}
	
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
	
	public final static String ID_BLAZEGRAPH_1 = "blazegraph_1";
	public final static String ID_BLAZEGRAPH_2 = "blazegraph_2";
	public final static String ID_RDF4J_1 = "rdf4j_1";
	
	public final static String NAMESPACE_LAB_1 = "lab_1";
	public final static String NAMESPACE_LAB_2 = "lab_2";
	public final static String NAMESPACE_DOE_CHEMRXN = "doe_chemrxn";
	public final static String NAMESPACE_WIKIDATA_SMALL = "wikidata_small";
	public final static String NAMESPACE_BLAZEGRAPH_EMTPY = "blazegraph_empty";
	
	public static final String NAMESPACE_RDF4J_EMPTY = "rdf4j_empty";

	private static TripleStoreProvider instance = null;
	private Map<String, ContainerInfo> id2container = new HashMap<String, ContainerInfo>();
	private Map<String, EndpointInfo> namespace2endpoint = new HashMap<String, EndpointInfo>();
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
	
	private GenericContainer<?> createContainerByImage(String imageName) {
		return new GenericContainer<>(DockerImageName.parse(imageName));
	}
	
	/**
	 * @param imageName if null image will be deleted after test; if not null the image can be reused when restarting the test 
	 * @return
	 */
	private GenericContainer<?> createContainerByBuilder(String imageName) {
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
	
	// Will create a container that is shared between tests.
	// NOTE: requires access to the docker.cmclinnovations.com registry from the machine the test is run on.
	// For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
	//@Container
	//private static final GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("docker.cmclinnovations.com/blazegraph_for_tests:1.0.0"))
	//		.withExposedPorts(9999);
	private GenericContainer<?> createBlazegraphTripleStore(int port) {
		String imageName = "blazegraphtest";
		GenericContainer<?> tripleStore = null;
		try {
			tripleStore = createContainerByImage(imageName).withExposedPorts(port);
			tripleStore.start();
			LOGGER.debug("Docker image was started, name=" + imageName + ", port=" + port);
		} catch (ContainerLaunchException exc) {
			LOGGER.debug("Docker image was not found, name=" + imageName + ", port=" + port);
			tripleStore = createContainerByBuilder(imageName).withExposedPorts(port);
			tripleStore.start();
			LOGGER.debug("Docker image was created and started, name=" + imageName + ", port=" + port);
		}
		return tripleStore;
	}
	
	private GenericContainer<?> createRDF4JServer(int port) {
		String imageName = "eclipse/rdf4j-workbench:3.7.4";
		GenericContainer<?> tripleStore = createContainerByImage(imageName).withExposedPorts(port);
		tripleStore.start();
		LOGGER.debug("Docker image was started, name=" + imageName + ", port=" + port);
		return tripleStore;
	}
	
	private String getLocalhost(GenericContainer<?> container) {
		return container.getHost() + ":" + container.getFirstMappedPort();
	}
	
	private String getDockerIp(GenericContainer<?> container, int port) {
		InspectContainerResponse info = container.getContainerInfo();
		String ipAddress = info.getNetworkSettings().getIpAddress();
		return ipAddress + ":" + port; 
	}
	
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
	
	public static Properties readStandardNamespaceProperties() {
		try (InputStream input = new FileInputStream("./src/test/resources/FedQuery/RWStore.properties")) {
            Properties props = new Properties();
            props.load(input);
            return props;
        } catch (IOException e) {
        	throw new JPSRuntimeException(e.getMessage(), e);
        }
	}
	
	public static int uploadFiles(String endpointUrl, Collection<File> files) {
		RemoteStoreClient storeClient = new RemoteStoreClient(endpointUrl, endpointUrl);
		int count = 0;
		//LOGGER.debug("uploading files to endpointURL=" + endpointUrl + ", number of files=" + files.size());
		for (File file : files) {
			count += 1;
			//LOGGER.debug("uploading file (" + count + "/" + files.size() + ")=" + file.getName());
			storeClient.uploadFile(file);
		}		
		LOGGER.debug("finished uploading files to endpointUrl=" + endpointUrl, ", number of files=" + files.size());
		return files.size();
	}
	
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
	
	private void init() {
		LOGGER.debug("initializing triple stores ...");		
		String dirTestResources = DIR_TEST_RESOURCES + "/FedQuery/datasets/";
		initBlazegraph1(dirTestResources);
		initBlazegraph2(dirTestResources);
		initRdf4j1(dirTestResources);
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
		LOGGER.debug("container created with id=" + ID_BLAZEGRAPH_1 + ", serviceUrl=" + serviceUrl, ", dockerHost" + dockerHost);
		
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
		LOGGER.debug("container created with id=" + ID_BLAZEGRAPH_2 + ", serviceUrl=" + serviceUrl, ", dockerHost" + dockerHost);

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
		LOGGER.debug("container created with id=" + ID_RDF4J_1 + ", serviceUrl=" + serviceUrl, ", dockerHost" + dockerHost);
		
		createEmptyDatasetRdf4j(ID_RDF4J_1, serviceUrl, NAMESPACE_RDF4J_EMPTY);
	}

	private void createDatasetBlazegraph(String containerId, String namespace, String endpointPath, Collection<File> files) {
		// create namespace
		String serviceUrl = getServiceUrl(containerId);
		BlazegraphRepositoryWrapper wrapper = new BlazegraphRepositoryWrapper(serviceUrl);
		Properties props = readStandardNamespaceProperties();
		wrapper.createNamespace(namespace, props);
		wrapper.close();
		
		EndpointInfo info = new EndpointInfo(containerId, endpointPath);
		namespace2endpoint.put(namespace, info);
		
		// upload data
		String url = getEndpointUrl(serviceUrl, namespace);
		uploadFiles(url, files);
		LOGGER.debug("created dataset and uploaded data, url=" + url);
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
	}
}
