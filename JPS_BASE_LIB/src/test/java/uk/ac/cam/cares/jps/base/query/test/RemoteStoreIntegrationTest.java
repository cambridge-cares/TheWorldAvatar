package uk.ac.cam.cares.jps.base.query.test;

import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Paths;

import org.apache.http.client.utils.URIBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.utility.DockerImageName;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * requires docker to be installed to run, hence the @Ignore until everyone is
 * expected to have docker installed
 * @author Kok Foong Lee
 *
 */
public class RemoteStoreIntegrationTest {
	@Container
	private GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("docker.cmclinnovations.com/blazegraph_for_tests:1.0.0"))
												 .withExposedPorts(9999);
	
	@Test
	public void testUploadRDFFile() throws URISyntaxException {
		// start containers
		blazegraph.start();
		
		String endpoint = new URIBuilder().setScheme("http").setHost(blazegraph.getHost())
				.setPort(blazegraph.getFirstMappedPort())
				.setPath("/blazegraph/namespace/kb/sparql").build().toString();

		RemoteStoreClient storeClient = new RemoteStoreClient(endpoint,endpoint);
		
		// getResource returns URL with encodings. convert it to URI to remove them
		// upload the file testOWL.owl to the test container
		String filepath = new URI(getClass().getClassLoader().getResource(Paths.get("KBClientTest","testOWL.owl").toString()).toString()).getPath();
		File testOwl = new File(filepath);
		storeClient.uploadRDFFile(testOwl);
		
		// construct a simple query to check that triples have been uploaded
		SelectQuery query = Queries.SELECT();
        query.where(query.var().has(query.var(),query.var()));
        
        // length is the number of triples uploaded to blazegraph
        Assertions.assertTrue(storeClient.executeQuery(query.getQueryString()).length() > 1);
		blazegraph.stop();
	}
}
