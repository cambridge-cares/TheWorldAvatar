package uk.ac.cam.cares.jps.base.query.test;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Paths;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.ParseException;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.util.EntityUtils;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.junit.Assert;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * requires docker to be installed to run, hence the @Ignore until everyone is
 * expected to have docker installed
 * 
 * @author Kok Foong Lee
 *
 */
@Testcontainers
public class RemoteStoreIntegrationTest {
	static RemoteStoreClient storeClient;

	@Container
	private static GenericContainer<?> blazegraph = new GenericContainer<>(
			DockerImageName.parse("docker.cmclinnovations.com/blazegraph_for_tests:1.0.0"))
			.withExposedPorts(9999);

	@BeforeAll
	public static void initialise() throws URISyntaxException {
		// start containers
		blazegraph.start();

		String endpoint = new URIBuilder().setScheme("http").setHost(blazegraph.getHost())
				.setPort(blazegraph.getFirstMappedPort())
				.setPath("/blazegraph/namespace/kb/sparql").build().toString();

		storeClient = new RemoteStoreClient(endpoint, endpoint);
	}

	@AfterAll
	public static void stopContainers() {
		// close containers after all tests
		if (blazegraph.isRunning()) {
			blazegraph.stop();
		}
	}

	@Test
	public void testUploadFile() throws URISyntaxException {
		// getResource returns URL with encodings. convert it to URI to remove them
		// upload the file testOWL.owl to the test container
		String filepath = new URI(
				getClass().getClassLoader().getResource(Paths.get("KBClientTest", "testOWL.owl").toString()).toString())
				.getPath();
		File testOwl = new File(filepath);
		storeClient.uploadFile(testOwl);

		// construct a simple query to check that triples have been uploaded
		SelectQuery query = Queries.SELECT();
		query.where(query.var().has(query.var(), query.var()));

		// length is the number of triples uploaded to blazegraph
		Assertions.assertTrue(storeClient.executeQuery(query.getQueryString()).length() > 1);
	}

	@Test
	public void testExecuteUpdateByPost() throws ParseException, IOException {
		String s = "<http://" + UUID.randomUUID().toString() + ">";
		String p = "<http://" + UUID.randomUUID().toString() + ">";
		String o = "<http://" + UUID.randomUUID().toString() + ">";
		String update = "insert data {" + s + " " + p + " " + o + "}";
		HttpResponse response = storeClient.executeUpdateByPost(update);
		HttpEntity entity = response.getEntity();
		// the entity should not be null as we are querying blazegraph
		Assert.assertNotNull(entity);
		String html = EntityUtils.toString(entity);
		System.out.println(html);
		Pattern pattern = Pattern.compile("mutationCount=(.*)</p");
		Matcher matcher = pattern.matcher(html);
		// matcher should find the value
		Assert.assertTrue(matcher.find());
		// the value should be 1 as one triple was added to KG
		Assert.assertEquals(1, Integer.parseInt(matcher.group(1)));
	}
}
