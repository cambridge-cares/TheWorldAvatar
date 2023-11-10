package uk.ac.cam.cares.jps.base.query;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Base64;
import java.util.UUID;

import org.apache.http.HttpHeaders;
import org.apache.http.ParseException;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpDelete;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.json.JSONArray;
import org.junit.Assert;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient.RemoteStore;

/**
 * This tests the remote store client against an authenticated remote store.
 * 
 * @author Wilson Ang
 *
 */
@Testcontainers
public class AuthenticatedRemoteStoreIntegrationTest {
	static RemoteStoreClient storeClient;
	public static final String USERNAME = "bg_user";
	public static final String PASSWORD = "bg_password";
	static String default_endpoint;
	static String sparql_endpoint;
	
	@TempDir
	static Path tempDir;

	@Container
	public static GenericContainer<?> blazegraph = new GenericContainer<>(
			DockerImageName.parse("ghcr.io/cambridge-cares/blazegraph:1.1.0")).withExposedPorts(8080).withEnv("BLAZEGRAPH_PASSWORD_FILE", "/run/secrets/blazegraph_password").withEnv("BLAZEGRAPH_USER", "bg_user")
			.withCommand("bash", "-c", "mkdir -p /run/secrets && echo bg_password > /run/secrets/blazegraph_password && /docker-entrypoint.sh");

	@BeforeAll
	public static void initialise() throws URISyntaxException, IOException, InterruptedException {
		blazegraph.start();
		default_endpoint = new URIBuilder().setScheme("http").setHost(blazegraph.getHost())
				.setPort(blazegraph.getFirstMappedPort())
				.setPath("/blazegraph/namespace/kb/sparql").build().toString();

		sparql_endpoint = new URIBuilder().setScheme("http").setHost(blazegraph.getHost())
				.setPort(blazegraph.getFirstMappedPort())
				.setPath("/blazegraph/namespace").build().toString();

		createNamespace("test", sparql_endpoint);
		
		storeClient = new RemoteStoreClient();
	}
	@AfterEach
	public void deleteDataAndStopContainers() {
		storeClient.setUpdateEndpoint(default_endpoint);
		storeClient.setUser(USERNAME);
		storeClient.setPassword(PASSWORD);
		//delete all triples
		String deleteData = "DELETE WHERE {?a ?b ?c}";
		storeClient.executeUpdate(deleteData);
		storeClient.setUpdateEndpoint(sparql_endpoint + "/test/sparql");
		storeClient.executeUpdate(deleteData);
	}

	@AfterAll
	public static void StopContainers() {
		// close containers after each test
		if (blazegraph.isRunning()) {
			blazegraph.stop();
		}
	}

	@Test
	void testExecuteUpdateByPost() throws ParseException, IOException, InterruptedException {
		String s = "http://" + UUID.randomUUID().toString();
		String p = "http://" + UUID.randomUUID().toString();
		String o = "http://" + UUID.randomUUID().toString();
		String update = "insert data { <" + s + "> <" + p + "> <" + o + "> }";
		storeClient.setUpdateEndpoint(default_endpoint);
		storeClient.setQueryEndpoint(default_endpoint);
		//execute update by post without credentials
		try (CloseableHttpResponse response = storeClient.executeUpdateByPost(update)) {
				} catch (Exception e) {
			Assert.assertTrue(e.toString().contains("Response status code =401"));
		}
		//set credentials and execute update by post
		storeClient.setUser(USERNAME);
		storeClient.setPassword(PASSWORD);
		storeClient.executeUpdateByPost(update);
		// construct a simple query for the first set of triple
		SelectQuery query = Queries.SELECT();
		Variable o1Var = SparqlBuilder.var("var");
		query.select(o1Var).where(Rdf.iri(s).has(Rdf.iri(p), o1Var));
		JSONArray result = storeClient.executeQuery(query.getQueryString());
		Assert.assertEquals(1, result.length());
		Assert.assertEquals(o, result.getJSONObject(0).getString("var"));
	}

	@Test
	void testUploadFile() throws URISyntaxException, InterruptedException {
		// getResource returns URL with encodings. convert it to URI to remove them
		// upload the file testOWL.owl to the test container
		String filepath = new URI(
				getClass().getClassLoader().getResource(Paths.get("KBClientTest", "testOWL.owl").toString()).toString())
				.getPath();
		File testOwl = new File(filepath);
		storeClient.setUpdateEndpoint(default_endpoint);
		storeClient.setQueryEndpoint(default_endpoint);
		//upload file without credentials
		try {
		storeClient.uploadFile(testOwl);
		Assert.fail();
		} catch (Exception e) {
			Assert.assertTrue(e.toString().contains("status code = 401"));
		}
		//set credentials and upload file
		storeClient.setUser(USERNAME);
		storeClient.setPassword(PASSWORD);
		storeClient.uploadFile(testOwl);
		// construct a simple query to check that triples have been uploaded
		SelectQuery query = Queries.SELECT();
        query.where(query.var().has(query.var(),query.var()));
        // length is the number of triples uploaded to blazegraph
		Assertions.assertTrue(storeClient.executeQuery(query.getQueryString()).length() > 1);
	}

	@Test
	public void testExecuteQuery() {
		// first upload a triple to KG
		String s = "http://" + UUID.randomUUID().toString();
		String p1 = "http://" + UUID.randomUUID().toString();
		String o1 = "http://" + UUID.randomUUID().toString();
		String p2 = "http://" + UUID.randomUUID().toString();
		String o2 = UUID.randomUUID().toString();
		String update = "insert data {<" + s + "> <" + p1 + "> <" + o1 + ">. <" + s + "> <" + p2 + "> \"" + o2 + "\". }";
		storeClient.setUpdateEndpoint(default_endpoint);
		storeClient.setQueryEndpoint(default_endpoint);
		storeClient.setUser(USERNAME);
		storeClient.setPassword(PASSWORD);
		storeClient.executeUpdate(update);

		// construct a simple query to check that triples have been uploaded
		SelectQuery query = Queries.SELECT();
		Variable o1Var = query.var();
		Variable o2Var = query.var();
		query.where(Rdf.iri(s).has(Rdf.iri(p1), o1Var).and(Rdf.iri(s).has(Rdf.iri(p2), o2Var)));

		//set new store client without credentials
		storeClient = new RemoteStoreClient(default_endpoint, default_endpoint);
		// run query without credentials
		try {
			JSONArray result = storeClient.executeQuery(query.getQueryString());
			Assert.fail();
		} catch (Exception e) {
			Assert.assertTrue(e.getCause().getCause().toString().contains("HttpException: 401 Unauthorized"));
		}
		//set credentials
		storeClient.setUser(USERNAME);
		storeClient.setPassword(PASSWORD);
		//run query with credentials
		JSONArray result = storeClient.executeQuery(query.getQueryString());
		Assert.assertEquals(1, result.length());
		Assert.assertEquals(o1, result.getJSONObject(0).getString(o1Var.getQueryString().substring(1)));
		Assert.assertEquals(o2, result.getJSONObject(0).getString(o2Var.getQueryString().substring(1)));
	}

	@Test
	public void testFederatedExecuteQuery() {
		// upload a triple to kb namespace
		String s = "http://testURI_1";
		String p1 = "http://someObjectProperty";
		String o1 = "http://testURI_2";
		String update1 = "insert data {<" + s + "> <" + p1 + "> <" + o1 + ">. }";
		storeClient.setUpdateEndpoint(default_endpoint);
		storeClient.setUser(USERNAME);
		storeClient.setPassword(PASSWORD);
		storeClient.executeUpdate(update1);

		// upload a triple to test namespace
		String p2 = "http://someDataProperty";
		String d1 = "This is working!";
		String update2 = "insert data {<" + s + "> <" + p2 + "> " + "\"" + d1 + "\". }";
		storeClient.setUpdateEndpoint(sparql_endpoint + "/test/sparql");
		storeClient.setUser(USERNAME);
		storeClient.setPassword(PASSWORD);
		storeClient.executeUpdate(update2);

		RemoteStore remoteStore = new RemoteStore("kb",default_endpoint);
		storeClient.addRemoteStore(remoteStore);
		RemoteStore testRemoteStore = new RemoteStore("test",sparql_endpoint + "/test/sparql");
		storeClient.addRemoteStore(testRemoteStore);

		// construct a simple query for the first set of triple
		SelectQuery query = Queries.SELECT();
		Variable o1Var = SparqlBuilder.var("var");
		query.where(Rdf.iri(s).has(Rdf.iri(p1), o1Var));

		try {
		JSONArray result = storeClient.executeFederatedQuery(query.getQueryString());
		Assert.fail();
		} catch (Exception e) {
			Assert.assertEquals("class org.eclipse.rdf4j.http.protocol.UnauthorizedException", e.getCause().getCause().getCause().getCause().getClass().toString());
		}

		//set credentials for each remote store
		storeClient = new RemoteStoreClient();
		remoteStore = new RemoteStore("kb",default_endpoint);
		remoteStore.setUserName(USERNAME);
		remoteStore.setPassword(PASSWORD);
		storeClient.addRemoteStore(remoteStore);
		testRemoteStore = new RemoteStore("test",sparql_endpoint + "/test/sparql");
		testRemoteStore.setUserName(USERNAME);
		testRemoteStore.setPassword(PASSWORD);
		storeClient.addRemoteStore(testRemoteStore);

		// construct a simple query for the first set of triple
		query = Queries.SELECT();
		o1Var = SparqlBuilder.var("var");
		query.where(Rdf.iri(s).has(Rdf.iri(p1), o1Var));

		// run a federated query and check whether the results obtained is correct
		JSONArray result = storeClient.executeFederatedQuery(query.getQueryString());
		Assert.assertEquals(1, result.length());
		Assert.assertEquals(o1, result.getJSONObject(0).getString("var"));

		// construct a simple query for the second set of triple
		SelectQuery query2 = Queries.SELECT();
		query2.where(Rdf.iri(s).has(Rdf.iri(p2), o1Var));

		// run a federated query and check whether the results obtained is correct
		result = storeClient.executeFederatedQuery(query2.getQueryString());
		Assert.assertEquals(1, result.length());
		Assert.assertEquals("This is working!", result.getJSONObject(0).getString("var"));
	}
	
    public static void createNamespace(String namespace, String sparql_endpoint) {
        // Generate XML properties for request
        String payload =
                "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>" +
                        "<!DOCTYPE properties SYSTEM \"http://java.sun.com/dtd/properties.dtd\">" +
                        "<properties>" +
                        "  <entry key=\"com.bigdata.rdf.sail.truthMaintenance\">false</entry>" +
                        "  <entry key=\"com.bigdata.rdf.store.AbstractTripleStore.textIndex\">false</entry>" +
                        "  <entry key=\"com.bigdata.namespace." + namespace + ".lex.com.bigdata.btree.BTree.branchingFactor\">400</entry>" +
                        "  <entry key=\"com.bigdata.namespace." + namespace + ".spo.com.bigdata.btree.BTree.branchingFactor\">1024</entry>" +
                        "  <entry key=\"com.bigdata.rdf.store.AbstractTripleStore.justify\">false</entry>" +
                        "  <entry key=\"com.bigdata.rdf.store.AbstractTripleStore.statementIdentifiers\">false</entry>" +
                        "  <entry key=\"com.bigdata.rdf.store.AbstractTripleStore.axiomsClass\">com.bigdata.rdf.axioms.NoAxioms</entry>" +
                        "  <entry key=\"com.bigdata.rdf.sail.namespace\">" + namespace + "</entry>" +
                        "  <entry key=\"com.bigdata.rdf.store.AbstractTripleStore.quads\">false</entry>" +
                        "  <entry key=\"com.bigdata.rdf.store.AbstractTripleStore.geoSpatial\">false</entry>" +
                        "  <entry key=\"com.bigdata.rdf.sail.isolatableIndices\">false</entry>" +
                        "</properties>";
        StringEntity configEntity = new StringEntity(payload, ContentType.create("application/xml", "UTF-8"));
        // Create a new post request
        HttpPost request = new HttpPost(sparql_endpoint);

		String auth = USERNAME + ":" + PASSWORD;
		String encodedAuth = Base64.getEncoder().encodeToString(auth.getBytes());
		request.setHeader(HttpHeaders.AUTHORIZATION, "Basic " + encodedAuth);

        request.setHeader("Accept", "application/xml");
        request.addHeader("Content-Type", "application/xml");
        request.setEntity(configEntity);
        // Send request
        try (CloseableHttpClient httpClient = HttpClients.createDefault()) {
            httpClient.execute(request);
        } catch (IOException e) {
            throw new JPSRuntimeException("Unable to create namespace: " + e.getMessage());
        }
    }

	public static void deleteNamespace(String namespace) {
		HttpDelete request = new HttpDelete(sparql_endpoint + "/" + namespace);
		String auth = USERNAME + ":" + PASSWORD;
		String encodedAuth = Base64.getEncoder().encodeToString(auth.getBytes());
		request.setHeader(HttpHeaders.AUTHORIZATION, "Basic " + encodedAuth);
		// Send request
        try (CloseableHttpClient httpClient = HttpClients.createDefault()) {
            httpClient.execute(request);
        } catch (IOException e) {
            throw new JPSRuntimeException("Unable to delete namespace: " + e.getMessage());
        }
	}
}

