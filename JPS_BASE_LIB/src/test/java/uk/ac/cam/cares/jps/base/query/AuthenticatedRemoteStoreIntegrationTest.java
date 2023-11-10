package uk.ac.cam.cares.jps.base.query;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Paths;
import java.util.Base64;

import org.apache.http.HttpHeaders;
import org.apache.http.ParseException;
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
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import uk.ac.cam.cares.jps.base.BlazegraphContainer;
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

	private static RemoteStoreClient authenticatedStoreClient;
	private static RemoteStoreClient unauthenticatedStoreClient;
	private static String baseEndpoint;
	private static String defaultEndpoint;
	private static String otherEndpoint;

	private static final String s = "http://testURI_1";
	private static final String p = "http://someObjectProperty";
	private static final String o = "http://testURI_2";
	private static final String p2 = "http://someDataProperty";
	private static final String o2 = "value";

	private static final String VAR1_NAME = "var1";
	private static final String VAR2_NAME = "var2";
	private static final Variable o1Var = SparqlBuilder.var(VAR1_NAME);
	private static final Variable o2Var = SparqlBuilder.var(VAR2_NAME);
	private static final String ONE_TRIPLE_QUERY = Queries.SELECT().select(o1Var)
			.where(Rdf.iri(s).has(Rdf.iri(p), o1Var)).getQueryString();
	private static final String TWO_TRIPLE_QUERY = Queries.SELECT()
			.where(Rdf.iri(s).has(Rdf.iri(p), o1Var).and(Rdf.iri(s).has(Rdf.iri(p2), o2Var))).getQueryString();

	@Container
	public static BlazegraphContainer blazegraph = new BlazegraphContainer().withAuthentication();

	@BeforeAll
	public static void initialise() throws URISyntaxException, IOException, InterruptedException {
		defaultEndpoint = blazegraph.getURL();

		authenticatedStoreClient = blazegraph.getRemoteStoreClient();

		unauthenticatedStoreClient = blazegraph.getRemoteStoreClient(false);

		baseEndpoint = new URIBuilder().setScheme("http").setHost(blazegraph.getHost())
				.setPort(blazegraph.getFirstMappedPort())
				.setPath("/blazegraph/namespace").build().toString();

		createNamespace("test", baseEndpoint);

		otherEndpoint = baseEndpoint + "/test/sparql";
	}

	@AfterEach
	public void deleteDataAndStopContainers() {
		// delete all triples
		String deleteData = "DELETE WHERE {?a ?b ?c}";

		authenticatedStoreClient.setUpdateEndpoint(otherEndpoint);
		authenticatedStoreClient.executeUpdate(deleteData);

		authenticatedStoreClient.setUpdateEndpoint(defaultEndpoint);
		authenticatedStoreClient.executeUpdate(deleteData);

	}

	@Test
	void testExecuteUpdateByPostUnauthorised() throws ParseException, IOException, InterruptedException {

		String update = "insert data { <" + s + "> <" + p + "> <" + o + "> }";

		// execute update by post without credentials
		Assert.assertThrows("Unauthenticated store client should not be able to update by post.",
				JPSRuntimeException.class, () -> unauthenticatedStoreClient.executeUpdateByPost(update));
	}

	@Test
	void testExecuteUpdateByPost() throws ParseException, IOException, InterruptedException {

		String update = "insert data { <" + s + "> <" + p + "> <" + o + "> }";

		// execute update by post with credentials
		authenticatedStoreClient.executeUpdateByPost(update);

		// construct a simple query for the first set of triple
		JSONArray result = authenticatedStoreClient.executeQuery(ONE_TRIPLE_QUERY);
		Assert.assertEquals(1, result.length());
		Assert.assertEquals(o, result.getJSONObject(0).getString(VAR1_NAME));
	}

	@Test
	void testUploadFileUnauthorised() throws URISyntaxException, InterruptedException {
		File testOwl = getTestOwlFile();
		// upload file without credentials
		Assert.assertThrows("Unauthenticated store client should not be able to upload a file.",
				JPSRuntimeException.class, () -> unauthenticatedStoreClient.uploadFile(testOwl));
	}

	@Test
	void testUploadFile() throws URISyntaxException, InterruptedException {
		File testOwl = getTestOwlFile();
		// upload file with credentials
		authenticatedStoreClient.uploadFile(testOwl);

		SelectQuery query = Queries.SELECT();
		query.where(query.var().has(query.var(), query.var()));
		// length is the number of triples uploaded to blazegraph
		Assertions.assertTrue(authenticatedStoreClient.executeQuery(query.getQueryString()).length() > 1);
	}

	private File getTestOwlFile() throws URISyntaxException {
		// getResource returns URL with encodings. convert it to URI to remove them
		// upload the file testOWL.owl to the test container
		String filepath = new URI(
				getClass().getClassLoader().getResource(Paths.get("KBClientTest", "testOWL.owl").toString()).toString())
				.getPath();
		File testOwl = new File(filepath);
		return testOwl;
	}

	@Test
	void testExecuteQueryUnauthorised() {
		addTriples();

		// run query without credentials
		Assert.assertThrows("Unauthenticated store client should not be able to run a query.",
				JPSRuntimeException.class, () -> unauthenticatedStoreClient.executeQuery(TWO_TRIPLE_QUERY));
	}

	@Test
	void testExecuteQuery() {
		addTriples();

		// run query with credentials
		JSONArray result = authenticatedStoreClient.executeQuery(TWO_TRIPLE_QUERY);
		Assert.assertEquals(1, result.length());
		Assert.assertEquals(o, result.getJSONObject(0).getString(VAR1_NAME));
		Assert.assertEquals(o2, result.getJSONObject(0).getString(VAR2_NAME));
	}

	private void addTriples() {
		// first upload a triple to KG
		String update = "insert data {<" + s + "> <" + p + "> <" + o + ">; <" + p2 + "> \"" + o2 + "\". }";

		authenticatedStoreClient.executeUpdate(update);
	}

	@Test
	void testFederatedExecuteQueryUnauthorised() {

		addTriplesToTwoNamespaces();

		RemoteStoreClient unauthorisedFedRemoteStore = new RemoteStoreClient();
		unauthorisedFedRemoteStore.addRemoteStore(new RemoteStore("kb", defaultEndpoint));
		unauthorisedFedRemoteStore.addRemoteStore(new RemoteStore("test", otherEndpoint));

		Assert.assertThrows("Unauthenticated store client should not be able to run a federated query.",
				RuntimeException.class, () -> unauthorisedFedRemoteStore.executeFederatedQuery(TWO_TRIPLE_QUERY));

	}

	@Test
	void testFederatedExecuteQuery() {

		addTriplesToTwoNamespaces();

		RemoteStoreClient authorisedFedRemoteStore = new RemoteStoreClient();
		authorisedFedRemoteStore
				.addRemoteStore(new RemoteStore("kb", defaultEndpoint, defaultEndpoint, BlazegraphContainer.USERNAME,
						BlazegraphContainer.PASSWORD));
		authorisedFedRemoteStore
				.addRemoteStore(new RemoteStore("test", otherEndpoint, otherEndpoint, BlazegraphContainer.USERNAME,
						BlazegraphContainer.PASSWORD));

		// run a federated query and check whether the results obtained is correct
		JSONArray result = authorisedFedRemoteStore.executeFederatedQuery(TWO_TRIPLE_QUERY);
		Assert.assertEquals(1, result.length());
		Assert.assertEquals(o, result.getJSONObject(0).getString(VAR1_NAME));
		Assert.assertEquals(o2, result.getJSONObject(0).getString(VAR2_NAME));

	}

	private void addTriplesToTwoNamespaces() {
		// upload a triple to test namespace
		String update2 = "insert data {<" + s + "> <" + p2 + "> " + "\"" + o2 + "\". }";
		authenticatedStoreClient.setUpdateEndpoint(otherEndpoint);
		authenticatedStoreClient.executeUpdate(update2);

		// upload a triple to kb namespace
		String update1 = "insert data {<" + s + "> <" + p + "> <" + o + ">. }";
		authenticatedStoreClient.setUpdateEndpoint(defaultEndpoint);
		authenticatedStoreClient.executeUpdate(update1);
	}

	public static void createNamespace(String namespace, String baseEndpoint) {
		// Generate XML properties for request
		String payload = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>" +
				"<!DOCTYPE properties SYSTEM \"http://java.sun.com/dtd/properties.dtd\">" +
				"<properties>" +
				"  <entry key=\"com.bigdata.rdf.sail.truthMaintenance\">false</entry>" +
				"  <entry key=\"com.bigdata.rdf.store.AbstractTripleStore.textIndex\">false</entry>" +
				"  <entry key=\"com.bigdata.namespace." + namespace
				+ ".lex.com.bigdata.btree.BTree.branchingFactor\">400</entry>" +
				"  <entry key=\"com.bigdata.namespace." + namespace
				+ ".spo.com.bigdata.btree.BTree.branchingFactor\">1024</entry>" +
				"  <entry key=\"com.bigdata.rdf.store.AbstractTripleStore.justify\">false</entry>" +
				"  <entry key=\"com.bigdata.rdf.store.AbstractTripleStore.statementIdentifiers\">false</entry>" +
				"  <entry key=\"com.bigdata.rdf.store.AbstractTripleStore.axiomsClass\">com.bigdata.rdf.axioms.NoAxioms</entry>"
				+
				"  <entry key=\"com.bigdata.rdf.sail.namespace\">" + namespace + "</entry>" +
				"  <entry key=\"com.bigdata.rdf.store.AbstractTripleStore.quads\">false</entry>" +
				"  <entry key=\"com.bigdata.rdf.store.AbstractTripleStore.geoSpatial\">false</entry>" +
				"  <entry key=\"com.bigdata.rdf.sail.isolatableIndices\">false</entry>" +
				"</properties>";
		StringEntity configEntity = new StringEntity(payload, ContentType.create("application/xml", "UTF-8"));
		// Create a new post request
		HttpPost request = new HttpPost(baseEndpoint);

		String auth = BlazegraphContainer.USERNAME + ":" + BlazegraphContainer.PASSWORD;
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
		HttpDelete request = new HttpDelete(baseEndpoint + "/" + namespace);
		String auth = BlazegraphContainer.USERNAME + ":" + BlazegraphContainer.PASSWORD;
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
