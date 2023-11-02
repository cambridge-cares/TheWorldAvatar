package uk.ac.cam.cares.jps.base.query;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Paths;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.http.HttpEntity;
import org.apache.http.ParseException;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.util.EntityUtils;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.json.JSONArray;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import uk.ac.cam.cares.jps.base.BlazegraphContainer;

/**
 * requires docker to be installed to run, hence the @Ignore until everyone is
 * expected to have docker installed
 * 
 * @author Kok Foong Lee
 *
 */
@Testcontainers
class RemoteStoreIntegrationTest {

	static RemoteStoreClient storeClient;

	@Container
	private static final BlazegraphContainer blazegraph = new BlazegraphContainer();

	@BeforeAll
	static void initialise() throws URISyntaxException {
		storeClient = blazegraph.getRemoteStoreClient();
	}

	@Test
	void testUploadFile() throws URISyntaxException {
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
	void testExecuteUpdateByPost() throws ParseException, IOException {
		String s = "<http://" + UUID.randomUUID().toString() + ">";
		String p = "<http://" + UUID.randomUUID().toString() + ">";
		String o = "<http://" + UUID.randomUUID().toString() + ">";
		String update = "insert data {" + s + " " + p + " " + o + "}";
		try (CloseableHttpResponse response = storeClient.executeUpdateByPost(update)) {
			HttpEntity entity = response.getEntity();
			// the entity should not be null as we are querying blazegraph
			Assertions.assertNotNull(entity);
			String html = EntityUtils.toString(entity);
			System.out.println(html);
			Pattern pattern = Pattern.compile("mutationCount=([0-9]+)");
			Matcher matcher = pattern.matcher(html);
			// matcher should find the value
			Assertions.assertTrue(matcher.find());
			// the value should be 1 as one triple was added to KG
			Assertions.assertEquals(1, Integer.parseInt(matcher.group(1)));
		} catch (Exception e) {
			throw e;
		}
	}

	@Test
	void testExecuteQuery() {
		// first upload a triple to KG
		String s = "http://" + UUID.randomUUID().toString();
		String p1 = "http://" + UUID.randomUUID().toString();
		String o1 = "http://" + UUID.randomUUID().toString();
		String p2 = "http://" + UUID.randomUUID().toString();
		String o2 = UUID.randomUUID().toString();
		String update = "insert data {<" + s + "> <" + p1 + "> <" + o1 + ">. <" + s + "> <" + p2 + "> \"" + o2
				+ "\". }";
		storeClient.executeUpdate(update);

		// construct a simple query to check that triples have been uploaded
		SelectQuery query = Queries.SELECT();
		Variable o1Var = query.var();
		Variable o2Var = query.var();
		query.where(Rdf.iri(s).has(Rdf.iri(p1), o1Var).and(Rdf.iri(s).has(Rdf.iri(p2), o2Var)));

		// length is the number of triples uploaded to blazegraph
		JSONArray result = storeClient.executeQuery(query.getQueryString());
		Assertions.assertEquals(1, result.length());
		Assertions.assertEquals(o1, result.getJSONObject(0).getString(o1Var.getQueryString().substring(1)));
		Assertions.assertEquals(o2, result.getJSONObject(0).getString(o2Var.getQueryString().substring(1)));
	}
}
