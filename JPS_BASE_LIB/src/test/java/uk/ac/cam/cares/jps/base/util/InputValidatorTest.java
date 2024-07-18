package uk.ac.cam.cares.jps.base.util;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.net.URISyntaxException;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;

public class InputValidatorTest {
	private String iriofnetwork = null;
	private String filePath = null;

	@Before
	public void setUp() throws URISyntaxException {
		iriofnetwork = "http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/SingaporeElectricalNetwork.owl#SingaporeElectricalNetwork";
		Path testResourcePath = Paths.get(this.getClass().getResource("/KBClientTest/testRDF.rdf").toURI());
		filePath = testResourcePath.toString();

	}

	/**
	 * tests if the IRI is valid
	 * 
	 */
	@Test
	public void testInputValidatorIRI() {
		assertFalse(InputValidator.checkIfValidIRI("abcd"));
		assertFalse(InputValidator.checkIfValidIRI(filePath));
		assertTrue(InputValidator.checkIfValidIRI(iriofnetwork));
	}

	/**
	 * tests if the file path is valid (not necessary if it exists)
	 * 
	 */
	@Test
	public void testInputValidatorFilePath() {
		assertFalse(InputValidator.checkIfFilePath(iriofnetwork));
		assertTrue(InputValidator.checkIfFilePath(filePath));
	}

	/**
	 * tests if file exist
	 * 
	 */
	@Test
	public void testInputValidatorFileExists() {
		assertTrue(InputValidator.checkIfValidFile(filePath));
		assertFalse(InputValidator.checkIfValidFile("C://ThisShouldntExist.pdf"));
	}

	/**
	 * tests if input is boolean string
	 * 
	 */
	@Test
	public void testIfBoolean() {
		assertTrue(InputValidator.checkBoolean("fAlse"));
		assertFalse(InputValidator.checkBoolean("1"));
		assertFalse(InputValidator.checkBoolean(0.6));
	}

	/**
	 * tests if it's a URL pattern
	 * 
	 */
	@Test
	public void testIfURLPattern() {
		assertFalse(InputValidator.checkIfURLpattern("0.6"));
		assertFalse(InputValidator.checkIfURLpattern(filePath));
		assertTrue(InputValidator.checkIfURLpattern(iriofnetwork));
	}

	/**
	 * tests if it's a SPARQL Query
	 * 
	 */
	@Test
	public void testIfQuery() {
		assertFalse(InputValidator.checkIfValidQuery(iriofnetwork));
		String queryString = new SelectBuilder()
				.addPrefix("j1", "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#")
				.addPrefix("j2", "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#")
				.addPrefix("j3", "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#")
				.addPrefix("j5", "http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#")
				.addPrefix("j7",
						"http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#")
				.addPrefix("j9", "http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#")
				.addVar("?entity").addVar("?valueofx").addVar("?valueofy").addVar("?BusNumbervalue")
				.addWhere("?entity", "a", "j1:PowerGenerator")
				.addWhere("?entity", "j9:realizes",
						"<http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#SolarGeneration>")
				.addWhere("?entity", "j2:isModeledBy", "?model").addWhere("?model", "j5:hasModelVariable", "?num")
				.addWhere("?num", "a", "j3:BusNumber").addWhere("?num", "j2:hasValue", "?vnum")
				.addWhere("?vnum", "j2:numericalValue", "?BusNumbervalue")

				.addWhere("?entity", "j7:hasGISCoordinateSystem", "?coorsys")
				.addWhere("?coorsys", "j7:hasProjectedCoordinate_x", "?x")
				.addWhere("?x", "j2:hasValue", "?xval").addWhere("?xval", "j2:numericalValue", "?valueofx")
				.addWhere("?coorsys", "j7:hasProjectedCoordinate_y", "?y")
				.addWhere("?y", "j2:hasValue", "?yval").addWhere("?yval", "j2:numericalValue", "?valueofy")
				.buildString();

		String trueQuery = "SELECT ?p ?o\r\n" +
				"{ \r\n" +
				"  <http://nasa.dataincubator.org/spacecraft/1968-089A> ?p ?o\r\n" +
				"}";
		assertTrue(InputValidator.checkIfValidQuery(queryString));
		assertTrue(InputValidator.checkIfValidQuery(trueQuery));

	}

	/**
	 * tests if it's a SPARQL update
	 * 
	 * @throws ParseException
	 */
	@Test
	public void testIfUpdate() throws ParseException {
		WhereBuilder where = new WhereBuilder()
				.addWhere("?s", "?p", "?o")
				.addFilter(
						"?s = <http://www.theworldavatar.com/kb/species/species.owl#species_1> && ?p = <http://www.w3.org/2008/05/skos#altLabel>");

		// Build update
		UpdateBuilder builder = new UpdateBuilder();

		// Add where
		builder.addInsert("?s", "?p", "TEST")
				.addDelete("?s", "?p", "?o")
				.addWhere(where);
		String trueUpdate = builder.buildRequest().toString();

		assertTrue(InputValidator.checkIfValidUpdate(trueUpdate));
		String trueQuery = "SELECT ?p ?o\r\n" +
				"{ \r\n" +
				"  <http://nasa.dataincubator.org/spacecraft/1968-089A> ?p ?o\r\n" +
				"}";
		assertFalse(InputValidator.checkIfValidUpdate(trueQuery));
	}

	/**
	 * tests if computer getTime is working and thus file got updated since 1970
	 * 
	 */
	@Test
	public void testIfFileUpdated() {

		long timeLast = 0;// Date.getTime() returns no of miliseconds since 1970.
		assertTrue(InputValidator.checkIfFileGotUpdated(filePath, timeLast));
	}

	// TODO: Vishvak test if JSON, test if Integer
	@Test
	public void testIfInteger() {
		assertTrue(InputValidator.checkIfInteger("9"));
		assertFalse(InputValidator.checkIfInteger("0.09"));
	}

	@Test
	public void testIfJSON() {
		JSONObject jo = new JSONObject().put("key", "value");
		assertTrue(InputValidator.checkIfValidJSONObject(jo.toString()));
		assertTrue(InputValidator.checkIfValidJSONObject("{jo:jijiji}"));
		assertFalse(InputValidator.checkIfValidJSONObject("{jo:;:jijiji}"));
	}
}
