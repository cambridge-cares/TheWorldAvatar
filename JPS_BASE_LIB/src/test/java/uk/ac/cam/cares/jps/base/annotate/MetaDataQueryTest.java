package uk.ac.cam.cares.jps.base.annotate;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;

import org.junit.Assert;
import org.junit.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.discovery.MediaType;

public class MetaDataQueryTest {

    @Test
    public void testGetSparqlQueryResources() {
        // Two cases are distinguished. One with non null arguments and one with null
        // arguments

        String expected_nullArg = "PREFIX dcterms:<http://purl.org/dc/terms/>\r\n"
                + "PREFIX xsd:<http://www.w3.org/2001/XMLSchema#>\r\n"
                + "SELECT ?resource ?mediatype ?creationTime ?agent ?simulationTime ?scenario \r\n"
                + "WHERE { \r\n"
                + "OPTIONAL {?resource dcterms:format ?mediatype .}. \r\n"
                + "OPTIONAL {?resource dcterms:created ?creationTime .}. \r\n"
                + "OPTIONAL {?resource dcterms:creator ?agent .}. \r\n"
                + "OPTIONAL {?resource dcterms:date ?simulationTime .}. \r\n"
                + "OPTIONAL {?resource dcterms:isPartOf ?scenario .}. \r\n"
                + "} ORDER BY DESC(?creationTime) \r\n"
                + "LIMIT 1000";

        String actual_nullArg = MetaDataQuery.getSparqlQueryResources(null, null,
                null, null, null, null, null, null);

        String expected = "PREFIX dcterms:<http://purl.org/dc/terms/>\r\n"
                + "PREFIX xsd:<http://www.w3.org/2001/XMLSchema#>\r\n"
                + "SELECT ?resource ?mediatype ?creationTime ?agent ?simulationTime ?scenario \r\n"
                + "WHERE { \r\n"
                + "OPTIONAL {?resource dcterms:format ?mediatype .}. \r\n"
                + "?resource dcterms:format \"text/turtle\" . \r\n"
                + "OPTIONAL {?resource dcterms:created ?creationTime .}. \r\n"
                + "FILTER ( ?creationTime >= \"testFromCreationTime\"^^xsd:dateTime ) \r\n"
                + "FILTER ( ?creationTime <= \"testToCreationTime\"^^xsd:dateTime ) \r\n"
                + "OPTIONAL {?resource dcterms:creator ?agent .}. \r\n"
                + "?resource dcterms:creator <testIriCreatingAgent> . \r\n"
                + "OPTIONAL {?resource dcterms:date ?simulationTime .}. \r\n"
                + "FILTER ( ?simulationTime >= \"testFromSimulationTime\"^^xsd:dateTime ) \r\n"
                + "FILTER ( ?simulationTime <= \"testToSimulationTime\"^^xsd:dateTime ) \r\n"
                + "OPTIONAL {?resource dcterms:isPartOf ?scenario .}. \r\n"
                + "?resource dcterms:isPartOf <testIriScenario> . \r\n"
                + "?resource dcterms:subject <topic1> ."
                + "?resource dcterms:subject <topic2> ."
                + "?resource dcterms:subject <topic3> ."
                + "} ORDER BY DESC(?creationTime) \r\n"
                + "LIMIT 1000";

        String actual = MetaDataQuery.getSparqlQueryResources(MediaType.TEXT_TURTLE, "testFromCreationTime",
                "testToCreationTime",
                "testIriCreatingAgent", "testFromSimulationTime", "testToSimulationTime", "testIriScenario",
                Arrays.asList("topic1", "topic2", "topic3"));

        Assert.assertEquals(expected_nullArg, actual_nullArg);
        Assert.assertEquals(expected, actual);
    }

    @Test
    public void testGetSparqlQueryResourcesOldRepository() {
        // Two cases are distinguished. One with non null arguments and one with null
        // arguments

        String expected_nullArg = "PREFIX dcterms:<http://purl.org/dc/terms/>\r\n"
                + "PREFIX xsd:<http://www.w3.org/2001/XMLSchema#>\r\n"
                + "PREFIX j1:<https://www.w3.org/2006/time#> \r\n"
                + "SELECT ?resource ?mediatype ?creationTime ?agent ?simulationTime ?scenario \r\n"
                + "WHERE { \r\n"
                + "OPTIONAL {?resource dcterms:format ?mediatype .}. \r\n"
                + "OPTIONAL {?resource dcterms:created ?creationTime .}. \r\n"
                + "OPTIONAL {?resource dcterms:creator ?agent .}. \r\n"
                + "OPTIONAL {?resource j1:hasTime ?inst .}. \r\n"
                + "OPTIONAL {?inst j1:inXSDDateTime ?simulationTime .}. \r\n"
                + "FILTER ( regex(str(?resource), \"C://JPS_DATA/workingdir/JPS_SCENARIO\") ) \r\n"
                + "OPTIONAL {?resource dcterms:isPartOf ?scenario .}. \r\n"
                + "} ORDER BY DESC(?simulationTime) \r\n"
                + "LIMIT 1000";

        String actual_nullArg = MetaDataQuery.getSparqlQueryResourcesOldRepository(null, null,
                null, null, null, null, null, null);

        String expected = "PREFIX dcterms:<http://purl.org/dc/terms/>\r\n"
                + "PREFIX xsd:<http://www.w3.org/2001/XMLSchema#>\r\n"
                + "PREFIX j1:<https://www.w3.org/2006/time#> \r\n"
                + "SELECT ?resource ?mediatype ?creationTime ?agent ?simulationTime ?scenario \r\n"
                + "WHERE { \r\n"
                + "OPTIONAL {?resource dcterms:format ?mediatype .}. \r\n"
                + "?resource dcterms:format \"text/turtle\" . \r\n"
                + "OPTIONAL {?resource dcterms:created ?creationTime .}. \r\n"
                + "FILTER ( ?creationTime >= \"testFromCreationTime\"^^xsd:dateTime ) \r\n"
                + "FILTER ( ?creationTime <= \"testToCreationTime\"^^xsd:dateTime ) \r\n"
                + "OPTIONAL {?resource dcterms:creator ?agent .}. \r\n"
                + "?resource dcterms:creator <testIriCreatingAgent> . \r\n"
                + "OPTIONAL {?resource j1:hasTime ?inst .}. \r\n"
                + "OPTIONAL {?inst j1:inXSDDateTime ?simulationTime .}. \r\n"
                + "FILTER ( ?simulationTime >= \"testFromSimulationTime\"^^xsd:dateTime ) \r\n"
                + "FILTER ( ?simulationTime <= \"testToSimulationTime\"^^xsd:dateTime ) \r\n"
                + "FILTER ( regex(str(?resource), \"C://JPS_DATA/workingdir/JPS_SCENARIO\") ) \r\n"
                + "OPTIONAL {?resource dcterms:isPartOf ?scenario .}. \r\n"
                + "?resource dcterms:isPartOf <testIriScenario> . \r\n"
                + "?resource dcterms:subject <topic1> . \r\n"
                + "?resource dcterms:subject <topic2> . \r\n"
                + "?resource dcterms:subject <topic3> . \r\n"
                + "} ORDER BY DESC(?simulationTime) \r\n"
                + "LIMIT 1000";

        String actual = MetaDataQuery.getSparqlQueryResourcesOldRepository(MediaType.TEXT_TURTLE,
                "testFromCreationTime",
                "testToCreationTime", "testIriCreatingAgent", "testFromSimulationTime",
                "testToSimulationTime", "testIriScenario", Arrays.asList("topic1", "topic2", "topic3"));

        Assert.assertEquals(expected_nullArg, actual_nullArg);
        Assert.assertEquals(expected, actual);

    }

    @Test // This unit test is for the query method that accepts one argument only
    public void testQuery1() {
        String sparql = "test";
        String expected = "queryResultString";
        // String meta_url= KeyValueMap.getInstance().get(IKeys.DATASET_META_URL);
        try (MockedStatic<MetaDataQuery> mtq = Mockito.mockStatic(MetaDataQuery.class)) {
            mtq.when(() -> MetaDataQuery.query(sparql)).thenReturn(expected);
            String actual = MetaDataQuery.query(sparql);
            Assert.assertEquals(expected, actual);
        }
    }

    @Test // This unit test is for the query method that accepts two arguments only
    public void testQuery2() {
        // Two cases are distinguished. One with an empty metadataseturl and one which
        // is not

        // The case with a non null metadataseturl also has to be mocked because if the
        // server is
        // inaccessible an HTTP exception is thrown
        String sparql = "PREFIX dcterms:<http://purl.org/dc/terms/> \r\n"
                + "PREFIX xsd:<http://www.w3.org/2001/XMLSchema#> \r\n"
                + "PREFIX j1:<https://www.w3.org/2006/time#> \r\n"
                + "SELECT ?resource ?mediatype ?creationTime ?agent ?simulationTime ?scenario \r\n"
                + "WHERE { \r\n"
                + "OPTIONAL {?resource dcterms:format ?mediatype .}. \r\n"
                + "OPTIONAL {?resource dcterms:created ?creationTime .}. \r\n"
                + "OPTIONAL {?resource dcterms:creator ?agent .}. \r\n"
                + "OPTIONAL {?resource j1:hasTime ?inst .}. \r\n"
                + "OPTIONAL {?inst j1:inXSDDateTime ?simulationTime .}. \r\n"
                + "FILTER ( regex(str(?resource), \"C://JPS_DATA/workingdir/JPS_SCENARIO\") ) \r\n"
                + "OPTIONAL {?resource dcterms:isPartOf ?scenario .}. \r\n"
                + "} ORDER BY DESC(?simulationTime) \r\n"
                + "LIMIT 1000";
        String metadataSetUrl = KeyValueMap.getInstance().get(IKeys.URL_RDF_METADATA);
        String expected1 = "expectedResult";
        try (MockedStatic<MetaDataQuery> mtq = Mockito.mockStatic(MetaDataQuery.class)) {
            mtq.when(() -> MetaDataQuery.query(sparql, metadataSetUrl)).thenReturn(expected1);
            String actual1 = MetaDataQuery.query(sparql, metadataSetUrl);
            Assert.assertEquals(expected1, actual1);
        }
        // String meta_url= KeyValueMap.getInstance().get(IKeys.DATASET_META_URL);
        String expected2 = "queryResultString";
        try (MockedStatic<MetaDataQuery> mtq = Mockito.mockStatic(MetaDataQuery.class)) {
            mtq.when(() -> MetaDataQuery.query(sparql, "")).thenReturn(expected2);
            String actual2 = MetaDataQuery.query(sparql, "");
            Assert.assertEquals(expected2, actual2);
        }
    }

    @Test // This unit test is for the queryResources method that accepts eight arguments
    public void testQueryResources1() {

        String sparql1 = "PREFIX dcterms:<http://purl.org/dc/terms/>\r\n"
                + "PREFIX xsd:<http://www.w3.org/2001/XMLSchema#>\r\n"
                + "SELECT ?resource ?mediatype ?creationTime ?agent ?simulationTime ?scenario \r\n"
                + "WHERE { \r\n"
                + "OPTIONAL {?resource dcterms:format ?mediatype .}. \r\n"
                + "?resource dcterms:format \"text/turtle\" . \r\n"
                + "OPTIONAL {?resource dcterms:created ?creationTime .}. \r\n"
                + "FILTER ( ?creationTime >= \"testFromCreationTime\"^^xsd:dateTime ) \r\n"
                + "FILTER ( ?creationTime <= \"testToCreationTime\"^^xsd:dateTime ) \r\n"
                + "OPTIONAL {?resource dcterms:creator ?agent .}. \r\n"
                + "?resource dcterms:creator <testIriCreatingAgent> . \r\n"
                + "OPTIONAL {?resource dcterms:date ?simulationTime .}. \r\n"
                + "FILTER ( ?simulationTime >= \"testFromSimulationTime\"^^xsd:dateTime ) \r\n"
                + "FILTER ( ?simulationTime <= \"testToSimulationTime\"^^xsd:dateTime ) \r\n"
                + "OPTIONAL {?resource dcterms:isPartOf ?scenario .}. \r\n"
                + "?resource dcterms:isPartOf <testIriScenario> . \r\n"
                + "?resource dcterms:subject <topic1> ."
                + "} ORDER BY DESC(?creationTime) \r\n"
                + "LIMIT 1000";

        String expected = "queryResultString";

        try (MockedStatic<MetaDataQuery> mtq = Mockito.mockStatic(MetaDataQuery.class, Mockito.CALLS_REAL_METHODS)) {
            mtq.when(() -> MetaDataQuery.query(sparql1)).thenReturn(expected);

            String actual = MetaDataQuery.queryResources(MediaType.TEXT_TURTLE, "testFromCreationTime",
                    "testToCreationTime",
                    "testIriCreatingAgent", "testFromSimulationTime", "testToSimulationTime",
                    "testIriScenario", Arrays.asList("topic1"));

            Assert.assertEquals(expected, actual);
        }
    }

    @Test // This unit test is for the queryResources method that accepts only three
          // arguments
    public void testQueryResources2() {

        String sparql1 = "PREFIX dcterms:<http://purl.org/dc/terms/>\r\n"
                + "PREFIX xsd:<http://www.w3.org/2001/XMLSchema#>\r\n"
                + "SELECT ?resource ?mediatype ?creationTime ?agent ?simulationTime ?scenario \r\n"
                + "WHERE { \r\n"
                + "OPTIONAL {?resource dcterms:format ?mediatype .}. \r\n"
                + "OPTIONAL {?resource dcterms:created ?creationTime .}. \r\n"
                + "OPTIONAL {?resource dcterms:creator ?agent .}. \r\n"
                + "?resource dcterms:creator <testIriCreatingAgent> . \r\n"
                + "OPTIONAL {?resource dcterms:date ?simulationTime .}. \r\n"
                + "FILTER ( ?simulationTime >= \"testFromSimulationTime\"^^xsd:dateTime ) \r\n"
                + "FILTER ( ?simulationTime <= \"testToSimulationTime\"^^xsd:dateTime ) \r\n"
                + "OPTIONAL {?resource dcterms:isPartOf ?scenario .}. \r\n"
                + "} ORDER BY DESC(?creationTime) \r\n"
                + "LIMIT 1000";

        String expected = "queryResultString";
        try (MockedStatic<MetaDataQuery> mtq = Mockito.mockStatic(MetaDataQuery.class, Mockito.CALLS_REAL_METHODS)) {
            mtq.when(() -> MetaDataQuery.query(sparql1)).thenReturn(expected);
            String actual = MetaDataQuery.queryResources("testIriCreatingAgent", "testFromSimulationTime",
                    "testToSimulationTime");
            Assert.assertEquals(expected, actual);
        }

    }

    @Test
    public void testQueryOldResources() {
        // This method has to be mocked because if the server is not accessible then a
        // HTTP exception is thrown.
        String expected = "queryResultString";
        try (MockedStatic<MetaDataQuery> mtq = Mockito.mockStatic(MetaDataQuery.class)) {
            mtq.when(() -> MetaDataQuery.queryOldResources(null, null, null, null)).thenReturn(expected);
            String actual = MetaDataQuery.queryOldResources(null, null, null, null);
            Assert.assertEquals(expected, actual);
        }
    }

    @Test
    public void testGetSparqlMetaDataResources()
            throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        MetaDataQuery mdq = new MetaDataQuery();
        Assert.assertNotNull(mdq.getClass().getDeclaredMethod("getSparqlMetaDataResources", String.class, String.class,
                String.class));
        Method getSpaMetDatRes = mdq.getClass().getDeclaredMethod("getSparqlMetaDataResources", String.class,
                String.class, String.class);
        getSpaMetDatRes.setAccessible(true);

        // First case where agentIri is not null and both time arguments contain the
        // string none
        String expectedQuery1 = "PREFIX j1:<https://www.w3.org/2006/time#>"
                + "PREFIX xsd:<http://www.w3.org/2001/XMLSchema#>"
                + "PREFIX dcterms:<http://purl.org/dc/terms/>"
                + "SELECT ?directory ?time ?agent "
                + "WHERE {?directory j1:hasTime ?inst ."
                + "?inst j1:inXSDDateTime ?time."
                + "OPTIONAL {?directory dcterms:creator ?agent .}.\r\n"
                + "?directory dcterms:creator <testAgentIri> . \r\n"
                + "}"
                + "ORDER BY DESC(?time) "
                + "LIMIT 1";

        // Second case where agentIri is null but one time argument contains the string
        // none
        String expectedQuery2 = "PREFIX j1:<https://www.w3.org/2006/time#> "
                + "PREFIX xsd:<http://www.w3.org/2001/XMLSchema#> "
                + "PREFIX dcterms:<http://purl.org/dc/terms/> "
                + "SELECT ?directory ?time ?agent "
                + "WHERE {?directory j1:hasTime ?inst ."
                + "?inst j1:inXSDDateTime ?time."
                + "filter(?time <= \"" + "" + "\"^^xsd:dateTime )."
                + "OPTIONAL {?directory dcterms:creator ?agent .}.\r\n"
                + ""
                + "}"
                + "ORDER BY DESC(?time) "
                + "LIMIT 1";

        String actualQuery1 = getSpaMetDatRes.invoke(mdq, "testTimeFromnone", "testTimeTonone", "testAgentIri")
                .toString();
        String actualQuery2 = getSpaMetDatRes.invoke(mdq, "testTimeFromnone", "", null).toString();
        Assert.assertEquals(expectedQuery1, actualQuery1);
        Assert.assertEquals(expectedQuery2, actualQuery2);

    }
}
