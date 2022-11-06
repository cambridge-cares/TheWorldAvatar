package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent.ontobim;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class SelectQueryBuilderTest {
    @Test
    void genSelectQuery() {
        String query = SelectQueryBuilder.genSelectQuery();
        String expected = "SELECT  ?subject\n" +
                "WHERE\n" +
                "  { ?subject  ontotimeseries:hasTimeSeries  ?timeseries}";
        assertTrue(query.contains(expected));
    }
}