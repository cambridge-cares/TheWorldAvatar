package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent.ontobim;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class SelectQueryBuilderTest {
    @Test
    void testGenUtilityTSSelectQuery() {
        String query = SelectQueryBuilder.genUtilityTSSelectQuery();
        String expected = "SELECT  ?subject\n" +
                "WHERE\n" +
                "  { ?subject  ontotimeseries:hasTimeSeries  ?timeseries}";
        assertTrue(query.contains(expected));
    }

    @Test
    void testGenZoneSelectQuery() {
        String query = SelectQueryBuilder.genZoneSelectQuery();
        String expected = "SELECT  ?zone ?name\n" +
                "WHERE\n" +
                "  {   { ?zone  rdf:type  bot:Building}\n" +
                "    UNION\n" +
                "      { ?zone  rdf:type    bot:Storey ;\n" +
                "               rdfs:label  ?name\n" +
                "        FILTER ( ?name IN (\"Attic\", \"Ground\", \"Level 1\") )\n" +
                "      }}";
        assertTrue(query.contains(expected));
    }

    @Test
    void testGenMeterSelectQuery() {
        String query = SelectQueryBuilder.genMeterSelectQuery();
        String expected = "SELECT  ?elecmeter ?watermeter ?oilmeter\n" +
                "WHERE\n" +
                "  { OPTIONAL\n" +
                "      { ?elecmeter  rdf:type  bim:ElectricityMeter}\n" +
                "    OPTIONAL\n" +
                "      { ?watermeter  rdf:type  bim:WaterMeter}\n" +
                "    OPTIONAL\n" +
                "      { ?oilmeter  rdf:type  bim:OilMeter}";
        assertTrue(query.contains(expected));
    }
}