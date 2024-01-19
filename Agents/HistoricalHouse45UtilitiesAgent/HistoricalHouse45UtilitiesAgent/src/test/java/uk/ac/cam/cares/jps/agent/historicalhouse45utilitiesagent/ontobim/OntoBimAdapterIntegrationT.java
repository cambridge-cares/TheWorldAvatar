package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent.ontobim;

import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.sparql.exec.UpdateExec;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;
import uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent.BuildingIRISingleton;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

@Testcontainers
class OntoBimAdapterIntegrationT {
    @Container
    private static final GenericContainer blazegraph = new GenericContainer(DockerImageName.parse("nawer/blazegraph:latest"))
            .withExposedPorts(9999);
    private static String endpoint;
    private static String baseURI;
    // URI
    private static final String TEST_URI = "http://www.test.com/kb/test/";
    private static final String RDF_URI = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    private static final String RDFS_URI = "http://www.w3.org/2000/01/rdf-schema#";
    private static final String BOT_URI = "https://w3id.org/bot#";
    private static final String SAREF_URI = "https://saref.etsi.org/core/";
    private static final String OM_URI = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    private static final String SKOS_URI = "http://www.w3.org/2004/02/skos/core#";
    private static final String QUDT_URI = "http://qudt.org/schema/qudt/";
    private static final String TIMESERIES_URI = "https://www.theworldavatar.com/kg/ontotimeseries/";
    private static final String UBEMMP_URI = "https://www.theworldavatar.com/kg/ontoubemmp/";
    private static final String ONTOBUILTENV_URI = "http://www.theworldavatar.com/ontology/ontobuiltenv/OntoBuiltEnv.owl#";
    private static final String BIM_URI = "http://www.theworldavatar.com/ontology/ontobim/ontoBIM#";
    // Instances
    private static final String electricityConsumptionQuantityInst = "MonthlyElectricityConsumption_Quantity_";
    private static final String waterConsumptionQuantityInst = "MonthlyWaterConsumption_Quantity_";
    private static final String oilConsumptionQuantityInst = "MonthlyOilConsumption_Quantity_";
    private static final String timeseriesIRI = TEST_URI + "Timeseries_5912";
    private static final String electricityGroundFloorMeasureIRI = TEST_URI + "GroundFloor_MonthlyElectricityConsumption_718";
    private static final String electricityFirstFloorMeasureIRI = TEST_URI + "FirstFloor_MonthlyElectricityConsumption_897";
    private static final String electricityAtticMeasureIRI = TEST_URI + "Attic_MonthlyElectricityConsumption_11132";
    private static final String waterMeasureIRI = TEST_URI + "MonthlyWaterConsumption_2151";
    private static final String oilMeasureIRI = TEST_URI + "MonthlyOilConsumption_2974";
    private static final String electricitySensorDisplayMeasureIRI = TEST_URI + "MonthlyElectricityConsumption_Sensordisplay_771";
    private static final String waterSensorDisplayMeasureIRI = TEST_URI + "MonthlyWaterConsumption_Sensordisplay_3810";
    private static final String oilSensorDisplayMeasureIRI = TEST_URI + "MonthlyOilConsumption_Sensordisplay_5012";
    private static final String citygmlIRI = TEST_URI + "CityGML_Building_123";
    private static final String buildingIRI = TEST_URI + "Building_5515";
    private static final String firstFloorIRI = TEST_URI + "Storey_5781";
    private static final String groundFloorIRI = TEST_URI + "Storey_162";
    private static final String atticIRI = TEST_URI + "Storey_836";
    private static final String elecMeterIRI = TEST_URI + "ElectricityMeter_44124";
    private static final String waterMeterIRI = TEST_URI + "WaterMeter_9521";
    private static final String oilMeterIRI = TEST_URI + "OilMeter_3175";
    private static final String meteringFunctionInst = SAREF_URI + "MeteringFunction_";
    // Properties
    private static final String TIMESERIES_HASTS = TIMESERIES_URI + "hasTimeSeries";
    private static final String RDF_TYPE = RDF_URI + "type";
    private static final String RDFS_LABEL = RDFS_URI + "label";
    private static final String SAREF_HAS_FUNCTION = SAREF_URI + "hasFunction";
    private static final String OM_HASUNIT = OM_URI + "hasUnit";
    private static final String OM_HASVALUE = OM_URI + "hasValue";
    private static final String OM_ENERGY = OM_URI + "Energy";
    private static final String OM_KWH = OM_URI + "kilowattHour";
    private static final String OM_VOLUME = OM_URI + "Volume";
    private static final String OM_CBM = OM_URI + "cubicMetre";
    private static final String OM_LITRE = OM_URI + "litre";
    private static final String SKOS_NOTATION = SKOS_URI + "notation";
    private static final String UBEMMP_CONSUMES_UTILITIES = UBEMMP_URI + "consumesUtilities";
    private static final String ONTOBUILTENV_HAS_CITYGML_REP = ONTOBUILTENV_URI + "hasOntoCityGMLRepresentation";
    // Classes
    private static final String BOT_BUILDING = BOT_URI + "Building";
    private static final String BOT_Storey = BOT_URI + "Storey";
    private static final String BIM_ELEC_METER = BIM_URI + "ElectricityMeter";
    private static final String BIM_WATER_METER = BIM_URI + "WaterMeter";
    private static final String BIM_OIL_METER = BIM_URI + "OilMeter";
    // Literal
    private static final String cubicMetreLiteral = "m3^^" + QUDT_URI + "UCUMcs";
    private static final String kwhLiteral = "kW.h^^" + QUDT_URI + "UCUMcs";
    private static final String litreLiteral = "L^^" + QUDT_URI + "UCUMcs";

    private static final String WHITESPACE = " ";
    private static final String NEWLINE = "\n";

    @BeforeAll
    static void init() {
        // Get host and port name to form KG endpoint
        endpoint = "http://" + blazegraph.getHost() + ":" + blazegraph.getFirstMappedPort();
        baseURI = endpoint + "/blazegraph/namespace/kb/"; // Default namespace is "kb"
        endpoint = baseURI + "sparql";
    }

    @BeforeEach
    void resetKG() {
        String deleteQuery = "DELETE WHERE {?s ?p ?o}";
        UpdateExec.service(endpoint).update(deleteQuery).execute();
        createSampleData();
    }

    @Test
    void testAddSupplementaryTriples() {
        // Set up the singleton and required IRIs
        BuildingIRISingleton singleton = BuildingIRISingleton.getInstance();
        singleton.setOntoCityGmlBuildingIri(citygmlIRI);
        // Invoke the test function
        OntoBimAdapter.addSupplementaryTriples(endpoint, endpoint, singleton);
        // Query the triples from KG to ensure function works as expected
        String results = queryFromKG();
        List<String> expected = genExpectedResults();
        expected.forEach(line -> assertTrue(results.contains(line)));
    }

    @Test
    void testAddSupplementaryTriplesWithNoCityGMLIri() {
        // Set up the singleton and required IRIs
        BuildingIRISingleton singleton = BuildingIRISingleton.getInstance();
        singleton.setOntoCityGmlBuildingIri("");
        // Invoke the test function
        OntoBimAdapter.addSupplementaryTriples(endpoint, endpoint, singleton);
        // Query the triples from KG to test that the following triple is not instantiated
        String results = queryFromKG();
        assertFalse(results.contains(buildingIRI + WHITESPACE + ONTOBUILTENV_HAS_CITYGML_REP + WHITESPACE + citygmlIRI));
    }

    private static void createSampleData() {
        StringBuilder insertQuery = new StringBuilder();
        insertQuery.append("INSERT DATA {");
        addInsertStatement(electricityGroundFloorMeasureIRI, TIMESERIES_HASTS, timeseriesIRI, insertQuery, false);
        addInsertStatement(electricityFirstFloorMeasureIRI, TIMESERIES_HASTS, timeseriesIRI, insertQuery, false);
        addInsertStatement(electricityAtticMeasureIRI, TIMESERIES_HASTS, timeseriesIRI, insertQuery, false);
        addInsertStatement(waterMeasureIRI, TIMESERIES_HASTS, timeseriesIRI, insertQuery, false);
        addInsertStatement(oilMeasureIRI, TIMESERIES_HASTS, timeseriesIRI, insertQuery, false);
        addInsertStatement(electricitySensorDisplayMeasureIRI, TIMESERIES_HASTS, timeseriesIRI, insertQuery, false);
        addInsertStatement(waterSensorDisplayMeasureIRI, TIMESERIES_HASTS, timeseriesIRI, insertQuery, false);
        addInsertStatement(oilSensorDisplayMeasureIRI, TIMESERIES_HASTS, timeseriesIRI, insertQuery, false);
        addInsertStatement(buildingIRI, RDF_TYPE, BOT_BUILDING, insertQuery, false);
        addInsertStatement(groundFloorIRI, RDF_TYPE, BOT_Storey, insertQuery, false);
        addInsertStatement(groundFloorIRI, RDFS_LABEL, "Ground", insertQuery, true);
        addInsertStatement(firstFloorIRI, RDF_TYPE, BOT_Storey, insertQuery, false);
        addInsertStatement(firstFloorIRI, RDFS_LABEL, "Level 1", insertQuery, true);
        addInsertStatement(atticIRI, RDF_TYPE, BOT_Storey, insertQuery, false);
        addInsertStatement(atticIRI, RDFS_LABEL, "Attic", insertQuery, true);
        addInsertStatement(elecMeterIRI, RDF_TYPE, BIM_ELEC_METER, insertQuery, false);
        addInsertStatement(waterMeterIRI, RDF_TYPE, BIM_WATER_METER, insertQuery, false);
        addInsertStatement(oilMeterIRI, RDF_TYPE, BIM_OIL_METER, insertQuery, false);
        insertQuery.append("}");
        QueryHandler.insertToEndpoint(insertQuery.toString(), endpoint);
    }

    private static void addInsertStatement(String subject, String predicate, String object, StringBuilder insertQuery, boolean isLiteral) {
        insertQuery.append("<" + subject + "> <" + predicate + "> ");
        if (isLiteral) {
            insertQuery.append("\"" + object + "\" .\n");
        } else {
            insertQuery.append("<" + object + "> .\n");
        }
    }

    private static String queryFromKG() {
        String selectQuery = "SELECT * WHERE {?s ?p ?o}";
        ResultSet queryResults = QueryHandler.execSelectQuery(selectQuery, endpoint);
        StringBuilder results = new StringBuilder();
        while (queryResults.hasNext()) {
            QuerySolution soln = queryResults.nextSolution();
            String subject = soln.get("?s").toString();
            String pred = soln.get("?p").toString();
            String object = soln.get("?o").toString();
            results.append(subject + WHITESPACE + pred + WHITESPACE + object + NEWLINE);
        }
        return results.toString();
    }

    private static List<String> genExpectedResults() {
        List<String> expected = new ArrayList();
        expected.add(buildingIRI + WHITESPACE + ONTOBUILTENV_HAS_CITYGML_REP + WHITESPACE + citygmlIRI);

        expected.add(groundFloorIRI + WHITESPACE + UBEMMP_CONSUMES_UTILITIES + WHITESPACE + baseURI + electricityConsumptionQuantityInst);
        expected.add(electricityGroundFloorMeasureIRI + WHITESPACE + TIMESERIES_HASTS + WHITESPACE + timeseriesIRI);
        expected.add(electricityGroundFloorMeasureIRI + WHITESPACE + OM_HASUNIT + WHITESPACE + OM_KWH);
        expected.add(electricityGroundFloorMeasureIRI + WHITESPACE + RDF_TYPE + WHITESPACE + OM_ENERGY);

        expected.add(firstFloorIRI + WHITESPACE + UBEMMP_CONSUMES_UTILITIES + WHITESPACE + baseURI + electricityConsumptionQuantityInst);
        expected.add(electricityFirstFloorMeasureIRI + WHITESPACE + TIMESERIES_HASTS + WHITESPACE + timeseriesIRI);
        expected.add(electricityFirstFloorMeasureIRI + WHITESPACE + OM_HASUNIT + WHITESPACE + OM_KWH);
        expected.add(electricityFirstFloorMeasureIRI + WHITESPACE + RDF_TYPE + WHITESPACE + OM_ENERGY);

        expected.add(atticIRI + WHITESPACE + UBEMMP_CONSUMES_UTILITIES + WHITESPACE + baseURI + electricityConsumptionQuantityInst);
        expected.add(electricityAtticMeasureIRI + WHITESPACE + TIMESERIES_HASTS + WHITESPACE + timeseriesIRI);
        expected.add(electricityAtticMeasureIRI + WHITESPACE + OM_HASUNIT + WHITESPACE + OM_KWH);
        expected.add(electricityAtticMeasureIRI + WHITESPACE + RDF_TYPE + WHITESPACE + OM_ENERGY);

        expected.add(buildingIRI + WHITESPACE + UBEMMP_CONSUMES_UTILITIES + WHITESPACE + baseURI + waterConsumptionQuantityInst);
        expected.add(waterMeasureIRI + WHITESPACE + TIMESERIES_HASTS + WHITESPACE + timeseriesIRI);
        expected.add(waterMeasureIRI + WHITESPACE + OM_HASUNIT + WHITESPACE + OM_CBM);
        expected.add(waterMeasureIRI + WHITESPACE + RDF_TYPE + WHITESPACE + OM_VOLUME);

        expected.add(buildingIRI + WHITESPACE + UBEMMP_CONSUMES_UTILITIES + WHITESPACE + baseURI + oilConsumptionQuantityInst);
        expected.add(oilMeasureIRI + WHITESPACE + TIMESERIES_HASTS + WHITESPACE + timeseriesIRI);
        expected.add(oilMeasureIRI + WHITESPACE + OM_HASUNIT + WHITESPACE + OM_LITRE);
        expected.add(oilMeasureIRI + WHITESPACE + RDF_TYPE + WHITESPACE + OM_VOLUME);

        expected.add(OM_CBM + WHITESPACE + SKOS_NOTATION + WHITESPACE + cubicMetreLiteral);
        expected.add(OM_KWH + WHITESPACE + SKOS_NOTATION + WHITESPACE + kwhLiteral);
        expected.add(OM_LITRE + WHITESPACE + SKOS_NOTATION + WHITESPACE + litreLiteral);

        expected.add(elecMeterIRI + WHITESPACE + SAREF_HAS_FUNCTION + WHITESPACE + meteringFunctionInst);
        expected.add(WHITESPACE + OM_HASVALUE + WHITESPACE + electricitySensorDisplayMeasureIRI);
        expected.add(waterMeterIRI + WHITESPACE + SAREF_HAS_FUNCTION + WHITESPACE + meteringFunctionInst);
        expected.add(WHITESPACE + OM_HASVALUE + WHITESPACE + waterSensorDisplayMeasureIRI);
        expected.add(oilMeterIRI + WHITESPACE + SAREF_HAS_FUNCTION + WHITESPACE + meteringFunctionInst);
        expected.add(WHITESPACE + OM_HASVALUE + WHITESPACE + oilSensorDisplayMeasureIRI);
        return expected;
    }
}