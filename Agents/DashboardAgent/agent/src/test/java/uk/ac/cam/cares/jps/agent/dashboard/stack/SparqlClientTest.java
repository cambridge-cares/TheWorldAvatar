package uk.ac.cam.cares.jps.agent.dashboard.stack;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.IntegrationTestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.datamodel.Organisation;
import uk.ac.cam.cares.jps.agent.dashboard.datamodel.OrganisationTest;
import uk.ac.cam.cares.jps.agent.dashboard.datamodel.ThresholdTest;
import uk.ac.cam.cares.jps.agent.dashboard.stack.sparql.utils.SparqlQueryTest;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class SparqlClientTest {
    public static final String SAMPLE_ORGANISATION_NAME = "Org 1";
    public static final String SAMPLE_BUILDING_INSTANCE = TestUtils.genInstance("Building");
    public static final String SAMPLE_OFFICE_INSTANCE = TestUtils.genInstance("Office");
    public static final String SAMPLE_OFFICE_NAME = "Admin Office";
    public static final String SAMPLE_OFFICE_DIRECTOR_ROOM_INSTANCE = TestUtils.genInstance("Room");
    public static final String SAMPLE_OFFICE_DIRECTOR_ROOM_NAME = "Director room";
    public static final String SAMPLE_OFFICE_DIRECTOR_ROOM_TEMPERATURE_MEASURE = TestUtils.genInstance("Measure");
    public static final String SAMPLE_OFFICE_DIRECTOR_ROOM_TEMPERATURE_TIME_SERIES_IRI = TestUtils.genTimeSeriesInstance();
    public static final String SAMPLE_OFFICE_STAFF_ROOM_INSTANCE = TestUtils.genInstance("Room");
    public static final String SAMPLE_OFFICE_STAFF_ROOM_NAME = "Staff room";
    public static final String SAMPLE_OFFICE_STAFF_ROOM_REL_HUMIDITY_MEASURE = TestUtils.genInstance("Measure");
    public static final String SAMPLE_OFFICE_STAFF_ROOM_REL_HUMIDITY_TIME_SERIES_IRI = TestUtils.genTimeSeriesInstance();
    public static final String SAMPLE_OFFICE_STAFF_ROOM_TEMPERATURE_MEASURE = TestUtils.genInstance("Measure");
    public static final String SAMPLE_OFFICE_STAFF_ROOM_TEMPERATURE_TIME_SERIES_IRI = TestUtils.genTimeSeriesInstance();
    public static final String SAMPLE_OFFICE_STORAGE_ROOM_INSTANCE = TestUtils.genInstance("Room");
    public static final String SAMPLE_OFFICE_STORAGE_ROOM_NAME = "Staff room";
    public static final String SAMPLE_OFFICE_SYSTEM_INSTANCE = TestUtils.genInstance("HVAC");
    public static final String SAMPLE_OFFICE_SYSTEM_NAME = "HVAC system";
    public static final String SAMPLE_OFFICE_SYSTEM_ELECTRICITY_CONSUMPTION_MEASURE = TestUtils.genInstance("Measure");
    public static final String SAMPLE_OFFICE_SYSTEM_ELECTRICITY_CONSUMPTION_TIME_SERIES_IRI = TestUtils.genTimeSeriesInstance();
    public static final String SAMPLE_OFFICE_SUB_SYSTEM_INSTANCE = TestUtils.genInstance("BTU");
    public static final String SAMPLE_OFFICE_SUB_SYSTEM_NAME = "BTU unit";
    public static final String SAMPLE_OFFICE_SUB_SYSTEM_ELECTRICITY_CONSUMPTION_MEASURE = TestUtils.genInstance("Measure");
    public static final String SAMPLE_OFFICE_SUB_SYSTEM_ELECTRICITY_CONSUMPTION_TIME_SERIES_IRI = TestUtils.genTimeSeriesInstance();
    public static final String TEMPERATURE = "Temperature";
    public static final String TEMPERATURE_UNIT = "degree";
    public static final String RELATIVE_HUMIDITY = "Relative Humidity";
    public static final double TEMPERATURE_MIN_THRESHOLD = 21.1;
    public static final double TEMPERATURE_MAX_THRESHOLD = 25.6;
    public static final double RELATIVE_HUMIDITY_MIN_THRESHOLD = 50.1;
    public static final double RELATIVE_HUMIDITY_MAX_THRESHOLD = 75.7;
    public static final String SAMPLE_LAB_INSTANCE = TestUtils.genInstance("Laboratory");
    public static final String SAMPLE_LAB_NAME = "Generic Laboratory";
    public static final String SAMPLE_LAB_BIO_ROOM_INSTANCE = TestUtils.genInstance("Room");
    public static final String SAMPLE_LAB_PILOT_ROOM_INSTANCE = TestUtils.genInstance("Room");
    public static final String SAMPLE_LAB_FRIDGE_NAME = "Chemical Cooling Unit";
    public static final String SAMPLE_LAB_FRIDGE_TYPE = "Fridge";
    public static final String SAMPLE_LAB_FRIDGE_INSTANCE = TestUtils.genInstance(SAMPLE_LAB_FRIDGE_TYPE);
    public static final String FRIDGE_ELEC_CONSUMPION_MEASURE = TestUtils.genInstance("Measure");
    public static final String FRIDGE_ELEC_CONSUMPION_TIME_SERIES_IRI = TestUtils.genTimeSeriesInstance();
    public static final String SAMPLE_LAB_SMART_SENSOR_NAME = "Sample Sensor Kit";
    public static final String SAMPLE_LAB_SMART_SENSOR_TYPE = "SmartSensor";
    public static final String SAMPLE_LAB_SMART_SENSOR_INSTANCE = TestUtils.genInstance(SAMPLE_LAB_SMART_SENSOR_TYPE);
    public static final String SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_ONE_INSTANCE = TestUtils.genInstance("TemperatureSensor");
    public static final String SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_TWO_INSTANCE = TestUtils.genInstance("StatusSensor");
    public static final String SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_THREE_INSTANCE = TestUtils.genInstance("ElectricitySensor");
    public static final String SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_FOUR_INSTANCE = TestUtils.genInstance("HumiditySensor");
    public static final String SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_FIVE_INSTANCE = TestUtils.genInstance("StatusSensor");
    public static final String SMART_SENSOR_ONE_TEMPERATURE_MEASURE = TestUtils.genInstance("Measure");
    public static final String SMART_SENSOR_ONE_TEMPERATURE_TIME_SERIES_IRI = TestUtils.genTimeSeriesInstance();
    public static final String SMART_SENSOR_TWO_STATE_MEASURE = TestUtils.genInstance("Measure");
    public static final String SMART_SENSOR_TWO_STATE_TIME_SERIES_IRI = TestUtils.genTimeSeriesInstance();
    public static final String SMART_SUB_SENSOR_THREE_ELECTRICITY_CONSUMPTION_MEASURE = TestUtils.genInstance("Measure");
    public static final String SMART_SUB_SENSOR_THREE_ELECTRICITY_CONSUMPTION_TIME_SERIES_IRI = TestUtils.genTimeSeriesInstance();
    public static final String SMART_SUB_SENSOR_FOUR_REL_HUMIDITY_MEASURE = TestUtils.genInstance("Measure");
    public static final String SMART_SUB_SENSOR_FOUR_REL_HUMIDITY_TIME_SERIES_IRI = TestUtils.genTimeSeriesInstance();
    public static final String SMART_SUB_SENSOR_FOUR_REL_HUMIDITY_STATE_MEASURE = TestUtils.genInstance("Measure");
    public static final String SMART_SUB_SENSOR_FOUR_REL_HUMIDITY_STATE_TIME_SERIES_IRI = TestUtils.genTimeSeriesInstance();
    public static final String SAMPLE_LAB_MAKE_UP_AIR_UNIT_NAME = "MAU-03";
    public static final String SAMPLE_LAB_MAKE_UP_AIR_UNIT_TYPE = "MakeUpAirUnit";
    public static final String MAKEUP_UNIT_TEMP_MEASURE = TestUtils.genInstance("Measure");
    public static final String MAKEUP_UNIT_TEMP_TIME_SERIES_IRI = TestUtils.genTimeSeriesInstance();
    public static final String SAMPLE_LAB_MAKE_UP_AIR_UNIT_INSTANCE = TestUtils.genInstance(SAMPLE_LAB_MAKE_UP_AIR_UNIT_TYPE);
    public static final String SAMPLE_LAB_MAKE_UP_AIR_UNIT_DUCT_INSTANCE = TestUtils.genInstance("Duct");
    public static final String SAMPLE_LAB_MAKE_UP_AIR_UNIT_DUCT_NAME = "Duct-5";
    public static final String STATE = "State";
    public static final String HUMIDITY_STATE = "Humidity State";
    public static final String ELECTRICITY_CONSUMPTION = "ElectricityConsumption";
    public static final String ELECTRICITY_CONSUMPTION_UNIT = "kwh";

    @BeforeAll
    static void setupNamespaces() {
        IntegrationTestUtils.createNamespace(IntegrationTestUtils.SPATIAL_ZONE_NAMESPACE);
        IntegrationTestUtils.createNamespace(IntegrationTestUtils.GENERAL_NAMESPACE);
    }

    @AfterEach
    void clearNamespace() {
        IntegrationTestUtils.updateEndpoint(IntegrationTestUtils.GENERAL_SPARQL_ENDPOINT, IntegrationTestUtils.SPARQL_DELETE);
        IntegrationTestUtils.updateEndpoint(IntegrationTestUtils.SPATIAL_ZONE_SPARQL_ENDPOINT, IntegrationTestUtils.SPARQL_DELETE);
    }


    @Test
    void testGetAllOrganisations_NoMeasures() {
        // Create a new Sparql client - note there should be no username and password for the test container
        SparqlClient client = new SparqlClient(IntegrationTestUtils.SPARQL_ENDPOINT, "", "");
        // Execute method
        List<Organisation> results = client.getAllOrganisations();
        // Verify if no organisation is retrieved if there are no related triples
        assertEquals(0, results.size());
    }

    @Test
    void testGetAllOrganisations_AllMeasures() {
        // Insert these triples into the blazegraph
        insertFacilityTriples(IntegrationTestUtils.SPATIAL_ZONE_SPARQL_ENDPOINT);
        insertAssetTriples(IntegrationTestUtils.GENERAL_SPARQL_ENDPOINT, true);
        insertSystemTriples(IntegrationTestUtils.SPATIAL_ZONE_SPARQL_ENDPOINT);
        // Create a new Sparql client - note there should be no username and password for the test container
        SparqlClient client = new SparqlClient(IntegrationTestUtils.SPARQL_ENDPOINT, "", "");
        // Execute method
        List<Organisation> results = client.getAllOrganisations();
        // Verify if result is as expected
        assertEquals(1, results.size()); // There should only be one organisation
        assertEquals(SAMPLE_ORGANISATION_NAME, results.get(0).getName());
        results.forEach(organisation -> {
            OrganisationTest.verifyOrganisationModel(organisation, OrganisationTest.genExpectedOrgOutputs(false,
                    new String[]{SAMPLE_LAB_SMART_SENSOR_NAME, SAMPLE_LAB_SMART_SENSOR_TYPE, TEMPERATURE, TEMPERATURE_UNIT, SMART_SENSOR_ONE_TEMPERATURE_MEASURE, SMART_SENSOR_ONE_TEMPERATURE_TIME_SERIES_IRI},
                    new String[]{SAMPLE_LAB_SMART_SENSOR_NAME, SAMPLE_LAB_SMART_SENSOR_TYPE, STATE, null, SMART_SENSOR_TWO_STATE_MEASURE, SMART_SENSOR_TWO_STATE_TIME_SERIES_IRI},
                    new String[]{SAMPLE_LAB_SMART_SENSOR_NAME, SAMPLE_LAB_SMART_SENSOR_TYPE, ELECTRICITY_CONSUMPTION, ELECTRICITY_CONSUMPTION_UNIT, SMART_SUB_SENSOR_THREE_ELECTRICITY_CONSUMPTION_MEASURE, SMART_SUB_SENSOR_THREE_ELECTRICITY_CONSUMPTION_TIME_SERIES_IRI},
                    new String[]{SAMPLE_LAB_SMART_SENSOR_NAME, SAMPLE_LAB_SMART_SENSOR_TYPE, RELATIVE_HUMIDITY, null, SMART_SUB_SENSOR_FOUR_REL_HUMIDITY_MEASURE, SMART_SUB_SENSOR_FOUR_REL_HUMIDITY_TIME_SERIES_IRI},
                    new String[]{SAMPLE_LAB_SMART_SENSOR_NAME, SAMPLE_LAB_SMART_SENSOR_TYPE, HUMIDITY_STATE, null, SMART_SUB_SENSOR_FOUR_REL_HUMIDITY_STATE_MEASURE, SMART_SUB_SENSOR_FOUR_REL_HUMIDITY_STATE_TIME_SERIES_IRI},
                    new String[]{SAMPLE_LAB_FRIDGE_NAME, SAMPLE_LAB_FRIDGE_TYPE, ELECTRICITY_CONSUMPTION, ELECTRICITY_CONSUMPTION_UNIT, FRIDGE_ELEC_CONSUMPION_MEASURE, FRIDGE_ELEC_CONSUMPION_TIME_SERIES_IRI},
                    new String[]{SAMPLE_LAB_MAKE_UP_AIR_UNIT_NAME + " " + SAMPLE_LAB_MAKE_UP_AIR_UNIT_DUCT_NAME, SAMPLE_LAB_MAKE_UP_AIR_UNIT_TYPE, TEMPERATURE, TEMPERATURE_UNIT, MAKEUP_UNIT_TEMP_MEASURE, MAKEUP_UNIT_TEMP_TIME_SERIES_IRI},
                    new String[]{SAMPLE_OFFICE_SYSTEM_NAME, StringHelper.SYSTEM_KEY, ELECTRICITY_CONSUMPTION, ELECTRICITY_CONSUMPTION_UNIT, SAMPLE_OFFICE_SYSTEM_ELECTRICITY_CONSUMPTION_MEASURE, SAMPLE_OFFICE_SYSTEM_ELECTRICITY_CONSUMPTION_TIME_SERIES_IRI},
                    new String[]{SAMPLE_OFFICE_SUB_SYSTEM_NAME, StringHelper.SYSTEM_KEY, ELECTRICITY_CONSUMPTION, ELECTRICITY_CONSUMPTION_UNIT, SAMPLE_OFFICE_SUB_SYSTEM_ELECTRICITY_CONSUMPTION_MEASURE, SAMPLE_OFFICE_SUB_SYSTEM_ELECTRICITY_CONSUMPTION_TIME_SERIES_IRI},
                    new String[]{SAMPLE_OFFICE_DIRECTOR_ROOM_NAME, StringHelper.ROOM_KEY, TEMPERATURE, TEMPERATURE_UNIT, SAMPLE_OFFICE_DIRECTOR_ROOM_TEMPERATURE_MEASURE, SAMPLE_OFFICE_DIRECTOR_ROOM_TEMPERATURE_TIME_SERIES_IRI},
                    new String[]{SAMPLE_OFFICE_STAFF_ROOM_NAME, StringHelper.ROOM_KEY, TEMPERATURE, TEMPERATURE_UNIT, SAMPLE_OFFICE_STAFF_ROOM_TEMPERATURE_MEASURE, SAMPLE_OFFICE_STAFF_ROOM_TEMPERATURE_TIME_SERIES_IRI},
                    new String[]{SAMPLE_OFFICE_STAFF_ROOM_NAME, StringHelper.ROOM_KEY, RELATIVE_HUMIDITY, null, SAMPLE_OFFICE_STAFF_ROOM_REL_HUMIDITY_MEASURE, SAMPLE_OFFICE_STAFF_ROOM_REL_HUMIDITY_TIME_SERIES_IRI}
            ), ThresholdTest.genExpectedThresholds(
                    new String[]{RELATIVE_HUMIDITY, String.valueOf(RELATIVE_HUMIDITY_MIN_THRESHOLD), String.valueOf(RELATIVE_HUMIDITY_MAX_THRESHOLD)},
                    new String[]{TEMPERATURE, String.valueOf(TEMPERATURE_MIN_THRESHOLD), String.valueOf(TEMPERATURE_MAX_THRESHOLD)}

            ));
        });
    }

    @Test
    void testGetAllOrganisations_OnlyAssetMeasures() {
        // Insert these triples into the blazegraph
        insertFacilityTriples(IntegrationTestUtils.SPATIAL_ZONE_SPARQL_ENDPOINT);
        insertAssetTriples(IntegrationTestUtils.GENERAL_SPARQL_ENDPOINT, true);
        // Create a new Sparql client - note there should be no username and password for the test container
        SparqlClient client = new SparqlClient(IntegrationTestUtils.SPARQL_ENDPOINT, "", "");
        // Execute method
        List<Organisation> results = client.getAllOrganisations();
        // Verify if result is as expected
        assertEquals(1, results.size()); // There should only be one organisation
        assertEquals(SAMPLE_ORGANISATION_NAME, results.get(0).getName());
        results.forEach(organisation -> {
            OrganisationTest.verifyOrganisationModel(organisation, OrganisationTest.genExpectedOrgOutputs(false,
                    new String[]{SAMPLE_LAB_SMART_SENSOR_NAME, SAMPLE_LAB_SMART_SENSOR_TYPE, TEMPERATURE, TEMPERATURE_UNIT, SMART_SENSOR_ONE_TEMPERATURE_MEASURE, SMART_SENSOR_ONE_TEMPERATURE_TIME_SERIES_IRI},
                    new String[]{SAMPLE_LAB_SMART_SENSOR_NAME, SAMPLE_LAB_SMART_SENSOR_TYPE, STATE, null, SMART_SENSOR_TWO_STATE_MEASURE, SMART_SENSOR_TWO_STATE_TIME_SERIES_IRI},
                    new String[]{SAMPLE_LAB_SMART_SENSOR_NAME, SAMPLE_LAB_SMART_SENSOR_TYPE, ELECTRICITY_CONSUMPTION, ELECTRICITY_CONSUMPTION_UNIT, SMART_SUB_SENSOR_THREE_ELECTRICITY_CONSUMPTION_MEASURE, SMART_SUB_SENSOR_THREE_ELECTRICITY_CONSUMPTION_TIME_SERIES_IRI},
                    new String[]{SAMPLE_LAB_SMART_SENSOR_NAME, SAMPLE_LAB_SMART_SENSOR_TYPE, RELATIVE_HUMIDITY, null, SMART_SUB_SENSOR_FOUR_REL_HUMIDITY_MEASURE, SMART_SUB_SENSOR_FOUR_REL_HUMIDITY_TIME_SERIES_IRI},
                    new String[]{SAMPLE_LAB_SMART_SENSOR_NAME, SAMPLE_LAB_SMART_SENSOR_TYPE, HUMIDITY_STATE, null, SMART_SUB_SENSOR_FOUR_REL_HUMIDITY_STATE_MEASURE, SMART_SUB_SENSOR_FOUR_REL_HUMIDITY_STATE_TIME_SERIES_IRI},
                    new String[]{SAMPLE_LAB_FRIDGE_NAME, SAMPLE_LAB_FRIDGE_TYPE, ELECTRICITY_CONSUMPTION, ELECTRICITY_CONSUMPTION_UNIT, FRIDGE_ELEC_CONSUMPION_MEASURE, FRIDGE_ELEC_CONSUMPION_TIME_SERIES_IRI},
                    new String[]{SAMPLE_LAB_MAKE_UP_AIR_UNIT_NAME + " " + SAMPLE_LAB_MAKE_UP_AIR_UNIT_DUCT_NAME, SAMPLE_LAB_MAKE_UP_AIR_UNIT_TYPE, TEMPERATURE, TEMPERATURE_UNIT, MAKEUP_UNIT_TEMP_MEASURE, MAKEUP_UNIT_TEMP_TIME_SERIES_IRI}
            ));
        });
    }

    @Test
    void testGetAllOrganisations_OnlyRoomMeasures() {
        // Insert these triples into the blazegraph
        insertFacilityTriples(IntegrationTestUtils.SPATIAL_ZONE_SPARQL_ENDPOINT);
        // Create a new Sparql client - note there should be no username and password for the test container
        SparqlClient client = new SparqlClient(IntegrationTestUtils.SPARQL_ENDPOINT, "", "");
        // Execute method
        List<Organisation> results = client.getAllOrganisations();
        // Verify if result is as expected
        assertEquals(1, results.size()); // There should only be one organisation
        assertEquals(SAMPLE_ORGANISATION_NAME, results.get(0).getName());
        results.forEach(organisation -> {
            OrganisationTest.verifyOrganisationModel(organisation, OrganisationTest.genExpectedOrgOutputs(false,
                    new String[]{SAMPLE_OFFICE_DIRECTOR_ROOM_NAME, StringHelper.ROOM_KEY, TEMPERATURE, TEMPERATURE_UNIT, SAMPLE_OFFICE_DIRECTOR_ROOM_TEMPERATURE_MEASURE, SAMPLE_OFFICE_DIRECTOR_ROOM_TEMPERATURE_TIME_SERIES_IRI},
                    new String[]{SAMPLE_OFFICE_STAFF_ROOM_NAME, StringHelper.ROOM_KEY, TEMPERATURE, TEMPERATURE_UNIT, SAMPLE_OFFICE_STAFF_ROOM_TEMPERATURE_MEASURE, SAMPLE_OFFICE_STAFF_ROOM_TEMPERATURE_TIME_SERIES_IRI},
                    new String[]{SAMPLE_OFFICE_STAFF_ROOM_NAME, StringHelper.ROOM_KEY, RELATIVE_HUMIDITY, null, SAMPLE_OFFICE_STAFF_ROOM_REL_HUMIDITY_MEASURE, SAMPLE_OFFICE_STAFF_ROOM_REL_HUMIDITY_TIME_SERIES_IRI}
            ), ThresholdTest.genExpectedThresholds(
                    new String[]{RELATIVE_HUMIDITY, String.valueOf(RELATIVE_HUMIDITY_MIN_THRESHOLD), String.valueOf(RELATIVE_HUMIDITY_MAX_THRESHOLD)},
                    new String[]{TEMPERATURE, String.valueOf(TEMPERATURE_MIN_THRESHOLD), String.valueOf(TEMPERATURE_MAX_THRESHOLD)}

            ));
        });
    }

    @Test
    void testGetAllOrganisations_OnlyRoomAndSystemMeasures() {
        // Insert these triples into the blazegraph
        insertFacilityTriples(IntegrationTestUtils.SPATIAL_ZONE_SPARQL_ENDPOINT);
        insertSystemTriples(IntegrationTestUtils.SPATIAL_ZONE_SPARQL_ENDPOINT);
        // Create a new Sparql client - note there should be no username and password for the test container
        SparqlClient client = new SparqlClient(IntegrationTestUtils.SPARQL_ENDPOINT, "", "");
        // Execute method
        List<Organisation> results = client.getAllOrganisations();
        // Verify if result is as expected
        assertEquals(1, results.size()); // There should only be one organisation
        assertEquals(SAMPLE_ORGANISATION_NAME, results.get(0).getName());
        results.forEach(organisation -> {
            OrganisationTest.verifyOrganisationModel(organisation, OrganisationTest.genExpectedOrgOutputs(false,
                    new String[]{SAMPLE_OFFICE_SYSTEM_NAME, StringHelper.SYSTEM_KEY, ELECTRICITY_CONSUMPTION, ELECTRICITY_CONSUMPTION_UNIT, SAMPLE_OFFICE_SYSTEM_ELECTRICITY_CONSUMPTION_MEASURE, SAMPLE_OFFICE_SYSTEM_ELECTRICITY_CONSUMPTION_TIME_SERIES_IRI},
                    new String[]{SAMPLE_OFFICE_SUB_SYSTEM_NAME, StringHelper.SYSTEM_KEY, ELECTRICITY_CONSUMPTION, ELECTRICITY_CONSUMPTION_UNIT, SAMPLE_OFFICE_SUB_SYSTEM_ELECTRICITY_CONSUMPTION_MEASURE, SAMPLE_OFFICE_SUB_SYSTEM_ELECTRICITY_CONSUMPTION_TIME_SERIES_IRI},
                    new String[]{SAMPLE_OFFICE_DIRECTOR_ROOM_NAME, StringHelper.ROOM_KEY, TEMPERATURE, TEMPERATURE_UNIT, SAMPLE_OFFICE_DIRECTOR_ROOM_TEMPERATURE_MEASURE, SAMPLE_OFFICE_DIRECTOR_ROOM_TEMPERATURE_TIME_SERIES_IRI},
                    new String[]{SAMPLE_OFFICE_STAFF_ROOM_NAME, StringHelper.ROOM_KEY, TEMPERATURE, TEMPERATURE_UNIT, SAMPLE_OFFICE_STAFF_ROOM_TEMPERATURE_MEASURE, SAMPLE_OFFICE_STAFF_ROOM_TEMPERATURE_TIME_SERIES_IRI},
                    new String[]{SAMPLE_OFFICE_STAFF_ROOM_NAME, StringHelper.ROOM_KEY, RELATIVE_HUMIDITY, null, SAMPLE_OFFICE_STAFF_ROOM_REL_HUMIDITY_MEASURE, SAMPLE_OFFICE_STAFF_ROOM_REL_HUMIDITY_TIME_SERIES_IRI}
            ), ThresholdTest.genExpectedThresholds(
                    new String[]{RELATIVE_HUMIDITY, String.valueOf(RELATIVE_HUMIDITY_MIN_THRESHOLD), String.valueOf(RELATIVE_HUMIDITY_MAX_THRESHOLD)},
                    new String[]{TEMPERATURE, String.valueOf(TEMPERATURE_MIN_THRESHOLD), String.valueOf(TEMPERATURE_MAX_THRESHOLD)}
            ));
        });
    }

    public static void insertFacilityTriples(String endpoint) {
        StringBuilder builder = new StringBuilder();
        String organisationRepresentation = TestUtils.genInstance("Organisation");
        String organisationNameRepresentation = TestUtils.genInstance("OrganisationName");
        String directorRoomRepresentation = TestUtils.genInstance("Representation");
        String staffRoomRepresentation = TestUtils.genInstance("Representation");
        String storageRoomRepresentation = TestUtils.genInstance("Representation");
        builder.append(SparqlQueryTest.genExpectedPrefixesString())
                .append("INSERT DATA {<")
                // A building has lab and office facility
                .append(SAMPLE_BUILDING_INSTANCE).append("> rdf:type bot:Building;")
                .append("ontobim:hasFacility <").append(SAMPLE_LAB_INSTANCE).append(">;")
                .append("ontobim:hasFacility <").append(SAMPLE_OFFICE_INSTANCE).append(">.")
                // The organisation managing the facilities
                .append("<").append(organisationRepresentation).append("> <https://www.omg.org/spec/Commons/Designators/hasName> <").append(organisationNameRepresentation).append(">.")
                .append("<").append(organisationNameRepresentation).append("> rdfs:label \"").append(SAMPLE_ORGANISATION_NAME).append("\".")
                // Lab has assets
                .append("<").append(SAMPLE_LAB_INSTANCE).append("> rdfs:label \"").append(SAMPLE_LAB_NAME).append("\";")
                .append("<https://www.theworldavatar.com/kg/ontoassetmanagement/isManagedBy> <").append(organisationRepresentation).append(">;")
                .append("ontobim:hasRoom <").append(SAMPLE_LAB_BIO_ROOM_INSTANCE).append(">;")
                .append("ontobim:hasRoom <").append(SAMPLE_LAB_PILOT_ROOM_INSTANCE).append(">.")
                .append("<").append(SAMPLE_LAB_BIO_ROOM_INSTANCE).append("> rdf:type ontobim:Room.")
                .append("<").append(SAMPLE_LAB_PILOT_ROOM_INSTANCE).append("> rdf:type ontobim:Room.")
                // Asset measures should be placed in another endpoint
                .append("<").append(SAMPLE_LAB_BIO_ROOM_INSTANCE).append("> bot:containsElement <").append(SAMPLE_LAB_SMART_SENSOR_INSTANCE).append(">.")
                .append("<").append(SAMPLE_LAB_SMART_SENSOR_INSTANCE).append("> rdfs:label \"").append(SAMPLE_LAB_SMART_SENSOR_NAME).append("\";")
                .append("rdf:type ontodevice:SmartSensor.")
                .append("<").append(SAMPLE_LAB_PILOT_ROOM_INSTANCE).append("> bot:containsElement <").append(SAMPLE_LAB_MAKE_UP_AIR_UNIT_INSTANCE).append(">;")
                .append("bot:containsElement <").append(SAMPLE_LAB_FRIDGE_INSTANCE).append(">.")
                .append("<").append(SAMPLE_LAB_MAKE_UP_AIR_UNIT_INSTANCE).append("> rdfs:label \"").append(SAMPLE_LAB_MAKE_UP_AIR_UNIT_NAME).append("\";")
                .append("rdf:type ontodevice:MakeUpAirUnit.")
                .append("<").append(SAMPLE_LAB_FRIDGE_INSTANCE).append("> rdfs:label \"").append(SAMPLE_LAB_FRIDGE_NAME).append("\";")
                .append("rdf:type ontodevice:Fridge.")
                // Office has no assets but rooms has measures
                // Thresholds
                .append(genThresholdTriples(SAMPLE_OFFICE_INSTANCE, "ontodevice:hasMinThreshold", "om:Temperature", TEMPERATURE_MIN_THRESHOLD))
                .append(genThresholdTriples(SAMPLE_OFFICE_INSTANCE, "ontodevice:hasMaxThreshold", "om:Temperature", TEMPERATURE_MAX_THRESHOLD))
                .append(genThresholdTriples(SAMPLE_OFFICE_INSTANCE, "ontodevice:hasMinThreshold", "om:RelativeHumidity", RELATIVE_HUMIDITY_MIN_THRESHOLD))
                .append(genThresholdTriples(SAMPLE_OFFICE_INSTANCE, "ontodevice:hasMaxThreshold", "om:RelativeHumidity", RELATIVE_HUMIDITY_MAX_THRESHOLD))
                // Link facility to rooms
                .append("<").append(SAMPLE_OFFICE_INSTANCE).append("> rdfs:label \"").append(SAMPLE_OFFICE_NAME).append("\";")
                .append("<https://www.theworldavatar.com/kg/ontoassetmanagement/isManagedBy> <").append(organisationRepresentation).append(">;")
                .append("ontobim:hasRoom <").append(SAMPLE_OFFICE_DIRECTOR_ROOM_INSTANCE).append(">;")
                .append("ontobim:hasRoom <").append(SAMPLE_OFFICE_STAFF_ROOM_INSTANCE).append(">;")
                .append("ontobim:hasRoom <").append(SAMPLE_OFFICE_STORAGE_ROOM_INSTANCE).append(">.")
                // Director room should only have temperature
                .append("<").append(SAMPLE_OFFICE_DIRECTOR_ROOM_INSTANCE).append("> rdf:type ontobim:Room;")
                .append("ontobim:hasIfcRepresentation <").append(directorRoomRepresentation).append(">.")
                .append("<").append(directorRoomRepresentation).append("> rdfs:label \"").append(SAMPLE_OFFICE_DIRECTOR_ROOM_NAME).append("\".")
                .append(genMeasureTriples(SAMPLE_OFFICE_DIRECTOR_ROOM_INSTANCE, "ontodevice:hasTemperature", TEMPERATURE, TEMPERATURE_UNIT,
                        SAMPLE_OFFICE_DIRECTOR_ROOM_TEMPERATURE_MEASURE, SAMPLE_OFFICE_DIRECTOR_ROOM_TEMPERATURE_TIME_SERIES_IRI))
                // Staff room should have both temperature and humidity
                .append("<").append(SAMPLE_OFFICE_STAFF_ROOM_INSTANCE).append("> rdf:type ontobim:Room;")
                .append("ontobim:hasIfcRepresentation <").append(staffRoomRepresentation).append(">.")
                .append("<").append(staffRoomRepresentation).append("> rdfs:label \"").append(SAMPLE_OFFICE_STAFF_ROOM_NAME).append("\".")
                .append(genMeasureTriples(SAMPLE_OFFICE_STAFF_ROOM_INSTANCE, "ontodevice:hasTemperature", TEMPERATURE, TEMPERATURE_UNIT,
                        SAMPLE_OFFICE_STAFF_ROOM_TEMPERATURE_MEASURE, SAMPLE_OFFICE_STAFF_ROOM_TEMPERATURE_TIME_SERIES_IRI))
                .append(genMeasureTriples(SAMPLE_OFFICE_STAFF_ROOM_INSTANCE, "ontodevice:hasRelativeHumidity", RELATIVE_HUMIDITY, "",
                        SAMPLE_OFFICE_STAFF_ROOM_REL_HUMIDITY_MEASURE, SAMPLE_OFFICE_STAFF_ROOM_REL_HUMIDITY_TIME_SERIES_IRI))
                // Storage room has no measures
                .append("<").append(SAMPLE_OFFICE_STORAGE_ROOM_INSTANCE).append("> rdf:type ontobim:Room;")
                .append("ontobim:hasIfcRepresentation <").append(storageRoomRepresentation).append(">.")
                .append("<").append(storageRoomRepresentation).append("> rdfs:label \"").append(SAMPLE_OFFICE_STORAGE_ROOM_NAME).append("\".")
                .append("}");
        IntegrationTestUtils.updateEndpoint(endpoint, builder.toString());
    }

    public static void insertSystemTriples(String endpoint) {
        StringBuilder builder = new StringBuilder();
        builder.append(SparqlQueryTest.genExpectedPrefixesString())
                .append("INSERT DATA {<")
                // Insert a HVAC system
                .append(SAMPLE_OFFICE_INSTANCE).append("> ontotechsystem:containsSystem <").append(SAMPLE_OFFICE_SYSTEM_INSTANCE).append(">.")
                .append("<").append(SAMPLE_OFFICE_SYSTEM_INSTANCE).append("> rdfs:label \"").append(SAMPLE_OFFICE_SYSTEM_NAME).append("\";")
                .append("ontotechsystem:composedOf <").append(SAMPLE_OFFICE_SUB_SYSTEM_INSTANCE).append(">.") // Include its subsystem
                // Generate the measure for the system's energy consumption with actual GIS data
                .append(genMeasureTriples(SAMPLE_OFFICE_SYSTEM_INSTANCE, "ontoubemmp:consumesEnergy", ELECTRICITY_CONSUMPTION, ELECTRICITY_CONSUMPTION_UNIT,
                        SAMPLE_OFFICE_SYSTEM_ELECTRICITY_CONSUMPTION_MEASURE, SAMPLE_OFFICE_SYSTEM_ELECTRICITY_CONSUMPTION_TIME_SERIES_IRI))
                // Generate the measure for the sub-system's energy consumption, this will not be instantiated in the postgis system
                .append("<").append(SAMPLE_OFFICE_SUB_SYSTEM_INSTANCE).append("> rdfs:label \"").append(SAMPLE_OFFICE_SUB_SYSTEM_NAME).append("\".")
                .append(genMeasureTriples(SAMPLE_OFFICE_SUB_SYSTEM_INSTANCE, "ontoubemmp:consumesEnergy", ELECTRICITY_CONSUMPTION, ELECTRICITY_CONSUMPTION_UNIT,
                        SAMPLE_OFFICE_SUB_SYSTEM_ELECTRICITY_CONSUMPTION_MEASURE, SAMPLE_OFFICE_SUB_SYSTEM_ELECTRICITY_CONSUMPTION_TIME_SERIES_IRI))
                .append("}");
        IntegrationTestUtils.updateEndpoint(endpoint, builder.toString());
    }

    public static void insertAssetTriples(String endpoint, boolean isComplex) {
        StringBuilder builder = new StringBuilder();
        String intermediateSensorTwo = TestUtils.genInstance("Sensor");
        String intermediateSensorFour = TestUtils.genInstance("Sensor");
        String intermediateMAUSensor = TestUtils.genInstance("Sensor");
        String operatingRangeInstance = TestUtils.genInstance("OperatingRange");
        String operatingPropertyInstance = TestUtils.genInstance("OperatingProperty");
        builder.append(SparqlQueryTest.genExpectedPrefixesString())
                .append("INSERT DATA {")
                // First sub sensor pattern
                .append("<").append(SAMPLE_LAB_SMART_SENSOR_INSTANCE).append("> ontodevice:sendsSignalTo <").append(SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_ONE_INSTANCE).append(">.")
                .append(genMeasureTriples(SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_ONE_INSTANCE, "ontodevice:measures", TEMPERATURE, TEMPERATURE_UNIT, SMART_SENSOR_ONE_TEMPERATURE_MEASURE, SMART_SENSOR_ONE_TEMPERATURE_TIME_SERIES_IRI));
        if (isComplex) {
            // Second sub sensor pattern
            builder.append("<").append(SAMPLE_LAB_SMART_SENSOR_INSTANCE).append("> ontodevice:sendsSignalTo <").append(intermediateSensorTwo).append(">.")
                    .append("<").append(intermediateSensorTwo).append("> saref:consistsOf <").append(SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_TWO_INSTANCE).append(">.")
                    .append(genMeasureTriples(SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_TWO_INSTANCE, "observes", STATE, "", SMART_SENSOR_TWO_STATE_MEASURE, SMART_SENSOR_TWO_STATE_TIME_SERIES_IRI))
                    // Third sub sensor pattern
                    .append("<").append(SAMPLE_LAB_SMART_SENSOR_INSTANCE).append("> saref:consistsOf <").append(SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_THREE_INSTANCE).append(">.")
                    .append(genMeasureTriples(SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_THREE_INSTANCE, "ontodevice:measures", ELECTRICITY_CONSUMPTION, ELECTRICITY_CONSUMPTION_UNIT, SMART_SUB_SENSOR_THREE_ELECTRICITY_CONSUMPTION_MEASURE, SMART_SUB_SENSOR_THREE_ELECTRICITY_CONSUMPTION_TIME_SERIES_IRI))
                    // Fourth sub sensor pattern
                    .append("<").append(SAMPLE_LAB_SMART_SENSOR_INSTANCE).append("> saref:consistsOf <").append(intermediateSensorFour).append(">.")
                    .append("<").append(intermediateSensorFour).append("> ontodevice:sendsSignalTo <").append(SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_FOUR_INSTANCE).append(">.")
                    .append(genMeasureTriples(SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_FOUR_INSTANCE, "ontodevice:measures", RELATIVE_HUMIDITY, "", SMART_SUB_SENSOR_FOUR_REL_HUMIDITY_MEASURE, SMART_SUB_SENSOR_FOUR_REL_HUMIDITY_TIME_SERIES_IRI))
                    // Fifth sub sensor pattern - directly nested below sensor four
                    .append("<").append(SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_FOUR_INSTANCE).append("> saref:consistsOf <").append(SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_FIVE_INSTANCE).append(">.")
                    .append(genMeasureTriples(SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_FIVE_INSTANCE, "observes", HUMIDITY_STATE, "", SMART_SUB_SENSOR_FOUR_REL_HUMIDITY_STATE_MEASURE, SMART_SUB_SENSOR_FOUR_REL_HUMIDITY_STATE_TIME_SERIES_IRI))
                    // Pattern inclusive of an additional sub element
                    .append("<").append(SAMPLE_LAB_MAKE_UP_AIR_UNIT_INSTANCE).append("> saref:consistsOf <").append(SAMPLE_LAB_MAKE_UP_AIR_UNIT_DUCT_INSTANCE).append(">.")
                    .append(genMeasureTriples(intermediateMAUSensor, "ontodevice:measures", TEMPERATURE, TEMPERATURE_UNIT, MAKEUP_UNIT_TEMP_MEASURE, MAKEUP_UNIT_TEMP_TIME_SERIES_IRI))
                    .append("<").append(intermediateMAUSensor).append("> ontodevice:isAttachedTo <").append(SAMPLE_LAB_MAKE_UP_AIR_UNIT_DUCT_INSTANCE).append(">.")
                    .append("<").append(SAMPLE_LAB_MAKE_UP_AIR_UNIT_DUCT_INSTANCE).append("> rdfs:label \"").append(SAMPLE_LAB_MAKE_UP_AIR_UNIT_DUCT_NAME).append("\".")
                    // For alternate measure paths
                    .append("<").append(SAMPLE_LAB_FRIDGE_INSTANCE).append("> ontodevice:hasOperatingRange <").append(operatingRangeInstance).append(">.")
                    .append("<").append(operatingRangeInstance).append("> ssn:hasOperatingProperty <").append(operatingPropertyInstance).append(">.")
                    .append(genMeasureTriples(operatingPropertyInstance, "ontodevice:hasQuantity", ELECTRICITY_CONSUMPTION, ELECTRICITY_CONSUMPTION_UNIT, FRIDGE_ELEC_CONSUMPION_MEASURE, FRIDGE_ELEC_CONSUMPION_TIME_SERIES_IRI));
        }
        builder.append("}");
        IntegrationTestUtils.updateEndpoint(endpoint, builder.toString());
    }

    private static StringBuilder genMeasureTriples(String item, String measureProperty, String measureName, String unit, String measureInst, String timeSeriesInst) {
        StringBuilder builder = new StringBuilder();
        if (measureProperty.equals("observes")) {
            builder.append("<").append(item).append("> ontodevice:").append(measureProperty).append(" <").append(measureInst).append(">.");
        } else {
            // Quantity instance is only relevant for non-observes property
            String quantityInst = TestUtils.genInstance("Quantity");
            builder.append("<").append(item).append("> ").append(measureProperty).append(" <").append(quantityInst).append(">.")
                    .append("<").append(quantityInst).append("> om:hasValue <").append(measureInst).append(">.");
        }
        builder.append("<").append(measureInst).append("> rdfs:label \"").append(measureName).append("\";")
                .append("ontotimeseries:hasTimeSeries <").append(timeSeriesInst).append(">.");
        if (!unit.equals("")) {
            String unitInst = TestUtils.genInstance("Unit");
            builder.append("<").append(measureInst).append("> om:hasUnit <").append(unitInst).append(">.")
                    .append("<").append(unitInst).append("> om:symbol \"").append(unit).append("\".");
        }
        return builder;
    }

    private static StringBuilder genThresholdTriples(String facility, String measureProperty, String measureType, double thresholdVal) {
        StringBuilder builder = new StringBuilder();
        String thresholdInst = TestUtils.genInstance("Threshold");
        String quantityInst = TestUtils.genInstance("Quantity");
        String measureInst = TestUtils.genInstance("Measure");
        builder.append("<").append(facility).append("> ").append(measureProperty).append(" <").append(thresholdInst).append(">.")
                .append("<").append(thresholdInst).append("> ontodevice:hasQuantity <").append(quantityInst).append(">.")
                .append("<").append(quantityInst).append("> rdf:type ").append(measureType).append(";")
                .append("om:hasValue <").append(measureInst).append(">.")
                .append("<").append(measureInst).append("> om:hasNumericalValue ").append(thresholdVal).append(".");
        return builder;
    }
}