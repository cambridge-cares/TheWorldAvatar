package uk.ac.cam.cares.jps.agent.sparql;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class OntologyConstantTest {
    @Test
    void testFields() {
        // Prefix
        assertEquals("twa", OntologyConstant.BASE_PREFIX);
        assertEquals("rdf", OntologyConstant.RDF_PREFIX);
        assertEquals("om", OntologyConstant.OM_PREFIX);
        assertEquals("skos", OntologyConstant.SKOS_PREFIX);
        assertEquals("qudt", OntologyConstant.QUDT_PREFIX);
        assertEquals("ontoubemmp", OntologyConstant.UBEMMP_PREFIX);
        assertEquals("heatnetwork", OntologyConstant.ONTOHEATNETWORK_PREFIX);
        assertEquals("ontocape", OntologyConstant.ONTOCAPE_PREFIX);
        // URI
        assertEquals("http://www.w3.org/1999/02/22-rdf-syntax-ns#", OntologyConstant.RDF_URI);
        assertEquals("http://www.ontology-of-units-of-measure.org/resource/om-2/", OntologyConstant.OM_URI);
        assertEquals("http://www.w3.org/2004/02/skos/core#", OntologyConstant.SKOS_URI);
        assertEquals("http://qudt.org/schema/qudt/", OntologyConstant.QUDT_URI);
        assertEquals("https://www.theworldavatar.com/kg/ontoubemmp/", OntologyConstant.UBEMMP_URI);
        assertEquals("https://www.theworldavatar.com/kg/ontoheatnetwork/", OntologyConstant.ONTOHEATNETWORK_URI);
        assertEquals("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_performance/economic_performance.owl#", OntologyConstant.ONTOCAPE_URI);
        // Classes/ Properties
        assertEquals(OntologyConstant.OM_PREFIX + ":kilowattHour", OntologyConstant.OM_KWH);
        assertEquals(OntologyConstant.UBEMMP_PREFIX + ":ElectricityConsumption", OntologyConstant.UBEMMP_ELECCONSUMPTION);
        assertEquals(OntologyConstant.ONTOHEATNETWORK_PREFIX + ":CostInTimeInterval", OntologyConstant.HEATNETWORK_COST_INTERVAL);
        assertEquals(OntologyConstant.RDF_PREFIX + ":type", OntologyConstant.RDFTYPE);
        assertEquals(OntologyConstant.OM_PREFIX + ":hasValue", OntologyConstant.OM_HASVALUE);
        assertEquals(OntologyConstant.OM_PREFIX + ":hasUnit", OntologyConstant.OM_HASUNIT);
        assertEquals(OntologyConstant.SKOS_PREFIX + ":notation", OntologyConstant.SKOS_NOTATION);
        assertEquals(OntologyConstant.UBEMMP_PREFIX + ":consumesUtilities", OntologyConstant.UBEMMP_CONSUMES_UTILITIES);
        assertEquals(OntologyConstant.ONTOCAPE_PREFIX + ":hasUtilityCost", OntologyConstant.ONTOCAPE_HAS_UTILITY_COST);
        // Literal
        assertEquals("'kW.h'^^" + OntologyConstant.QUDT_PREFIX + ":UCUMcs", OntologyConstant.KWH_LITERAL);
    }
}