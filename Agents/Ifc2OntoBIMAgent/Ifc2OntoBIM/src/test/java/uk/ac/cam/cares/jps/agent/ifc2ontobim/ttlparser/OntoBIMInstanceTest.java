package uk.ac.cam.cares.jps.agent.ifc2ontobim.ttlparser;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class OntoBIMInstanceTest {
    @Test
    void testRetrieveOntoBimName() {
        assertEquals("Project", OntoBIMInstance.retrieveOntoBimName("IfcProject"));
        assertEquals("Roof", OntoBIMInstance.retrieveOntoBimName("IfcRoof"));
        assertEquals("GeometricRepresentationContext", OntoBIMInstance.retrieveOntoBimName("IfcGeometricRepresentationContext"));
        assertEquals("Polyline", OntoBIMInstance.retrieveOntoBimName("IfcPolyline"));
        assertEquals("CartesianPoint", OntoBIMInstance.retrieveOntoBimName("IfcCartesianPoint"));
        assertEquals("LineVertex", OntoBIMInstance.retrieveOntoBimName("IfcCartesianPoint_List"));
    }
}