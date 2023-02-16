package uk.ac.cam.cares.jps.agent.ifc2ontobim.ttlparser;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class OntoBIMInstanceTest {
    @Test
    void testRetrieveOntoBimName() {
        assertEquals("IfcProjectRepresentation", OntoBIMInstance.retrieveOntoBimName("IfcProject"));
        assertEquals("IfcSiteRepresentation", OntoBIMInstance.retrieveOntoBimName("IfcSite"));
        assertEquals("IfcBuildingRepresentation", OntoBIMInstance.retrieveOntoBimName("IfcBuilding"));
        assertEquals("IfcStoreyRepresentation", OntoBIMInstance.retrieveOntoBimName("IfcBuildingStorey"));
        assertEquals("IfcRoomRepresentation", OntoBIMInstance.retrieveOntoBimName("IfcSpace"));
        assertEquals("IfcModelRepresentation", OntoBIMInstance.retrieveOntoBimName("IfcRoof"));
        assertEquals("GeometricRepresentationContext", OntoBIMInstance.retrieveOntoBimName("IfcGeometricRepresentationContext"));
        assertEquals("Polyline", OntoBIMInstance.retrieveOntoBimName("IfcPolyline"));
        assertEquals("CartesianPoint", OntoBIMInstance.retrieveOntoBimName("IfcCartesianPoint"));
        assertEquals("LineVertex", OntoBIMInstance.retrieveOntoBimName("IfcCartesianPoint_List"));
    }
}