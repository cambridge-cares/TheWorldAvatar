package uk.ac.cam.cares.jps.base.query.sparql;

import org.junit.Test;

import static org.junit.jupiter.api.Assertions.*;

public class PathsTest {

    @Test
    //A simple test, since Paths.java only defines several strings. Only possible confusion are OCPSYST and OCPSPAC, but those are still just strings (see Prefixes.java)
    //Hence the only thing to test is if those strings actually are the input strings
    public void TestPaths() {
        assertEquals("CLASS", Paths.CLASS);
        assertEquals("OCPSYST",Paths.PVALNUMVAL[0]);
        assertEquals("hasValue",Paths.PVALNUMVAL[1]);
        assertEquals("OCPSYST",Paths.PVALNUMVAL[2]);
        assertEquals("numericalValue",Paths.PVALNUMVAL[3]);
        assertEquals("OCPSPAC", Paths.PGISCOORDX[0]);
        assertEquals("hasGISCoordinateSystem", Paths.PGISCOORDX[1]);
        assertEquals("OCPSPAC", Paths.PGISCOORDX[2]);
        assertEquals("hasProjectedCoordinate_x", Paths.PGISCOORDX[3]);
        assertEquals("OCPSYST", Paths.PGISCOORDX[4]);
        assertEquals("hasValue", Paths.PGISCOORDX[5]);
        assertEquals("OCPSYST", Paths.PGISCOORDX[6]);
        assertEquals("numericalValue", Paths.PGISCOORDX[7]);
        assertEquals("OCPSPAC", Paths.PGISCOORDY[0]);
        assertEquals("hasGISCoordinateSystem", Paths.PGISCOORDY[1]);
        assertEquals("OCPSPAC", Paths.PGISCOORDY[2]);
        assertEquals("hasProjectedCoordinate_y", Paths.PGISCOORDY[3]);
        assertEquals("OCPSYST", Paths.PGISCOORDY[4]);
        assertEquals("hasValue", Paths.PGISCOORDY[5]);
        assertEquals("OCPSYST", Paths.PGISCOORDY[6]);
        assertEquals("numericalValue", Paths.PGISCOORDY[7]);
    }
}