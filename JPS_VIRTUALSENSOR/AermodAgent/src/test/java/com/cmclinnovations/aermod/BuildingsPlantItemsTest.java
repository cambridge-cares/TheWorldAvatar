/* This class queries the knowledge graph for buildings and plant items within a given scope. It uses this information
to run the Buildings Profile Input Program for PRIME (BPIPPRM). The BPIPPRM output file (buildings.dat) is
subsequently used to run AERMOD.
 */

package com.cmclinnovations.aermod;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.Polygon;
import org.locationtech.jts.geom.PrecisionModel;
import org.locationtech.jts.io.ParseException;
import org.locationtech.jts.io.WKTReader;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import java.nio.file.Path;

public class BuildingsPlantItemsTest {

    Path simulationDirectory = Path.of("C:\\Users\\KNAG01\\Dropbox (Cambridge CARES)\\IRP3 CAPRICORN shared folder\\KNAGARAJAN\\Projects\\Dispersion\\Data\\17\\");
//    Two equivalent polygons which define a rectangular region within Jurong Island. The values in wkt are in EPSG:4326/WGS84 coordinates while those in wkt2 are in EPSG:3857 coordinates.
    String wkt = "POLYGON ((1.259121 103.672485, 1.259121 103.716345, 1.281174 103.716345, 1.281174 103.672485, 1.259121 103.672485))" ;
    String wkt2 = "POLYGON ((11540768.2395 140175.9917, 11540768.2395 142631.5238, 11545650.7123 142631.5238, 11545650.7123 140175.9917, 11540768.2395 140175.9917))" ;

    String wkt3 = "POLYGON ((1.217 103.651, 1.217 103.74, 1.30 103.74, 1.30 103.651, 1.217 103.651))" ;
    int nx = 100;
    int ny = 100;

    BuildingsPlantItems bp = new BuildingsPlantItems() ;


    /* Test the initialization code within the constructor of the BuildingsPlantItems class. */
    @Test
    public void testInit() throws org.opengis.util.FactoryException, FactoryException, TransformException, ParseException {

        GeometryFactory geometryFactory = new GeometryFactory(new PrecisionModel(),4326);
        Polygon scope;
        try {
            scope = (Polygon) new WKTReader(geometryFactory).read(wkt3);
        } catch (ParseException p) {
            System.out.println("Failed to parse given EWKT literal");
            return;
        }

        GeometryFactory geometryFactory2 = new GeometryFactory(new PrecisionModel(),3857);
        Polygon scope2;
        try {
            scope2 = (Polygon) new WKTReader(geometryFactory2).read(wkt2);
        } catch (ParseException p) {
            System.out.println("Failed to parse given EWKT literal");
            return;
        }


        int centreZoneNumber = (int) Math.ceil((scope.getCentroid().getCoordinate().getX() + 180)/6);
        int srid;
        if (scope.getCentroid().getCoordinate().getY() < 0) {
            srid = Integer.valueOf("327" + centreZoneNumber);
        } else {
            srid = Integer.valueOf("326" + centreZoneNumber);
        }

        // Set the second argument to either scope or scope2 for testing purposes.
        bp.init(simulationDirectory, scope, nx, ny, srid);
        Assertions.assertTrue(bp.locindex > -1);
        bp.initGrid();
        bp.getStacksBuildings();
        int res = bp.createBPIPPRMInput();
        Assertions.assertEquals(res,0);
//        int res = bp.run();
//        Assertions.assertEquals(res,0);




//        try {
//            bp.init(simulationDirectory, scope, nx, ny, srid);
//            int res = bp.run();
//            Assertions.assertEquals(res,0);
//        } catch (Exception e) {
//            System.out.println(e);
//        }


    }








}
