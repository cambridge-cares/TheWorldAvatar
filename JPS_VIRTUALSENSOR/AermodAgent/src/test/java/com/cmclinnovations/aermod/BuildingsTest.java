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

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class BuildingsTest {

    String simulationDirectory = "C:\\Users\\KNAG01\\Dropbox (Cambridge CARES)\\IRP3 CAPRICORN shared folder\\KNAGARAJAN\\Projects\\Dispersion\\Data\\19\\";
    //    Two equivalent polygons which define a rectangular region within Jurong Island. The values in wkt are in EPSG:4326/WGS84 coordinates
    //    while those in wkt2 are in EPSG:3857 coordinates.
    // For EPSG:4326/Wgs84 format, longitude is specified before latitude.
    String wkt = "POLYGON ((103.672485 1.259121, 103.716345 1.259121, 103.716345 1.281174, 103.672485 1.281174, 103.672485 1.259121))" ;
    String wkt2 = "POLYGON ((11540768.2395 140175.9917, 11540768.2395 142631.5238, 11545650.7123 142631.5238, 11545650.7123 140175.9917, 11540768.2395 140175.9917))" ;

    String wkt3 = "POLYGON ((103.651 1.217, 103.742 1.217, 103.742 1.308, 103.651 1.308, 103.651 1.217))" ;
    GeometryFactory geometryFactory = new GeometryFactory(new PrecisionModel(),4326);
    Polygon scope = (Polygon) new WKTReader(geometryFactory).read(wkt3);
    int nx = 100;
    int ny = 100;

    Buildings bp = new Buildings() ;

    public BuildingsTest() throws ParseException {  }


    /* Test the initialization code within the constructor of the BuildingsPlantItems class. */
    @Test
    public void testInit() throws org.opengis.util.FactoryException, FactoryException, TransformException, ParseException {

        int centreZoneNumber = (int) Math.ceil((scope.getCentroid().getCoordinate().getX() + 180)/6);
        System.out.println(centreZoneNumber);
        int srid;
        if (scope.getCentroid().getCoordinate().getY() < 0) {
            srid = Integer.valueOf("327" + centreZoneNumber);
        } else {
            srid = Integer.valueOf("326" + centreZoneNumber);
        }

        bp.init(simulationDirectory, scope, nx, ny, srid);
        Assertions.assertTrue(bp.locindex > -1);
        bp.getStacksBuildings();
        int res = bp.createBPIPPRMInput();
        Assertions.assertEquals(res,0);
        int res2 = bp.createAERMODSourceInput();
        Assertions.assertEquals(res2,0);
//        int rds = bp.runBPIPPRM();
//        Assertions.assertEquals(rds,0);
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

    @Test
    public void testCoordinates() {

        double tol = 0.01;
        double error_x,error_y;

        int centreZoneNumber = (int) Math.ceil((scope.getCentroid().getCoordinate().getX() + 180)/6);
        System.out.println(centreZoneNumber);
        int srid;
        if (scope.getCentroid().getCoordinate().getY() < 0) {
            srid = Integer.valueOf("327" + centreZoneNumber);
        } else {
            srid = Integer.valueOf("326" + centreZoneNumber);
        }

        String UTMCoordSys = "EPSG:" + srid;

//Test conversion from EPSG:4326 to UTM coordinates, which is EPSG:32648 in Singapore's case
        List<List<Double>> inputcoordinates = new ArrayList<>();
        List<Double> inputcoords = new ArrayList<>(Arrays.asList(103.69,1.27));
        inputcoordinates.add(inputcoords);
        List<List<Double>> outputCoordinates = Buildings.convertCoordinates(inputcoordinates,"EPSG:4326",UTMCoordSys);

        for (List<Double> outcoord:outputCoordinates) {
            System.out.println(outcoord.get(0));
            System.out.println(outcoord.get(1));
        }

        error_x = outputCoordinates.get(0).get(0) - 354252.595175792;
        error_y = outputCoordinates.get(0).get(1) - 140410.3230744134;

        Assertions.assertTrue(error_x < tol);
        Assertions.assertTrue(error_y < tol);

        inputcoordinates.clear();
        inputcoords.clear();
        outputCoordinates.clear();
// Test conversion from EPSG:24500 to EPSG:4326
        inputcoords = new ArrayList<>(Arrays.asList(17612.50301,28401.53841));
        inputcoordinates.add(inputcoords);
        outputCoordinates = Buildings.convertCoordinates(inputcoordinates,"EPSG:24500","EPSG:4326");

        for (List<Double> outcoord:outputCoordinates) {
            System.out.println(outcoord.get(0));
            System.out.println(outcoord.get(1));
        }

        error_x = outputCoordinates.get(0).get(0) - 103.7399612;
        error_y = outputCoordinates.get(0).get(1) - 1.2731394;

        Assertions.assertTrue(error_x < tol);
        Assertions.assertTrue(error_y < tol);

        inputcoordinates.clear();
        inputcoords.clear();
        outputCoordinates.clear();

//        Test conversion from EPSG:24500 to UTM coordinates
        inputcoords = new ArrayList<>(Arrays.asList(17612.50301,28401.53841));
        inputcoordinates.add(inputcoords);
        outputCoordinates = Buildings.convertCoordinates(inputcoordinates,"EPSG:24500",UTMCoordSys);

        for (List<Double> outcoord:outputCoordinates) {
            System.out.println(outcoord.get(0));
            System.out.println(outcoord.get(1));
        }

        error_x = outputCoordinates.get(0).get(0) - 359812.2475178047;
        error_y = outputCoordinates.get(0).get(1) - 140754.64070724646;

        Assertions.assertTrue(error_x < tol);
        Assertions.assertTrue(error_y < tol);

        inputcoordinates.clear();
        inputcoords.clear();
        outputCoordinates.clear();



    }

    @Test
    public void testrun() throws IOException {
        int rds = Buildings.runBPIPPRM(simulationDirectory+"bpipprm\\");
        Assertions.assertEquals(rds,0);
    }



}
