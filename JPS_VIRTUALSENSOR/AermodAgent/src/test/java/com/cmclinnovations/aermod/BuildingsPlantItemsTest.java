/* This class queries the knowledge graph for buildings and plant items within a given scope. It uses this information
to run the Buildings Profile Input Program for PRIME (BPIPPRM). The BPIPPRM output file (buildings.dat) is
subsequently used to run AERMOD.
 */

package com.cmclinnovations.aermod;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import org.locationtech.jts.geom.*;
import org.locationtech.jts.io.ParseException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.base.util.CRSTransformer;
import org.locationtech.jts.io.WKTReader;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;


import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import com.cmclinnovations.aermod.BuildingsPlantItems;

public class BuildingsPlantItemsTest {

    Path simulationDirectory = Path.of("C:\\Users\\KNAG01\\Dropbox (Cambridge CARES)\\IRP3 CAPRICORN shared folder\\KNAGARAJAN\\Projects\\Dispersion\\Data\\17\\");
    String wkt = "POLYGON ((1.259121 103.672485, 1.259121 103.716345, 1.281174 103.716345, 1.281174 103.672485, 1.259121 103.672485))" ;
    int nx = 3;
    int ny = 3;

    BuildingsPlantItems bp ;


    /* Test the initialization code within the constructor of the BuildingsPlantItems class. */
    @Test
    public void testBuildingsPlantItems() {

        GeometryFactory geometryFactory = new GeometryFactory(new PrecisionModel(),4326);
        Polygon scope;
        try {
            scope = (Polygon) new WKTReader(geometryFactory).read(wkt);
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

        try {
            bp = new BuildingsPlantItems(simulationDirectory, scope, nx, ny, srid);
        } catch (Exception e) {
            System.out.println("Error in constructor of BuildingsPlantItems class");
        }
        Assertions.assertTrue(bp.locindex > 0);

    }
    @Test
    public void testRun() {
        int res = bp.run();
        Assertions.assertEquals(res,0);
    }





}
