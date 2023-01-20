/* This class queries the knowledge graph for buildings and plant items within a given scope. It uses this information
to run the Buildings Profile Input Program for PRIME (BPIPPRM). The BPIPPRM output file (buildings.dat) is
subsequently used to run AERMOD.
 */

package com.cmclinnovations.aermod;

import java.io.BufferedWriter;
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
import org.locationtech.jts.io.WKTReader;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;
import org.opengis.util.FactoryException;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.base.util.CRSTransformer;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class BuildingsPlantItems {

    private static final Logger LOGGER = LogManager.getLogger(BuildingsPlantItems.class);

    // Class variables accessed in several agent methods

    String[] locations = {"Jurong Island"};
    String[] StackQueryEndpoint = {"jibusinessunits"} ;
    String[] GeospatialQueryEndpoint = {"jriEPSG24500"} ;

    public static int locindex = -1;
    public static String StackQueryIRI;
    public static String GeospatialQueryIRI;

    /* Latitude and longitudes of locations where pollutant concentrations will be calculated are specified as strings
    where numbers are separated by commas */
    public static String Latitude;

    public static String Longitude;


    /* Receptor coordinates */
    public static List<List<Double>> ReceptorDatabaseCoordinates = new ArrayList<>() ;

    // Coordinate reference systems used by database (DatabaseCoordSys) and AERMOD(UTMCoordSys)
    public static String[] DatabaseCRS = {"EPSG:24500"};
    public static String DatabaseCoordSys, UTMCoordSys ;

    /* Each element of StackProperties contains the (x,y) coordinates of the center of the base polygon of the stack and the stack height.
    Each element of BuildingVertices contains the coordinates of the vertices of the base polygon.
     */
    public static List<String> StackProperties = new ArrayList<>()  ;
    public static List<Double> StackEmissions = new ArrayList<>()  ;
    public static List<Double> StackDiameter = new ArrayList<>()  ;

    /* Each element of BuildingProperties contains the (x,y) coordinates of the center of the base polygon of the building and the building height.
    Each element of BuildingVertices contains the coordinates of the vertices of the base polygon.
     */
    public static List<String> BuildingVertices = new ArrayList<>() ;
    public static List<String> BuildingProperties = new ArrayList<>() ;


    /* Variables for grid. x and y variables correspond to Easting and Northing respectively. Units
     *  of gridSpacing is meters */

    public static List<Integer> cellmap = new ArrayList<>() ;
    public static List<Integer> stackHead = new ArrayList<>() ;
    public static List<Integer>  stackList = new ArrayList<>() ;
    public static List<Integer> buildingHead = new ArrayList<>() ;
    public static List<Integer>  buildingList = new ArrayList<>() ;

    // Boolean arrays to check if stacks and buildings have been used previously
    public static List<Boolean> stackUsed = new ArrayList<>() ;
    public static List<Boolean> buildingUsed = new ArrayList<>() ;
    public static Integer numberGridsX, numberGridsY, numberTotalGrids ;

    /* Maximum distance between stack and receptor for which AERMOD computes pollutant concentrations
  in meters. */
    public static double cutoffRadius = 100.0;
    public static Double xlo, ylo, xhi, yhi ;
    public static Double gridSpacing = cutoffRadius;

    //    These values are taken from bboxfinder.com and are in EPSG:4326/WGS84 format.
    public static List<String> boundaryPolygons = new ArrayList<> (
            Arrays.asList("POLYGON ((1.216988 103.650684, 1.216988 103.743038, 1.293530 103.743038, 1.293530 103.650684, 1.216988 103.650684))")) ;


    // Variables used to run AERMOD and its preprocessors
    public static List<String> BPIPPRMBuildingInput = new ArrayList<>();
    public static List<String> BPIPPRMStackInput = new ArrayList<>() ;

    public Path simulationDirectory;
    public static Path bpipprmDirectory;

    public BuildingsPlantItems(Path simulationDirectory, Polygon scope, int nx, int ny, int srid) throws ParseException {
        this.simulationDirectory = simulationDirectory;
        bpipprmDirectory = simulationDirectory.resolve("bpipprm");
        bpipprmDirectory.toFile().mkdir();

        // Determine namespace to query based on input polygon
        List<Double> xDoubles = new ArrayList<>();
        List<Double> yDoubles = new ArrayList<>();

        String originalSRID = "EPSG:" + scope.getSRID();

        for (int i = 0; i < scope.getCoordinates().length; i++) {
            double[] xyTransformed = CRSTransformer.transform(originalSRID, "EPSG:4326", scope.getCoordinates()[i].x, scope.getCoordinates()[i].y);
            xDoubles.add(xyTransformed[0]);
            yDoubles.add(xyTransformed[1]);
        }

        double xMax = Collections.max(xDoubles);
        double xMin = Collections.min(xDoubles);
        double yMax = Collections.max(yDoubles);
        double yMin = Collections.min(yDoubles);

        GeometryFactory geometryFactory = new GeometryFactory(new PrecisionModel(),4326);
        Polygon boundary = null;

        for (int i = 0; i < boundaryPolygons.size(); i++){
            String wkt = boundaryPolygons.get(i);
            boundary = (Polygon) new WKTReader(geometryFactory).read(wkt);
            if (boundary.covers(scope)) {
                locindex = i;
                break;
            }
        }

        if (locindex == -1) {
            throw new RuntimeException("Input polygon not found in any namespace.");
        }

        StackQueryIRI = StackQueryEndpoint[locindex];
        GeospatialQueryIRI = GeospatialQueryEndpoint[locindex];
        DatabaseCoordSys = DatabaseCRS[locindex];
        UTMCoordSys = "EPSG:" + srid;

        double dx = (xMax - xMin)/nx;
        double dy = (yMax - yMin)/ny;

        for (int i = 0; i < nx; i++){
            for (int j = 0; j < ny; j++){
                double xCoord = xMin + (0.5 + i)*dx;
                double yCoord = yMin + (0.5 + j)*dy;
                double[] xyTransformed = CRSTransformer.transform("EPSG:4326", DatabaseCoordSys, xCoord, yCoord);
                ReceptorDatabaseCoordinates.add(Arrays.asList(xyTransformed[0],xyTransformed[1]));
            }
        }

        Envelope bounds = boundary.getEnvelopeInternal();
        xlo = bounds.getMinX();
        xhi = bounds.getMaxX();
        ylo = bounds.getMinY();
        yhi = bounds.getMaxY();

        xlo = gridSpacing*Math.floor(xlo/gridSpacing) ;
        xhi = gridSpacing*Math.ceil(xhi/gridSpacing) ;
        ylo = gridSpacing*Math.floor(ylo/gridSpacing) ;
        yhi = gridSpacing*Math.ceil(yhi/gridSpacing) ;
        float numberIntervalsX = (float) ((xhi - xlo)/gridSpacing);
        float numberIntervalsY = (float) ((yhi - ylo)/gridSpacing);
        numberGridsX = 1 + Math.round(numberIntervalsX);
        numberGridsY = 1 + Math.round(numberIntervalsY);
        numberTotalGrids = numberGridsX*numberGridsY ;


    }

    public static int run() {
        try {
            initGrid();
            getStacksBuildings();
            if (createBPIPPRMInput() == 0) runBPIPPRM();
            else {
                LOGGER.error("Failed to create BPIPPRM input file, terminating");
                return 1;
            }
        } catch (Exception e) {
            return 1;
        }
        return 0;
    }

    public static List<List<Double>> convertCoordinates
            (List<List<Double>> inputcoordinates, String inputCRS, String outputCRS) {

        List<List<Double>> outputcoordinates = new ArrayList<>();

        for (int i = 0; i < inputcoordinates.size(); i++) {
            double[] xyTransformed = CRSTransformer.transform(inputCRS, outputCRS, inputcoordinates.get(i).get(0), inputcoordinates.get(i).get(1));
            outputcoordinates.add(Arrays.asList(xyTransformed[0],xyTransformed[1]));
        }
        return outputcoordinates;
    }

    /* Initialize grid and query geospatial endpoint for corners of stacks and buildings.
Average the x and y coordinates for all corners corresponding to the minimum z value.
Use the average coordinates to assign stacks and buildings to grid cells and populate
the linked lists for each of these types of structures.
 */
    public static void initGrid () {

        for (int i = 0; i < numberGridsX; i++) {
            for (int j = 0; j < numberGridsY; j++) {
                int icell = i + j*numberGridsX ;
                cellmap.add(i + 1 + j*numberGridsX);
                cellmap.add(i - 1 + j*numberGridsX);
                cellmap.add(i + (j-1)*numberGridsX);
                cellmap.add(i + (j+1)*numberGridsX);
                cellmap.add(i - 1 + (j-1)*numberGridsX);
                cellmap.add(i - 1 + (j+1)*numberGridsX);
                cellmap.add(i + 1 + (j-1)*numberGridsX);
                cellmap.add(i + 1 + (j+1)*numberGridsX);

            }
        }

        for (int i = 0; i < numberTotalGrids; i++){
            stackHead.add(-1);
            buildingHead.add(-1);
        }

        JSONArray StackOCGMLIRI = StackQuery(StackQueryIRI) ;
        JSONArray BuildingOCGMLIRI = BuildingQuery(StackQueryIRI) ;

        for (int i = 0; i < StackOCGMLIRI.length(); i++){
            stackList.add(-1);
            stackUsed.add(false);
        }
        for (int i = 0; i < BuildingOCGMLIRI.length(); i++){
            buildingList.add(-1);
            buildingUsed.add(false);
        }
        for (int i = 0; i < StackOCGMLIRI.length(); i++) {
            Double emission = StackOCGMLIRI.getJSONObject(i).getDouble("emission");
            StackEmissions.add(emission);
            String IRI = StackOCGMLIRI.getJSONObject(i).getString("IRI");
            StringBuffer coordinateQuery = new StringBuffer("PREFIX ocgml: <http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#>\n");
            coordinateQuery.append("SELECT ?geometricIRI ?polygonData WHERE {\n");
            coordinateQuery.append("?geometricIRI ocgml:GeometryType ?polygonData.\n") ;
            coordinateQuery.append("?geometricIRI ocgml:cityObjectId <").append(IRI).append(">.}");
            JSONArray coordinateQueryResult = AccessAgentCaller.queryStore(GeospatialQueryIRI, coordinateQuery.toString());
            String StackX = "0";
            String StackY = "0";
            String StackZ = "0";

            int basePolygonIndex = -1;

            for (int ip = 0; ip < coordinateQueryResult.length(); ip++) {
                JSONObject coordiS = coordinateQueryResult.getJSONObject(ip);
                String coordiData = coordiS.getString("polygonData");
                List<String> z_values = new ArrayList<>();


                String[] coordinates = coordiData.split("#");
                double sum_x = 0; double sum_y = 0;
                double sum_z = 0; double min_z = 0;

                for(int j = 1; j <= coordinates.length; j++) {
                    if( j%3==0 ){
                        z_values.add(coordinates[j-1]);
                        sum_x = sum_x + Double.parseDouble(coordinates[j-3]);
                        sum_y = sum_y + Double.parseDouble(coordinates[j-2]);
                        sum_z = sum_z + Double.parseDouble(coordinates[j-1]);
                        min_z = Math.min(min_z,Double.parseDouble(coordinates[j-1]));
                    }
                }
                if (min_z == sum_z/(coordinates.length/3) && !z_values.isEmpty()) {
                    StackX = String.valueOf(sum_x/(coordinates.length/3));
                    StackY = String.valueOf(sum_y/(coordinates.length/3));
                    basePolygonIndex = ip;
                }
                if (!z_values.isEmpty() && Double.parseDouble(StackZ) < Double.parseDouble(Collections.max(z_values))) {
                    StackZ = Collections.max(z_values);
                }
            }
            StringBuffer averageCoordinate = new StringBuffer();

            averageCoordinate.append(StackX).append("#").append(StackY).append("#").append(StackZ);
            StackProperties.add(averageCoordinate.toString());

            // Calculate stack diameter from base polygon data
            JSONObject coordiS = coordinateQueryResult.getJSONObject(basePolygonIndex);
            String coordiData = coordiS.getString("polygonData");
            String[] coordinates = coordiData.split("#");
            Double StackDoubleX = Double.parseDouble(StackX);
            Double StackDoubleY = Double.parseDouble(StackY);
            Double radius = 0.0;
            for (int j = 0; j < coordinates.length;j+=3){
                Double dx = StackDoubleX - Double.parseDouble(coordinates[j]);
                Double dy = StackDoubleY - Double.parseDouble(coordinates[j+1]);
                Double dist = Math.sqrt(dx*dx + dy*dy);
                radius = radius +dist;
            }
            radius = radius/(coordinates.length/3);
            StackDiameter.add(2*radius);
            int ix = (int) (Math.floor((Double.parseDouble(StackX) - xlo)/gridSpacing));
            int iy = (int) (Math.floor((Double.parseDouble(StackY) - ylo)/gridSpacing));
            int icell = ix + iy*numberGridsX ;
            stackList.set(i,stackHead.get(icell));
            stackHead.set(icell,i);

        }

        for (int i = 0; i < BuildingOCGMLIRI.length(); i++) {

            String IRI = BuildingOCGMLIRI.getJSONObject(i).getString("IRI");
            StringBuffer coordinateQuery = new StringBuffer("PREFIX ocgml: <http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#>\n");
            coordinateQuery.append("SELECT ?polygondata WHERE {\n");
            coordinateQuery.append("?surfaceIRI ocgml:GeometryType ?polygondata.");
            coordinateQuery.append("?geometricIRI ocgml:lod2MultiSurfaceId ?surfaceIRI.");
            coordinateQuery.append("?geometricIRI ocgml:buildingId <").append(IRI).append(">.}");
            JSONArray coordinateQueryResult = AccessAgentCaller.queryStore(GeospatialQueryIRI, coordinateQuery.toString());
            String BuildingX = "0";
            String BuildingY = "0";
            String BuildingZ = "0";

            for (int ip = 0; ip < coordinateQueryResult.length(); ip++) {
                JSONObject coordiS = coordinateQueryResult.getJSONObject(ip);
                String coordiData = coordiS.getString("polygondata");
                List<String> z_values = new ArrayList<>();
                String[] coordinates = coordiData.split("#");
                double sum_x = 0; double sum_y = 0;
                double sum_z = 0; double min_z = 0;

                for (int j = 1; j <= coordinates.length; j++) {
                    if( j%3==0 ){
                        z_values.add(coordinates[j-1]);
                        sum_x = sum_x + Double.parseDouble(coordinates[j-3]);
                        sum_y = sum_y + Double.parseDouble(coordinates[j-2]);
                        sum_z = sum_z + Double.parseDouble(coordinates[j-1]);
                        min_z = Math.min(min_z,Double.parseDouble(coordinates[j-1]));
                    }
                }
                if (min_z == sum_z/(coordinates.length/3) && !z_values.isEmpty()) {
                    BuildingX = String.valueOf(sum_x/(coordinates.length/3));
                    BuildingY = String.valueOf(sum_y/(coordinates.length/3));
                    BuildingVertices.add(coordiData);
                }
                if (!z_values.isEmpty() && Double.parseDouble(BuildingZ) < Double.parseDouble(Collections.max(z_values))) {
                    BuildingZ = Collections.max(z_values);
                }
            }

            StringBuffer averageCoordinate = new StringBuffer();
            averageCoordinate.append(BuildingX).append("#").append(BuildingY).append("#").append(BuildingZ);
            BuildingProperties.add(averageCoordinate.toString());


            int ix = (int) (Math.floor((Double.parseDouble(BuildingX) - xlo)/gridSpacing));
            int iy = (int) (Math.floor((Double.parseDouble(BuildingY) - ylo)/gridSpacing));
            int icell = ix + iy*numberGridsX ;
            buildingList.set(i, buildingHead.get(icell));
            buildingHead.set(icell,i);

        }


    }


    /* Identify stacks close enough to each receptor to be able to influence local pollutant concentrations
    at that receptor. Identify buildings close enough to each stack to cause downwash effects. Write all the data to
    the BPIPPRMBuildingInput and BPIPPRMStackInput string buffers. The string buffers are then written to bpipprm.inp, which is the input
    for AERMOD's building pre-processsor BPIPPRM.

     Stack input format (one line per stack): Name, base elevation, height, coordinates.

     */
    public static void getStacksBuildings() throws FactoryException, TransformException {

        int numberStacks = 0;
        List<Integer> usedstacks = new ArrayList<>() ;
        /* Loop over receptors to identify stacks within cutoff distance */
        for (int i = 0; i < ReceptorDatabaseCoordinates.size(); i++) {
            double ReceptorX = ReceptorDatabaseCoordinates.get(i).get(0);
            double ReceptorY = ReceptorDatabaseCoordinates.get(i).get(1);
            int ix = (int) (Math.floor((ReceptorX - xlo)/gridSpacing));
            int iy = (int) (Math.floor((ReceptorY - ylo)/gridSpacing));
            int icell = ix + iy*numberGridsX ;
            int stackIndex = stackHead.get(icell);

            while (stackIndex > -1) {
                if (!stackUsed.get(stackIndex)) {
                    String StackAttribute = StackProperties.get(stackIndex);
                    String[] StackCoords = StackAttribute.split("#");
                    // Check if stack is close enough to receptor
                    Double StackX = Double.parseDouble(StackCoords[0]);
                    Double StackY = Double.parseDouble(StackCoords[1]);
                    Double StackHeight = Double.parseDouble(StackCoords[2]);
                    Double dist2 = (StackX - ReceptorX)*(StackX - ReceptorX) + (StackY - ReceptorY)*(StackY - ReceptorY) ;
                    if (dist2 < cutoffRadius*cutoffRadius) {
                        numberStacks++;
                        List<Double> inputcoords =
                                new ArrayList<>(Arrays.asList(StackX, StackY)) ;
                        List<List<Double>> inputcoordinates = new ArrayList<>(Arrays.asList(inputcoords)) ;
                        // convert from Database coordinates to UTM
                        List<List<Double>> outputCoordinates = convertCoordinates(inputcoordinates,DatabaseCoordSys,UTMCoordSys);

                        Double StackEastUTM = outputCoordinates.get(0).get(0);
                        Double StackNorthUTM = outputCoordinates.get(0).get(1);
                        String InputLine = "\'Stk" + String.valueOf(numberStacks) + "\'" + " " + "0.0 " +
                                StackHeight + " " + StackEastUTM + " " + StackNorthUTM + " \n" ;
                        BPIPPRMStackInput.add(InputLine);
                        stackUsed.set(stackIndex,true);
                        usedstacks.add(stackIndex);
                    }
                }

                stackIndex = stackList.get(stackIndex);
            }

            int jcell0 = 8*icell;

            for (int nabor = 0; nabor < 8; nabor++) {
                int jcell = cellmap.get(jcell0 + nabor);
                stackIndex = stackHead.get(jcell);
                while (stackIndex > -1) {
                    if (!stackUsed.get(stackIndex)) {
                        String StackAttribute = StackProperties.get(stackIndex);
                        String[] StackCoords = StackAttribute.split("#");
                        // Check if stack is close enough to receptor
                        Double StackX = Double.parseDouble(StackCoords[0]);
                        Double StackY = Double.parseDouble(StackCoords[1]);
                        Double StackHeight = Double.parseDouble(StackCoords[2]);
                        Double dist2 = (StackX - ReceptorX)*(StackX - ReceptorX) + (StackY - ReceptorY)*(StackY - ReceptorY) ;
                        if (dist2 < cutoffRadius*cutoffRadius) {
                            numberStacks++;
                            List<Double> inputcoords =
                                    new ArrayList<>(Arrays.asList(Double.parseDouble(StackCoords[0]), Double.parseDouble(StackCoords[1]))) ;
                            List<List<Double>> inputcoordinates = new ArrayList<>(Arrays.asList(inputcoords)) ;
                            // convert coordinates from Database coordinates to UTM
                            List<List<Double>> outputCoordinates = convertCoordinates(inputcoordinates,DatabaseCoordSys,UTMCoordSys);

                            Double StackEastUTM = outputCoordinates.get(0).get(0);
                            Double StackNorthUTM = outputCoordinates.get(0).get(1);
                            String InputLine = "\'Stk" + String.valueOf(numberStacks) + "\'" + " " + "0.0 " +
                                    StackHeight + " " + StackEastUTM + " " + StackNorthUTM + " \n" ;
                            BPIPPRMStackInput.add(InputLine);
                            stackUsed.set(stackIndex,true);
                            usedstacks.add(stackIndex);
                        }
                    }

                    stackIndex = stackList.get(stackIndex);
                }
            }

        }

        /* Loop over stacks to identify buildings close enough to cause downwash effects */
        int numberBuildings = 0;
        for (int i = 0; i < usedstacks.size(); i++) {
            int stackIndex = usedstacks.get(i) ;
            String StackAttribute = StackProperties.get(stackIndex);
            String[] StackCoords = StackAttribute.split("#");
            Double StackX = Double.parseDouble(StackCoords[0]);
            Double StackY = Double.parseDouble(StackCoords[1]);
            Double StackHeight = Double.parseDouble(StackCoords[2]);
            int ix = (int) (Math.floor((StackX - xlo)/gridSpacing));
            int iy = (int) (Math.floor((StackY - ylo)/gridSpacing));
            int icell = ix + iy*numberGridsX ;
            int buildingIndex = buildingHead.get(icell);

            while (buildingIndex > -1) {
                if (!buildingUsed.get(buildingIndex)) {
                    String BuildingAttribute = BuildingProperties.get(buildingIndex);

                    String[] BuildCoords = BuildingAttribute.split("#");
                    // Check if stack is close enough to receptor
                    Double BuildingX = Double.parseDouble(BuildCoords[0]);
                    Double BuildingY = Double.parseDouble(BuildCoords[1]);
                    Double BuildingHeight = Double.parseDouble(StackCoords[2]);
                    Double dist2 = (BuildingX - StackX)*(BuildingX - StackX) + (BuildingY - StackY)*(BuildingY - StackY) ;
                    Double gepHeight = 2.5*BuildingHeight;
                    Double criticalDistance = 5.0*BuildingHeight;
                    Double criticalDistanceSquared = criticalDistance*criticalDistance;


                    if (dist2 < criticalDistanceSquared && StackHeight < gepHeight) {
                        numberBuildings++;
                        String InputLine = "Build" + String.valueOf(numberBuildings) + " " + "1 " + "0.0" + " \n" ;
                        BPIPPRMBuildingInput.add(InputLine);
                        String BasePolygonVertices = BuildingVertices.get(buildingIndex);
                        String [] BaseVertices = BasePolygonVertices.split("#");
                        int numCorners = BaseVertices.length/3;
                        InputLine = numCorners + " " + BuildingHeight + " \n" ;
                        BPIPPRMBuildingInput.add(InputLine);

                        List<List<Double>> inputcoordinates = new ArrayList<> () ;

                        for (int j = 0; j < BaseVertices.length; j+=3 ){
                            List<Double> inputcoords = new ArrayList<>(Arrays.asList(Double.parseDouble(BaseVertices[j]), Double.parseDouble(BaseVertices[j+1]))) ;
                            inputcoordinates.add(inputcoords);
                        }


                        // convert coordinates from Database coordinates to UTM
                        List<List<Double>> outputCoordinates = convertCoordinates(inputcoordinates,DatabaseCoordSys,UTMCoordSys);
                        for (int j = 0; j < outputCoordinates.size(); j++ ){
                            Double VertexEastUTM = outputCoordinates.get(j).get(0);
                            Double VertexNorthUTM = outputCoordinates.get(j).get(1);
                            InputLine = VertexEastUTM + " " + VertexNorthUTM + " \n";
                            BPIPPRMBuildingInput.add(InputLine);
                        }

                        buildingUsed.add(true);

                    }
                }

                buildingIndex = buildingList.get(stackIndex);
            }

            int jcell0 = 8*icell;

            for (int nabor = 0; nabor < 8; nabor++) {
                int jcell = cellmap.get(jcell0 + nabor);
                buildingIndex = buildingHead.get(icell);
                while (buildingIndex > -1) {
                    if (!buildingUsed.get(buildingIndex)) {
                        String BuildingAttribute = BuildingProperties.get(buildingIndex);

                        String[] BuildCoords = BuildingAttribute.split("#");
                        // Check if stack is close enough to receptor
                        Double BuildingX = Double.parseDouble(BuildCoords[0]);
                        Double BuildingY = Double.parseDouble(BuildCoords[1]);
                        Double BuildingHeight = Double.parseDouble(StackCoords[2]);
                        Double dist2 = (BuildingX - StackX)*(BuildingX - StackX) + (BuildingY - StackY)*(BuildingY - StackY) ;
                        Double gepHeight = 2.5*BuildingHeight;
                        Double criticalDistance = 5.0*BuildingHeight;
                        Double criticalDistanceSquared = criticalDistance*criticalDistance;


                        if (dist2 < criticalDistanceSquared && StackHeight < gepHeight) {
                            numberBuildings++;
                            String InputLine = "Build" + String.valueOf(numberBuildings) + " " + "1 " + "0.0" ;
                            BPIPPRMBuildingInput.add(InputLine);
                            String BasePolygonVertices = BuildingVertices.get(buildingIndex);
                            String [] BaseVertices = BasePolygonVertices.split("#");
                            int numCorners = BaseVertices.length/3;
                            InputLine = numCorners + " " + BuildingHeight + " \n" ;
                            BPIPPRMBuildingInput.add(InputLine);

                            List<List<Double>> inputcoordinates = new ArrayList<> () ;

                            for (int j = 0; j < BaseVertices.length; j+=3 ){
                                ArrayList<Double> inputcoords = new ArrayList<>(Arrays.asList(Double.parseDouble(BaseVertices[j]), Double.parseDouble(BaseVertices[j+1]))) ;
                                inputcoordinates.add(inputcoords);
                            }


                            // convert coordinates from Database coordinates to UTM
                            List<List<Double>> outputCoordinates = convertCoordinates(inputcoordinates,DatabaseCoordSys,UTMCoordSys);
                            for (int j = 0; j < outputCoordinates.size(); j++ ){
                                Double VertexEastUTM = outputCoordinates.get(j).get(0);
                                Double VertexNorthUTM = outputCoordinates.get(j).get(1);
                                InputLine = VertexEastUTM + " " + VertexNorthUTM ;
                                BPIPPRMBuildingInput.add(InputLine);
                            }

                            buildingUsed.set(buildingIndex,true);

                        }
                    }

                    buildingIndex = buildingList.get(buildingIndex);
                }
            }
        }

        // Add the numbers of buildings and stacks as the last elements of the BPIPPRMStackInput and
        // BPIPPRMBuildingInput arrays.However, this information must be written to the BPIPPRM input file first.
        String StackLine = String.valueOf(numberStacks) + " \n" ;
        String BuildingsLine = String.valueOf(numberBuildings) + " \n" ;
        BPIPPRMStackInput.add(StackLine);
        BPIPPRMBuildingInput.add(BuildingsLine);


    }

    /* Write out data to BPIPPRM input file and run this program. */
    public static int createBPIPPRMInput() {

        List<String> frontmatter = new ArrayList<>();
        frontmatter.add("\'BPIPPRM test run\' \n");
        frontmatter.add("\'p\' \n");
        frontmatter.add("\' METERS    \'  1.0  \n");
        frontmatter.add("\'UTMY \'  0.0 \n");

        StringBuilder sb = new StringBuilder();

        sb.append(frontmatter);
        sb.append(System.lineSeparator());
        int numberBuildingLines = BPIPPRMBuildingInput.size() ;
        sb.append(BPIPPRMBuildingInput.get(numberBuildingLines - 1));
        sb.append(System.lineSeparator());
        for (int i = 0; i < numberBuildingLines-1; i++) {
            sb.append(BPIPPRMBuildingInput.get(i));
            sb.append(System.lineSeparator());
        }
        int numberStackLines = BPIPPRMStackInput.size() ;
        sb.append(BPIPPRMStackInput.get(numberStackLines - 1));
        for (int i = 0; i < numberStackLines-1; i++) {
            sb.append(BPIPPRMStackInput.get(i));
            sb.append(System.lineSeparator());
        }
        return writeToFile(bpipprmDirectory.resolve("bpipprm.inp"), sb.toString());

    }

    public static int writeToFile(Path path, String content) {
        try (BufferedWriter writer = Files.newBufferedWriter(path)) {
            LOGGER.info("Writing file: {}", path);
            writer.write(content);
            return 0;
        } catch (IOException e) {
            String errmsg = "Failed to write " + path.getFileName();
            LOGGER.error(errmsg);
            LOGGER.error(e.getMessage());
            return 1;
        }
    }

    public static int runBPIPPRM() {
        try {
            Process process = Runtime.getRuntime().exec(new String[]{"bpipprm.exe", "bpipprm.inp"}, null, bpipprmDirectory.toFile());
            if (process.waitFor() != 0) {
                return 1;
            }
        } catch (IOException e) {
            return 0;
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return 0;
        }
        return 0;
    }


    public static JSONArray StackQuery (String StackQueryIRI) {
        StringBuffer StackIRIQuery = new StringBuffer("PREFIX ns2: <https://www.theworldavatar.com/kg/ontobuiltenv/>\n");
        StackIRIQuery.append("PREFIX geo: <http://www.opengis.net/ont/geosparql#>\n");
        StackIRIQuery.append("PREFIX kb: <http://www.theworldavatar.com/kb/ontochemplant/>\n");
        StackIRIQuery.append("PREFIX ocp: <http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#>\n");
        StackIRIQuery.append("PREFIX om:  <http://www.ontology-of-units-of-measure.org/resource/om-2/>\n");
        StackIRIQuery.append("PREFIX rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n");
        StackIRIQuery.append("SELECT ?IRI ?emission WHERE {");
        StackIRIQuery.append("?chemical_plant rdf:type <http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#ChemicalPlant>.");
        StackIRIQuery.append("?chemical_plant geo:ehContains ?plant_item .");
        StackIRIQuery.append("?plant_item rdf:type <http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#PlantItem>.");
        StackIRIQuery.append("?plant_item ns2:hasOntoCityGMLRepresentation ?IRI .");
        StackIRIQuery.append("?plant_item ocp:hasIndividualCO2Emission ?CO2 .");
        StackIRIQuery.append("?CO2 om:hasNumericalValue ?emission .}");
        StackIRIQuery.append("LIMIT 100");
        JSONArray StackIRIQueryResult = AccessAgentCaller.queryStore(StackQueryIRI, StackIRIQuery.toString());
        return StackIRIQueryResult;
    }

    public static JSONArray BuildingQuery (String StackQueryIRI) {
        StringBuffer BuildingIRIQuery = new StringBuffer("PREFIX ns2: <https://www.theworldavatar.com/kg/ontobuiltenv/>\n");
        BuildingIRIQuery.append("PREFIX geo: <http://www.opengis.net/ont/geosparql#>\n");
        BuildingIRIQuery.append("PREFIX kb: <http://www.theworldavatar.com/kb/ontochemplant/>\n");
        BuildingIRIQuery.append("PREFIX ocp: <http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#>\n");
        BuildingIRIQuery.append("PREFIX om:  <http://www.ontology-of-units-of-measure.org/resource/om-2/>\n");
        BuildingIRIQuery.append("PREFIX rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n");
        BuildingIRIQuery.append("SELECT ?IRI WHERE {");
        BuildingIRIQuery.append("?chemical_plant rdf:type <http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#ChemicalPlant>.");
        BuildingIRIQuery.append("?chemical_plant geo:ehContains ?building .");
        BuildingIRIQuery.append("?building rdf:type <http://www.purl.org/oema/infrastructure/Building>.");
        BuildingIRIQuery.append("?building ns2:hasOntoCityGMLRepresentation ?IRI .}");
        BuildingIRIQuery.append("LIMIT 100");
        JSONArray BuildingIRIQueryResult = AccessAgentCaller.queryStore(StackQueryIRI, BuildingIRIQuery.toString());
        return BuildingIRIQueryResult;
    }



}
