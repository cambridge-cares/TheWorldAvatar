package com.cmclinnovations.aermod;

import geotrellis.proj4.CRS;
import geotrellis.proj4.Transform;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import org.locationtech.jts.geom.Envelope;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.Polygon;
import org.locationtech.jts.geom.PrecisionModel;
import org.locationtech.jts.io.ParseException;
import org.locationtech.jts.io.WKTReader;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;
import scala.Tuple2;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import static java.lang.Math.max;
import static java.lang.Math.min;


public class Buildings {

    private static final Logger LOGGER = LogManager.getLogger(BuildingsPlantItems.class);

    // Class variables accessed in several agent methods

    public static String[] locations = {"Jurong Island"};
    public static String[] StackQueryEndpoint = {"jibusinessunits"} ;
    public static String[] GeospatialQueryEndpoint = {"jriEPSG24500"} ;

    public static int locindex = -1;
    public static String StackQueryIRI;
    public static String GeospatialQueryIRI;


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




    //    These values are taken from bboxfinder.com and are in EPSG:4326/WGS84 format.
    public static List<String> boundaryPolygons = new ArrayList<> (
            Arrays.asList("POLYGON ((1.216988 103.650684, 1.216988 103.743038, 1.308804 103.743038, 1.308804 103.650684, 1.216988 103.650684))")) ;


    // Variables used to run AERMOD and its preprocessors
    public static List<String> BPIPPRMBuildingInput = new ArrayList<>();
    public static List<String> BPIPPRMStackInput = new ArrayList<>() ;

    public static String simulationDirectory;
    public static String bpipprmDirectory;

    public void init(String simulationDirectory, Polygon scope, int nx, int ny, int srid) throws ParseException, FactoryException, TransformException, org.opengis.util.FactoryException {


        this.simulationDirectory = simulationDirectory;
        bpipprmDirectory = simulationDirectory + "bpipprm\\";


        // Determine namespace to query based on input polygon

        // The boundary.covers(scope) test works correctly only if boundary and scope have the same srid. Hence, srid of scope must be 4326.//


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

        if (scope.getSRID() != 4326) {
            throw new RuntimeException("Input scope does not have 4326 as its srid.");
        }

        StackQueryIRI = StackQueryEndpoint[locindex];
        GeospatialQueryIRI = GeospatialQueryEndpoint[locindex];
        DatabaseCoordSys = DatabaseCRS[locindex];
        UTMCoordSys = "EPSG:" + srid;

//        From this point, all coordinates are in the database coordinate system.

//        Assign receptor coordinates.


        String originalSRID = "EPSG:" + scope.getSRID();
        List<List<Double>> inputcoordinates = new ArrayList<>() ;


        for (int i = 0; i < scope.getCoordinates().length; i++) {
            List<Double> coord = Arrays.asList(scope.getCoordinates()[i].x, scope.getCoordinates()[i].y);
            inputcoordinates.add(coord);
        }

        List<List<Double>> outputcoordinates = convertCoordinates(inputcoordinates,originalSRID,DatabaseCoordSys);

        double xMax = Collections.max(outputcoordinates.stream().map(x -> x.get(0)).collect(Collectors.toList()));
        double xMin = Collections.min(outputcoordinates.stream().map(x -> x.get(0)).collect(Collectors.toList()));
        double yMax = Collections.max(outputcoordinates.stream().map(x -> x.get(1)).collect(Collectors.toList()));
        double yMin = Collections.min(outputcoordinates.stream().map(x -> x.get(1)).collect(Collectors.toList()));

        double dx = (xMax - xMin)/nx;
        double dy = (yMax - yMin)/ny;

        for (int i = 0; i < nx; i++){
            for (int j = 0; j < ny; j++){
                double xCoord = xMin + (0.5 + i)*dx;
                double yCoord = yMin + (0.5 + j)*dy;
                ReceptorDatabaseCoordinates.add(Arrays.asList(xCoord,yCoord));
            }
        }


    }

    public static int run() {
        try {
            getStacksBuildings();
            if (createBPIPPRMInput() == 0) runBPIPPRM(bpipprmDirectory);
            else {
                LOGGER.error("Failed to create BPIPPRM input file, terminating");
                return 1;
            }
        } catch (Exception e) {
            return 1;
        }
        return 0;
    }

    //    The String inputs must be of a format similar to "EPSG:4326".
    public static List<List<Double>> convertCoordinates
    (List<List<Double>> inputcoordinates, String inputCRS, String outputCRS) {

        List<List<Double>> outputcoordinates = new ArrayList<>();


        String inputSys = inputCRS.split(":")[1];
        int inputCode = Integer.valueOf(inputSys);
        String outputSys = outputCRS.split(":")[1];
        int outputCode = Integer.valueOf(outputSys);

        CRS sourceCRS = CRS.fromEpsgCode(inputCode);
        CRS targetCRS = CRS.fromEpsgCode(outputCode);

        var convert = Transform.apply(sourceCRS,targetCRS);


        for (int i = 0; i < inputcoordinates.size(); i++) {
            double xi = inputcoordinates.get(i).get(0);
            double yi = inputcoordinates.get(i).get(1);
            Tuple2<Object, Object> res;
            if (inputCode == 4326) res = convert.apply(yi, xi);
            else res = convert.apply(xi,yi);
            double xt = (double) res._1();
            double yt = (double) res._2();
            outputcoordinates.add(Arrays.asList(xt,yt));
        }

        return outputcoordinates;
    }

    /* Query stacks and buildings
 */
    public static void getStacksBuildings () {


        JSONArray StackOCGMLIRI = StackQuery(StackQueryIRI) ;
        JSONArray BuildingOCGMLIRI = BuildingQuery(StackQueryIRI) ;

        int numberStacks = StackOCGMLIRI.length();
        int numberBuildings = BuildingOCGMLIRI.length();


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
                        min_z = min(min_z,Double.parseDouble(coordinates[j-1]));
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

            List<Double> inputcoords =
                    new ArrayList<>(Arrays.asList(Double.parseDouble(StackX), Double.parseDouble(StackY))) ;
            List<List<Double>> inputcoordinates = new ArrayList<>(Arrays.asList(inputcoords)) ;
            // convert coordinates from Database coordinates to UTM
            List<List<Double>> outputCoordinates = convertCoordinates(inputcoordinates,DatabaseCoordSys,UTMCoordSys);

            Double StackEastUTM = outputCoordinates.get(0).get(0);
            Double StackNorthUTM = outputCoordinates.get(0).get(1);
            String InputLine = "\'Stk" + i + "\'" + " " + "0.0 " +
                    StackZ + " " + StackEastUTM + " " + StackNorthUTM + " \n" ;
            BPIPPRMStackInput.add(InputLine);

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
                        min_z = min(min_z,Double.parseDouble(coordinates[j-1]));
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

            String InputLine = "\'Build" + i + "\' " + "1 " + "0.0" + " \n" ;
            BPIPPRMBuildingInput.add(InputLine);
            String BasePolygonVertices = BuildingVertices.get(i);
            String [] BaseVertices = BasePolygonVertices.split("#");
            int numCorners = BaseVertices.length/3;
            InputLine = numCorners + " " + BuildingZ + " \n" ;
            BPIPPRMBuildingInput.add(InputLine);

            List<List<Double>> inputcoordinates = new ArrayList<> () ;

            for (int j = 0; j < BaseVertices.length; j+=3 ){
                ArrayList<Double> inputcoords = new ArrayList<>(Arrays.asList(Double.parseDouble(BaseVertices[j]), Double.parseDouble(BaseVertices[j+1]))) ;
                inputcoordinates.add(inputcoords);
            }

            // convert coordinates from Database coordinates to UTM
            List<List<Double>> outputCoordinates = convertCoordinates(inputcoordinates,DatabaseCoordSys,UTMCoordSys);
            for (int j = 0; j < outputCoordinates.size(); j++ ) {
                Double VertexEastUTM = outputCoordinates.get(j).get(0);
                Double VertexNorthUTM = outputCoordinates.get(j).get(1);
                InputLine = VertexEastUTM + " " + VertexNorthUTM + " \n" ;
                BPIPPRMBuildingInput.add(InputLine);
            }
        }

        // Add the numbers of buildings and stacks as the last elements of the BPIPPRMStackInput and
        // BPIPPRMBuildingInput arrays.However, this information must be written to the BPIPPRM input file first.
        String StackLine = numberStacks + " \n" ;
        String BuildingsLine = numberBuildings + " \n" ;
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

        for (String st:frontmatter) {
            sb.append(st);
        }

        int numberBuildingLines = BPIPPRMBuildingInput.size() ;
        sb.append(BPIPPRMBuildingInput.get(numberBuildingLines - 1));
        for (int i = 0; i < numberBuildingLines-1; i++) {
            sb.append(BPIPPRMBuildingInput.get(i));
        }
        int numberStackLines = BPIPPRMStackInput.size() ;
        sb.append(BPIPPRMStackInput.get(numberStackLines - 1));
        for (int i = 0; i < numberStackLines-1; i++) {
            sb.append(BPIPPRMStackInput.get(i));
        }
        return writeToFile(bpipprmDirectory + "bpipprm.inp", sb.toString());

    }

    public static int writeToFile(String filename, String content) {
        try (FileWriter writer = new FileWriter(filename)) {
            LOGGER.info("Writing file: {}",filename);
            writer.write(content);
            return 0;
        } catch (IOException e) {
            String errmsg = "Failed to write " + filename;
            LOGGER.error(errmsg);
            LOGGER.error(e.getMessage());
            return 1;
        }
    }

    public static int runBPIPPRM(String runDirectory) {
        System.out.println(runDirectory);
        String execFile = runDirectory + "bpipprm.exe" ;
        String inputFile = runDirectory + "bpipprm3.inp" ;
        String outputFile1 = runDirectory + "buildings.dat";
        String outputFile2 = runDirectory + "buildings_summary.dat";
        try {
            Process p = new ProcessBuilder(execFile,inputFile,outputFile1,outputFile2).start();
            p.waitFor();
        } catch (IOException e) {
            return 1;
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
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
//        StackIRIQuery.append("LIMIT 100");
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
//        BuildingIRIQuery.append("LIMIT 100");
        JSONArray BuildingIRIQueryResult = AccessAgentCaller.queryStore(StackQueryIRI, BuildingIRIQuery.toString());
        return BuildingIRIQueryResult;
    }







}
