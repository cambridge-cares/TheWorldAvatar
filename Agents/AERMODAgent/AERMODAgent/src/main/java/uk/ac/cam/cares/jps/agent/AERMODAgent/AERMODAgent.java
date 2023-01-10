package uk.ac.cam.cares.jps.agent.AERMODAgent;

import com.jayway.jsonpath.JsonPath;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import org.springframework.stereotype.Controller;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;
import java.sql.SQLException;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Example JPS Agent that receives a request with two numbers, adds them together, then returns the
 * result in a JSON format.
 *
 * @author Owen Parry {@literal <oparry@cmclinnovations.com>}
 */
@Controller
@WebServlet(urlPatterns = {AERMODAgent.URL_PATH})
public class AERMODAgent extends JPSAgent {



    public static final String URL_PATH = "/performdispersioncalculation";

    // Display messages
    private static final String BAD_INPUT = "Error in input parameters, please check the" +
            " input file";


//    HashMap<String, String[]> locations = new HashMap<String,String[]>();


    // Logger for reporting info/errors
    private static final Logger LOGGER = LogManager.getLogger(AERMODAgent.class);

    // Expected request keys
    private static final String FIRST_PARAM_KEY = "a";
    private static final String SECOND_PARAM_KEY = "b";
    private static final String RESULT_KEY = "c";

    // Responses
    private static final String BAD_REQUEST_MSG_KEY = "bad_request";
    private static final String REQUEST_RECEIVED_MSG = "Request received.";
    private static final String INVALID_REQUEST_MSG = "Invalid request.";


    // Class variables accessed in several agent methods

    String[] locations = {"Jurong Island"};
    String[] StackQueryEndpoint = {"http://theworldavatar.com/blazegraph/namespace/jibusinessunits/sparql/"} ;
    String[] GeospatialQueryEndpoint = {"http://www.theworldavatar.com:83/citieskg/namespace/jriEPSG24500/sparql/surfacegeometry"} ;

    private static int locindex = -1;
    public static String StackQueryIRI;
    public static String GeospatialQueryIRI;

    /* Latitude and longitudes of locations where pollutant concentrations will be calculated are specified as strings
    where numbers are separated by commas */
    public static String Latitude;

    public static String Longitude;



    /* Receptor coordinates */
    public static ArrayList<Double> ReceptorLat, ReceptorLong;
    public static ArrayList<Double> ReceptorEastEPSG24500, ReceptorNorthEPSG24500;

    public static ArrayList<String> StackProperties  ;

    /* BuildingProperties contains array lists of strings. Each string contains the coordinates of
    the vertices of one polygon. BuildingHeights contains the height of each building.
     */
    public static ArrayList<ArrayList<String>> BuildingVertices ;
    public static ArrayList<String> BuildingProperties ;


    /* Variables for grid. x and y variables correspond to Easting and Northing respectively. Units
    *  of gridSpacing is meters */

    public static ArrayList<Integer> cellmap, stackHead, stackList, buildingHead,buildingList ;

    // Boolean arrays to check if stacks and buildings have been used previously
    public static ArrayList<Boolean> stackUsed, buildingUsed ;
    public static Integer numberGridsX, numberGridsY, numberTotalGrids ;

    public static Double xlo, ylo, xhi, yhi ;
    public static Double gridSpacing = 100.0;

//    These values are taken from epsg.io.
    public static Double[] MinX = {-444270.49};
    public static Double[] MaxX = {249115.69};
    public static Double[] MinY = {12573.67};
    public static Double[] MaxY = {753645.03};

    public static StringBuffer BPIPPRMBuildingInput = new StringBuffer();
    public static StringBuffer BPIPPRMStackInput = new StringBuffer() ;

    /* Maximum distance between stack and receptor for which AERMOD computes pollutant concentrations
    in meters. */
    public static double cutoffRadius = 100.0;




    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {

        if (validateInput(requestParams)) {
            try{
                ArrayList<String> ReceptorLatString = new ArrayList<String>(Arrays.asList(Latitude.split("\\s*,\\s*"))) ;
                ArrayList<String> ReceptorLongString = new ArrayList<String>(Arrays.asList(Longitude.split("\\s*,\\s*")));
                ReceptorLat = new ArrayList<Double>(ReceptorLatString.stream().map(Double::parseDouble).collect(Collectors.toList()))  ;
                ReceptorLong = new ArrayList<Double> (ReceptorLongString.stream().map(Double::parseDouble).collect(Collectors.toList())) ;

                ArrayList<ArrayList<Double>> inputCoordinates = new ArrayList<ArrayList<Double>> (Arrays.asList(ReceptorLat, ReceptorLong));

                ArrayList<ArrayList<Double>> outputCoordinates = convertCoordinates(inputCoordinates);
                /*TODO: The arguments to the get method in the next two lines may need to be interchanged. */
                ReceptorEastEPSG24500 = outputCoordinates.get(0);
                ReceptorNorthEPSG24500 = outputCoordinates.get(1);

                initGrid();
                buildings();
                dispersionCalculation();






                return requestParams ;
            } catch (Exception e){
                throw new JPSRuntimeException(e);
            }
        }
        else {
            System.out.println("bad input.\n");
            throw new JPSRuntimeException(BAD_INPUT);
        }


    }


    /**
     * Checks the incoming JSON request for validity.
     * 
     * @param requestParams JSOn request parameters.
     * 
     * @return request validity.
     * 
     * @throws BadRequestException if request is malformed.
     */
    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {

        if (requestParams.isEmpty()) {
            throw new BadRequestException();
        }

        String LOCATION = JsonPath.read(requestParams.toString(), "$.job.location");
        for (int i = 0; i < locations.length; i++){
            if (locations[i] == LOCATION) {
                locindex = i;
                break;
            }
        }
        if(LOCATION == null || locindex== -1){
            throw new BadRequestException("Invalid location input.\n");
        }

        StackQueryIRI = StackQueryEndpoint[locindex];
        GeospatialQueryIRI = GeospatialQueryEndpoint[locindex];

        Latitude = JsonPath.read(requestParams.toString(), "$.job.latitude");
        if(Latitude == null || Latitude.trim().isEmpty()){
            throw new BadRequestException("Must specify at least one latitude for dispersion calculations.\n");
        }
        Longitude = JsonPath.read(requestParams.toString(), "$.job.longitude");
        if(Longitude == null || Longitude.trim().isEmpty()){
            throw new BadRequestException("Must specify at least one longitude for dispersion calculations.\n");
        }

        
        return true;
    }

    public static ArrayList<ArrayList<Double>> convertCoordinates (ArrayList<ArrayList<Double>> inputcoordinates) {
        return inputcoordinates;
    }

    /* Initialize grid and query geospatial endpoint for corners of stacks and buildings.
    Average the x and y coordinates for all corners corresponding to the minimum z value.
    Use the average coordinates to assign stacks and buildings to grid cells and populate
    the linked lists for each of these types of structures.
     */
    public static void initGrid () {

        xlo = gridSpacing*Math.floor(MinX[locindex]/gridSpacing) ;
        xhi = gridSpacing*Math.ceil(MaxX[locindex]/gridSpacing) ;
        ylo = gridSpacing*Math.floor(MinY[locindex]/gridSpacing) ;
        yhi = gridSpacing*Math.ceil(MaxY[locindex]/gridSpacing) ;
        float numberIntervalsX = (float) ((xhi - xlo)/gridSpacing);
        float numberIntervalsY = (float) ((yhi - ylo)/gridSpacing);
        numberGridsX = 1 + Math.round(numberIntervalsX);
        numberGridsY = 1 + Math.round(numberIntervalsY);
        numberTotalGrids = numberGridsX*numberGridsY ;
        cellmap.ensureCapacity(8*numberTotalGrids);
        for (int i = 0; i < numberGridsX; i++) {
            for (int j = 0; j < numberGridsY; j++) {
                int icell = i + j*numberGridsX ;
                int cellmapindex0 = 8*icell;
                cellmap.set(cellmapindex0,(i + 1 + j*numberGridsX));
                cellmap.set(cellmapindex0 + 1,(i - 1 + j*numberGridsX));
                cellmap.set(cellmapindex0 + 2,(i + (j-1)*numberGridsX));
                cellmap.set(cellmapindex0 + 3,(i + (j+1)*numberGridsX));
                cellmap.set(cellmapindex0 + 4,(i - 1 + (j-1)*numberGridsX));
                cellmap.set(cellmapindex0 + 5,(i - 1 + (j+1)*numberGridsX));
                cellmap.set(cellmapindex0 + 6,(i + 1 + (j-1)*numberGridsX));
                cellmap.set(cellmapindex0 + 7,(i + 1 + (j+1)*numberGridsX));
            }
        }

        stackHead.ensureCapacity(numberTotalGrids);
        buildingHead.ensureCapacity(numberTotalGrids);
        for (int i = 0; i < numberTotalGrids; i++){
            stackHead.set(i,-1);
            buildingHead.set(i,-1);
        }

        JSONArray StackOCGMLIRI = StackQuery(StackQueryIRI) ;
        JSONArray BuildingOCGMLIRI = BuildingQuery(StackQueryIRI) ;
        stackList.ensureCapacity(StackOCGMLIRI.length());
        buildingList.ensureCapacity(BuildingOCGMLIRI.length());
        stackUsed.ensureCapacity(StackOCGMLIRI.length());
        buildingUsed.ensureCapacity(BuildingOCGMLIRI.length());

        for (int i = 0; i < StackOCGMLIRI.length(); i++){
            stackList.set(i,-1);
            stackUsed.set(i,false);
        }
        for (int i = 0; i < BuildingOCGMLIRI.length(); i++){
            buildingList.set(i,-1);
            buildingUsed.set(i,false);
        }
        for (int i = 0; i < StackOCGMLIRI.length(); i++) {
            String IRI = StackOCGMLIRI.getJSONObject(i).getString("IRI");
            StringBuffer coordinateQuery = new StringBuffer("PREFIX ocgml: <http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#>\n");
            coordinateQuery.append("SELECT ?geometricIRI ?polygonData WHERE {\n");
            coordinateQuery.append("?geometricIRI ocgml:GeometryType ?polygonData.\n") ;
            coordinateQuery.append("?geometricIRI ocgml:cityObjectId <").append(IRI).append(">.}");
            JSONArray coordinateQueryResult = AccessAgentCaller.queryStore(GeospatialQueryIRI, coordinateQuery.toString());
            String StackX = "0";
            String StackY = "0";
            String StackZ = "0";

            for (int ip = 0; ip < coordinateQueryResult.length(); ip++) {
                JSONObject coordiS = coordinateQueryResult.getJSONObject(ip);
                String coordiData = coordiS.getString("polygonData");
                ArrayList<String> z_values = new ArrayList<>();


                String[] coordinates = coordiData.split("#");
                double sum_x = 0; double sum_y = 0;
                double sum_z = 0; double min_z = 0;

                for(Integer j = 1; j <= coordinates.length; j++) {
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
                }
                if (!z_values.isEmpty() && Double.parseDouble(StackZ) < Double.parseDouble(Collections.max(z_values))) {
                    StackZ = Collections.max(z_values);
                }
            }
            StringBuffer averageCoordinate = new StringBuffer();

            averageCoordinate.append(StackX).append("#").append(StackY).append("#").append(StackZ);
            StackProperties.add(averageCoordinate.toString());

            int ix = (int) (Math.floor((Double.parseDouble(StackX) - xlo)/gridSpacing));
            int iy = (int) (Math.floor((Double.parseDouble(StackY) - ylo)/gridSpacing));
            int icell = ix + iy*numberGridsX ;
            stackList.set(i, stackHead.get(icell));
            stackHead.set(icell,i);

        }

        for (int i = 0; i < BuildingOCGMLIRI.length(); i++) {

            String IRI = BuildingOCGMLIRI.getJSONObject(i).getString("IRI");
            StringBuffer coordinateQuery = new StringBuffer("PREFIX ocgml: <http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#>\n");
            coordinateQuery.append("SELECT ?polygonData WHERE {\n");
            coordinateQuery.append("?surfaceIRI ocgml:GeometryType ?polygondata.");
            coordinateQuery.append("?geometricIRI ocgml:lod2MultiSurfaceId ?surfaceIRI.");
            coordinateQuery.append("?geometricIRI ocgml:buildingId <").append(IRI).append(">.}");
            JSONArray coordinateQueryResult = AccessAgentCaller.queryStore(GeospatialQueryIRI, coordinateQuery.toString());
            String BuildingX = "0";
            String BuildingY = "0";
            String BuildingZ = "0";
            ArrayList<String> buildvert = new ArrayList<>();

            for (int ip = 0; ip < coordinateQueryResult.length(); ip++) {
                JSONObject coordiS = coordinateQueryResult.getJSONObject(ip);
                String coordiData = coordiS.getString("polygonData");
                ArrayList<String> z_values = new ArrayList<>();
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
                    buildvert.set(0,coordiData);
                }
                if (!z_values.isEmpty() && Double.parseDouble(BuildingZ) < Double.parseDouble(Collections.max(z_values))) {
                    BuildingZ = Collections.max(z_values);
                }
            }

            BuildingVertices.add(buildprop);
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
    public static void buildings() {

        int numberStacks = 0;
        ArrayList<Integer> usedstacks = new ArrayList<Integer>() ;
        /* Loop over receptors to identify stacks within cutoff distance */
        for (int i = 0; i < ReceptorEastEPSG24500.size(); i++) {
            double ReceptorX = ReceptorEastEPSG24500.get(i);
            double ReceptorY = ReceptorNorthEPSG24500.get(i);
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
                        ArrayList<Double> inputcoords =
                                new ArrayList<Double>(Arrays.asList(Double.parseDouble(StackCoords[0]), Double.parseDouble(StackCoords[1]))) ;
                        ArrayList<ArrayList<Double>> inputcoordinates = new ArrayList<ArrayList<Double>>(Arrays.asList(inputcoords)) ;
                        // convert coordinates from EPSG24500 to UTM
                        ArrayList<ArrayList<Double>> outputCoordinates = convertCoordinates(inputcoordinates);

                        Double StackEastUTM = outputCoordinates.get(0).get(0);
                        Double StackNorthUTM = outputCoordinates.get(0).get(1);
                        String InputLine = 'Stk' + String.valueOf(numberStacks) + " " + "0.0 " +
                                StackHeight + " " + StackEastUTM + " " + StackNorthUTM + " \n" ;
                        BPIPPRMStackInput.append(InputLine);
                        stackUsed.set(stackIndex,false);
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
                            ArrayList<Double> inputcoords =
                                    new ArrayList<Double>(Arrays.asList(Double.parseDouble(StackCoords[0]), Double.parseDouble(StackCoords[1]))) ;
                            ArrayList<ArrayList<Double>> inputcoordinates = new ArrayList<ArrayList<Double>>(Arrays.asList(inputcoords)) ;
                            // convert coordinates from EPSG24500 to UTM
                            ArrayList<ArrayList<Double>> outputCoordinates = convertCoordinates(inputcoordinates);

                            Double StackEastUTM = outputCoordinates.get(0).get(0);
                            Double StackNorthUTM = outputCoordinates.get(0).get(1);
                            String InputLine = 'Stk' + String.valueOf(numberStacks) + " " + "0.0 " +
                                    StackHeight + " " + StackEastUTM + " " + StackNorthUTM + " \n" ;
                            BPIPPRMStackInput.append(InputLine);
                            stackUsed.set(stackIndex,false);
                            usedstacks.add(stackIndex);
                        }
                    }

                    stackIndex = stackList.get(stackIndex);
                }
            }

        }

        /* Loop over stacks to identify buildings close enough to cause downwash effects */
        int numberBuildings = 0;
        ArrayList<Integer> usedbuildings = new ArrayList<Integer>() ;
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

                    String[] BuildCoords = StackAttribute.split("#");
                    // Check if stack is close enough to receptor
                    Double BuildingX = Double.parseDouble(StackCoords[0]);
                    Double BuildingY = Double.parseDouble(StackCoords[1]);
                    Double BuildingHeight = Double.parseDouble(StackCoords[2]);
                    Double dist2 = (BuildingX - StackX)*(BuildingX - StackX) + (BuildingY - StackY)*(BuildingY - StackY) ;
                    Double gepHeight = 2.5*BuildingHeight;
                    Double criticalDistance = 5.0*BuildingHeight;
                    Double criticalDistanceSquared = criticalDistance*criticalDistance;


                    if (dist2 < criticalDistanceSquared && StackHeight < gepHeight) {
                        numberBuildings++;
                        ArrayList<Double> inputcoords =
                                new ArrayList<Double>(Arrays.asList(Double.parseDouble(StackCoords[0]), Double.parseDouble(StackCoords[1]))) ;
                        ArrayList<ArrayList<Double>> inputcoordinates = new ArrayList<ArrayList<Double>>(Arrays.asList(inputcoords)) ;
                        // convert coordinates from EPSG24500 to UTM
                        ArrayList<ArrayList<Double>> outputCoordinates = convertCoordinates(inputcoordinates);

                        Double StackEastUTM = outputCoordinates.get(0).get(0);
                        Double StackNorthUTM = outputCoordinates.get(0).get(1);
                        String InputLine = 'Stk' + String.valueOf(numberStacks) + " " + "0.0 " +
                                StackHeight + " " + StackEastUTM + " " + StackNorthUTM + " \n" ;
                        BPIPPRMStackInput.append(InputLine);
                        stackUsed.set(stackIndex,false);
                        usedstacks.add(stackIndex);
                    }
                }

                stackIndex = stackList.get(stackIndex);
            }


 





        }






    }


    public static void dispersionCalculation() {



    }

    public static JSONArray StackQuery (String StackQueryIRI) {
        StringBuffer StackIRIQuery = new StringBuffer("PREFIX ns2: <https://www.theworldavatar.com/kg/ontobuiltenv/>\n");
        StackIRIQuery.append("PREFIX geo: <http://www.opengis.net/ont/geosparql#>\n");
        StackIRIQuery.append("PREFIX kb: <http://www.theworldavatar.com/kb/ontochemplant/>\n");
        StackIRIQuery.append("PREFIX ocp: <http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#>\n");
        StackIRIQuery.append("PREFIX om:  <http://www.ontology-of-units-of-measure.org/resource/om-2/>\n");
        StackIRIQuery.append("SELECT ?IRI WHERE {");
        StackIRIQuery.append("?chemical_plant rdf:type <http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#ChemicalPlant>.");
        StackIRIQuery.append("?chemical_plant geo:ehContains ?plant_item .");
        StackIRIQuery.append("?plant_item rdf:type <http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#PlantItem>.");
        StackIRIQuery.append("?plant_item ns2:hasOntoCityGMLRepresentation ?IRI .");
        JSONArray StackIRIQueryResult = AccessAgentCaller.queryStore(StackQueryIRI, StackIRIQuery.toString());
        return StackIRIQueryResult;
    }

    public static JSONArray BuildingQuery (String StackQueryIRI) {
        StringBuffer BuildingIRIQuery = new StringBuffer("PREFIX ns2: <https://www.theworldavatar.com/kg/ontobuiltenv/>\n");
        BuildingIRIQuery.append("PREFIX geo: <http://www.opengis.net/ont/geosparql#>\n");
        BuildingIRIQuery.append("PREFIX kb: <http://www.theworldavatar.com/kb/ontochemplant/>\n");
        BuildingIRIQuery.append("PREFIX ocp: <http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#>\n");
        BuildingIRIQuery.append("PREFIX om:  <http://www.ontology-of-units-of-measure.org/resource/om-2/>\n");
        BuildingIRIQuery.append("SELECT ?IRI WHERE {");
        BuildingIRIQuery.append("?chemical_plant rdf:type <http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#ChemicalPlant>.");
        BuildingIRIQuery.append("?chemical_plant geo:ehContains ?building .");
        BuildingIRIQuery.append("?building rdf:type <http://www.purl.org/oema/infrastructure/Building>.");
        BuildingIRIQuery.append("?building ns2:hasOntoCityGMLRepresentation ?IRI .");
        JSONArray BuildingIRIQueryResult = AccessAgentCaller.queryStore( StackQueryIRI, BuildingIRIQuery.toString());
        return BuildingIRIQueryResult;
    }




}
// End of class.
