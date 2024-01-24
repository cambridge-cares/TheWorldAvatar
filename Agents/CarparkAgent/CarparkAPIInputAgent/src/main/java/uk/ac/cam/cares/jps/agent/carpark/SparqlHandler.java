package uk.ac.cam.cares.jps.agent.carpark;

import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.agent.carpark.file.ConfigReader;
import uk.ac.cam.cares.jps.base.util.JSONKeyToIRIMapper;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.*;

import me.xdrop.fuzzywuzzy.FuzzySearch;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import org.eclipse.rdf4j.sparqlbuilder.core.query.InsertDataQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;

public class SparqlHandler {
    private static final Logger LOGGER = LogManager.getLogger(SparqlHandler.class);
    private String queryEndpoint;
    private String updateEndpoint;
    private String sparqlUsername;
    private String sparqlPassword;
    RemoteStoreClient kbClient;

    /**
     * Namespaces for ontologies
     */
    private static final String OntoCarpark_NS = "https://www.theworldavatar.com/kg/ontocarpark/";
    private static final String RDFS_NS = "http://www.w3.org/2000/01/rdf-schema#";
    private static final String BOT_NS = "https://w3id.org/bot#";
    private static final String OntoBuiltEnv_NS = "https://www.theworldavatar.com/kg/ontobuiltenv/";
    private static final String ICONTACT_NS = "http://ontology.eil.utoronto.ca/icontact.owl#";
    private static final String GEO_NS = "http://www.w3.org/2003/01/geo/wgs84_pos#";

    /**
     * Prefixes
     */
    private static final Prefix PREFIX_ONTOCARPARK = SparqlBuilder.prefix("ontocarpark", iri(OntoCarpark_NS));
    private static final String generatedIRIPrefix = TimeSeriesSparql.TIMESERIES_NAMESPACE + "Carpark";
    private static final Prefix PREFIX_RDFS = SparqlBuilder.prefix("rdfs", iri(RDFS_NS));
    private static final Prefix PREFIX_BOT = SparqlBuilder.prefix("bot", iri(BOT_NS));
    private static final Prefix PREFIX_ONTOBUILTENV = SparqlBuilder.prefix("ontobuiltenv", iri(OntoBuiltEnv_NS));
    private static final Prefix PREFIX_ICONTACT = SparqlBuilder.prefix("icontact", iri(ICONTACT_NS));
    private static final Prefix PREFIX_GEO = SparqlBuilder.prefix("geo", iri(GEO_NS));

    /**
     * Relationships
     */
    private static final Iri hasAgency = PREFIX_ONTOCARPARK.iri("hasAgency");
    private static final Iri hasID = PREFIX_ONTOCARPARK.iri("hasID");
    private static final Iri hasLots = PREFIX_ONTOCARPARK.iri("hasLots");
    private static final Iri hasLotType = PREFIX_ONTOCARPARK.iri("hasLotType");
    private static final Iri hasWeekdayRates = PREFIX_ONTOCARPARK.iri("hasWeekdayRates");
    private static final Iri hasSaturdayRates = PREFIX_ONTOCARPARK.iri("hasSaturdayRates");
    private static final Iri hasSundayAndPHRates = PREFIX_ONTOCARPARK.iri("hasSundayAndPHRates");
    private static final Iri label = PREFIX_RDFS.iri("label");
    private static final Iri hasPropertyUsage = PREFIX_ONTOBUILTENV.iri("hasPropertyUsage");
    private static final Iri hasAddress = PREFIX_ONTOBUILTENV.iri("hasAddress");
    private static final Iri latitude = PREFIX_GEO.iri("lat");
    private static final Iri longitude = PREFIX_GEO.iri("long");
    private static final Iri hasUnitName = PREFIX_ONTOBUILTENV.iri("hasUnitName");
    private static final Iri hasStreetNumber = PREFIX_ICONTACT.iri("hasStreetNumber");
    private static final Iri hasStreet = PREFIX_ICONTACT.iri("hasStreet");
    private static final Iri hasBuilding = PREFIX_ICONTACT.iri("hasBuilding");

    /**
     * Classes
     */
    private static final Iri AvailableLots = PREFIX_ONTOCARPARK.iri("AvailableLots");
    private static final Iri Carpark = PREFIX_ONTOCARPARK.iri("Carpark");
    private static final Iri Cars = PREFIX_ONTOCARPARK.iri("Cars");
    private static final Iri Motorcycles = PREFIX_ONTOCARPARK.iri("Motorcycles");
    private static final Iri HeavyVehicles = PREFIX_ONTOCARPARK.iri("HeavyVehicles");
    private static final Iri Building = PREFIX_BOT.iri("Building");
    private static final Iri Address = PREFIX_ICONTACT.iri("Address");

    public JSONObject readings;
    public JSONObject priceReadings;

    private List<JSONKeyToIRIMapper> mappings;

    public SparqlHandler(String agentProp, RemoteStoreClient kbClient) throws IOException {
        this.kbClient = kbClient;
        // Retrieve agent properties
        loadproperties(agentProp);
    }

    public void loadproperties(String propfile) throws IOException {
        try (InputStream input = new FileInputStream(propfile)) {
            Properties prop = new Properties();
            prop.load(input);
            String mappingfolder;
            try {
                mappingfolder = System.getenv(prop.getProperty("Carpark.mappingfolder"));
            } catch (NullPointerException e) {
                LOGGER.fatal("The key Carpark.mappingfolder cannot be found");
                throw new IOException("The key Carpark.mappingfolder cannot be found");
            }

            if (mappingfolder == null) {
                LOGGER.fatal("The properties file does not contain the key Carpark.mappingfolder with a path to the folder containing the required JSON key to IRI Mappings");

                throw new InvalidPropertiesFormatException("The properties file does not contain the key Carpark.mappingfolder with a path to the folder containing the required JSON key to IRI Mappings");
            }
            mappings = new ArrayList<>();
            File folder = new File(mappingfolder);
            File[] mappingFiles = folder.listFiles();

            if (mappingFiles.length == 0) {
                LOGGER.fatal("No files in folder");
                throw new IOException("No files in folder");
            } else {
                for (File mappingFile : mappingFiles) {
                    JSONKeyToIRIMapper mapper = new JSONKeyToIRIMapper(generatedIRIPrefix, mappingFile.getAbsolutePath());
                    mappings.add(mapper);
                    mapper.saveToFile(mappingFile.getAbsolutePath());
                }
            }
        }
    }

    public String capitaliseFirstLetterOfEachWord(String word) {
        word = word.toLowerCase();
        String[] listOfLabel = word.split(" ");
        String output = "";
        for (int j = 0; j < listOfLabel.length; j++) {
            if (listOfLabel[j].length() < 1) {
                output = output + listOfLabel[j];
            } else {
                output = output + listOfLabel[j].substring(0, 1).toUpperCase() + listOfLabel[j].substring(1) + " ";
            }
        }
        return output.strip();
    }


    public void instantiateIfNotInstantiated(JSONObject carparkReadings, JSONObject prices) {
        readings = carparkReadings;
        priceReadings = prices;
        List<String> iris;
        JSONArray carparkRates = priceReadings.getJSONObject("result").getJSONArray("records");
        for (JSONKeyToIRIMapper mapping : mappings) {
            iris = mapping.getAllIRIs();

            for (String iri : iris) {
                if (!iri.contains("Carpark_time_")) {
                    String r = null;
                    Variable c = SparqlBuilder.var("c");
                    SelectQuery q = Queries.SELECT();
                    //check whether each data IRI has a rdf:type
                    TriplePattern qP = iri(iri).isA(c);
                    q.prefix(PREFIX_ONTOCARPARK).select(c).where(qP);
                    kbClient.setQuery(q.getQueryString());
                    try {
                        JSONArray qR = kbClient.executeQuery();

                        if (qR.isEmpty()) {
                            TriplePattern P1 = iri(iri).isA(AvailableLots);
                            InsertDataQuery insert = Queries.INSERT_DATA(P1);
                            insert.prefix(PREFIX_ONTOCARPARK);
                            kbClient.executeUpdate(insert.getQueryString());
                        } else {
                            continue;
                        }
                    } catch (Exception e) {
                        LOGGER.fatal("Could not check for an rdf:type for the Data IRI");
                        throw new JPSRuntimeException("Could not check for an rdf:type for the Data IRI");
                    }

                    StringTokenizer st = new StringTokenizer(iri, "_");
                    for (int i = 0; i < 2; i++)
                        st.nextToken();
                    String CarparkID, lotType;

                    CarparkID = st.nextToken();
                    lotType = st.nextToken();

                    final String lotTypeIri = OntoCarpark_NS + "Carpark_LotType_" + UUID.randomUUID();

                    //instantiate lotType as a rdf:type ontocarpark:Cars
                    if (lotType.equalsIgnoreCase("C")) {
                        TriplePattern updatePattern = iri(lotTypeIri).isA(Cars);
                        InsertDataQuery insert = Queries.INSERT_DATA(updatePattern);
                        insert.prefix(PREFIX_ONTOCARPARK);
                        kbClient.executeUpdate(insert.getQueryString());
                        //instantiate lotType as a rdf:type ontocarpark:HeavyVehicles
                    } else if (lotType.equalsIgnoreCase("H")) {
                        TriplePattern updatePattern = iri(lotTypeIri).isA(HeavyVehicles);
                        InsertDataQuery insert = Queries.INSERT_DATA(updatePattern);
                        insert.prefix(PREFIX_ONTOCARPARK);
                        kbClient.executeUpdate(insert.getQueryString());
                        //instantiate lotType as a rdf:type ontocarpark:Motorcycles
                    } else {
                        TriplePattern updatePattern = iri(lotTypeIri).isA(Motorcycles);
                        InsertDataQuery insert = Queries.INSERT_DATA(updatePattern);
                        insert.prefix(PREFIX_ONTOCARPARK);
                        kbClient.executeUpdate(insert.getQueryString());
                    }

                    String carparkIRI = null;
                    String buildingIRI = null;

                    //check whether a carpark instance has already been created for a particular carpark ID
                    Variable carpark = SparqlBuilder.var("carpark");
                    SelectQuery query = Queries.SELECT();
                    TriplePattern queryPattern = carpark.has(hasID, CarparkID);
                    query.prefix(PREFIX_ONTOCARPARK).select(carpark).where(queryPattern);

                    kbClient.setQuery(query.getQueryString());

                    try {
                        JSONArray queryResult = kbClient.executeQuery();
                        TriplePattern patternLotType;

                        //reuse the carpark instance created previously and link it to the different lotTypes IRIs
                        if (!queryResult.isEmpty()) {
                            carparkIRI = kbClient.executeQuery().getJSONObject(0).getString("carpark");
                            patternLotType = iri(carparkIRI).has(hasLotType, iri(lotTypeIri));
                        } else {
                            //if queryresult is empty, create a carpark instance
                            carparkIRI = OntoCarpark_NS + "Carpark_" + UUID.randomUUID();
                            patternLotType = iri(carparkIRI).has(hasLotType, iri(lotTypeIri));
                        }
                        InsertDataQuery insert = Queries.INSERT_DATA(patternLotType);
                        insert.prefix(PREFIX_ONTOCARPARK);
                        kbClient.executeUpdate(insert.getQueryString());

                        //instantiate rdf:type ontocarpark:Carpark for the carpark instances
                        TriplePattern pattern = iri(carparkIRI).isA(Carpark);
                        insert = new InsertDataQuery();
                        insert = Queries.INSERT_DATA(pattern);
                        insert.prefix(PREFIX_ONTOCARPARK);
                        kbClient.executeUpdate(insert.getQueryString());

                        /**
                         * Removed temporarily for testing
                         * //Check whether a building concept has been linked to the carpark instance via ontoBuiltEnv:hasPropertyUsage
                         Variable building = SparqlBuilder.var("building");
                         query = Queries.SELECT();
                         TriplePattern buildingPattern = building.has(hasPropertyUsage, iri(carparkIRI));
                         query.prefix(PREFIX_ONTOBUILTENV).select(building).where(buildingPattern);
                         kbClient.setQuery(query.getQueryString());
                         queryResult = kbClient.executeQuery();
                         if(!queryResult.isEmpty()) {
                         buildingIRI = queryResult.getJSONObject(0).getString("building");
                         }
                         else {
                         //if queryresult is empty
                         //create buildingIRI with rdf:type and link to carpark instance via hasPropertyUsage
                         buildingIRI = OntoCarpark_NS + "Building_" + UUID.randomUUID();
                         buildingPattern = iri(buildingIRI).isA(Building).andHas(hasPropertyUsage, iri(carparkIRI));
                         insert = new InsertDataQuery();
                         insert = Queries.INSERT_DATA(buildingPattern);
                         insert.prefix(PREFIX_BOT, PREFIX_ONTOBUILTENV);
                         kbClient.executeUpdate(insert.getQueryString());
                         }
                         */

                        //TriplePattern for CarparkID
                        pattern = iri(carparkIRI).has(hasID, CarparkID);
                        insert = new InsertDataQuery();
                        insert = Queries.INSERT_DATA(pattern);
                        insert.prefix(PREFIX_ONTOCARPARK);
                        kbClient.executeUpdate(insert.getQueryString());

                        //TriplePattern to link LotType IRI to data Iri
                        pattern = iri(lotTypeIri).has(hasLots, iri(iri));
                        insert = new InsertDataQuery();
                        insert = Queries.INSERT_DATA(pattern);
                        insert.prefix(PREFIX_ONTOCARPARK);
                        kbClient.executeUpdate(insert.getQueryString());
                    } catch (Exception e) {
                        LOGGER.fatal("Unable to execute query: {} {}", query.getQueryString(), e);
                        throw new JPSRuntimeException("Unable to execute query: " + query.getQueryString(), e);
                    }

                    Variable devLabel = SparqlBuilder.var("label");
                    query = Queries.SELECT();
                    queryPattern = iri(carparkIRI).has(label, devLabel);
                    query.prefix(PREFIX_RDFS).select(devLabel).where(queryPattern);
                    kbClient.setQuery(query.getQueryString());

                    JSONArray queryResult = kbClient.executeQuery();
                    if (queryResult.isEmpty()) {
                        String loc = "";
                        String agency = "";
                        String Devlabel = "";

                        try {
                            JSONArray jsArr;
                            jsArr = readings.getJSONArray("value");

                            for (int i = 0; i < jsArr.length(); i++) {
                                JSONObject currentObject = jsArr.getJSONObject(i);
                                String ID = currentObject.getString("CarParkID");
                                //Check for the correct iD and then store the Location and Agency
                                if (ID.equals(CarparkID)) {
                                    loc = currentObject.getString("Location");
                                    agency = currentObject.getString("Agency");
                                    Devlabel = currentObject.getString("Development");

                                    //Extracting out the Latitude and Longitude
                                    StringTokenizer str = new StringTokenizer(loc, " ");
                                    String lat = str.nextToken();
                                    String lon = str.nextToken();

                                    //Instantiate ontocarpark:hasAgency
                                    TriplePattern pattern = iri(carparkIRI).has(hasAgency, agency);
                                    InsertDataQuery insert = new InsertDataQuery();
                                    insert = Queries.INSERT_DATA(pattern);
                                    insert.prefix(PREFIX_ONTOCARPARK);
                                    kbClient.executeUpdate(insert.getQueryString());
                                    Devlabel = capitaliseFirstLetterOfEachWord(Devlabel);

                                    /**
                                     * //TriplePattern to query for whether buildingIRI hasAddress, if so, skip creating addressIRI and instantiating lat and long
                                     Variable add = SparqlBuilder.var("address");
                                     query = Queries.SELECT();
                                     pattern = iri(buildingIRI).has(hasAddress, add);
                                     query.prefix(PREFIX_ONTOBUILTENV).select(add).where(pattern);
                                     kbClient.setQuery(query.getQueryString());
                                     //and addressIRI = queryResult
                                     queryResult = kbClient.executeQuery();
                                     String addressIRI ;
                                     if(!queryResult.isEmpty()) {
                                     addressIRI = queryResult.getJSONObject(0).getString("address");
                                     } else {
                                     //else create addressIRI and link to buildingIRI via hasAddress and instantiate lat and long info
                                     addressIRI = OntoCarpark_NS + "Address_" + UUID.randomUUID();
                                     pattern = iri(buildingIRI).has(hasAddress, iri(addressIRI));
                                     insert = new InsertDataQuery();
                                     insert = Queries.INSERT_DATA(pattern);
                                     insert.prefix(PREFIX_ONTOBUILTENV);
                                     kbClient.executeUpdate(insert.getQueryString());
                                     }

                                     //Instantiate rdf:type for address and instantiate lat and long information
                                     pattern = iri(addressIRI).isA(Address).andHas(latitude, lat).andHas(longitude, lon);
                                     insert = new InsertDataQuery();
                                     insert = Queries.INSERT_DATA(pattern);
                                     insert.prefix(PREFIX_GEO, PREFIX_ICONTACT);
                                     kbClient.executeUpdate(insert.getQueryString());
                                     */

                                    //Instantiate rdfs:label
                                    pattern = iri(carparkIRI).has(label, Devlabel);
                                    insert = new InsertDataQuery();
                                    insert = Queries.INSERT_DATA(pattern);
                                    insert.prefix(PREFIX_RDFS);
                                    kbClient.executeUpdate(insert.getQueryString());
                                }
                            }
                        } catch (Exception e) {
                            LOGGER.fatal("Unable to execute query: {} {}", query.getQueryString(), e);
                            throw new JPSRuntimeException("Unable to execute query: " + query.getQueryString(), e);
                        }

                        /**
                         * Removed for testing
                         * //parse Development to street, street number, building and unit name
                         if (!Devlabel.contains("Street") & !Devlabel.contains("Avenue") & !Devlabel.contains("Blk") & !Devlabel.contains("St") & !Devlabel.contains("Rd") & !Devlabel.contains("Road")) {
                         pattern = iri(addressIRI).has(hasBuilding, Devlabel);
                         insert = new InsertDataQuery();
                         insert = Queries.INSERT_DATA(pattern);
                         insert.prefix(PREFIX_ICONTACT);
                         kbClient.executeUpdate(insert.getQueryString());

                         } else if ((Devlabel.contains("Centre") | Devlabel.contains("Market")) && (Devlabel.contains("Off Street") | Devlabel.contains("Off St"))) {
                         String buildingName = Devlabel.split("Off")[0];
                         pattern = iri(addressIRI).has(hasBuilding, buildingName.stripTrailing()).andHas(hasStreet, Devlabel);
                         insert = new InsertDataQuery();
                         insert = Queries.INSERT_DATA(pattern);
                         insert.prefix(PREFIX_ICONTACT);
                         kbClient.executeUpdate(insert.getQueryString());

                         } else if (Devlabel.contains("Lorong") & Devlabel.contains("Geylang")) {
                         String LorongWithStNum = Devlabel.split(" Geylang")[0];
                         String StNum = LorongWithStNum.split(" ")[1];
                         Devlabel = Devlabel.replace(StNum, "");
                         pattern = iri(addressIRI).has(hasStreetNumber, StNum).andHas(hasStreet, Devlabel);
                         insert = new InsertDataQuery();
                         insert = Queries.INSERT_DATA(pattern);
                         insert.prefix(PREFIX_ICONTACT);
                         kbClient.executeUpdate(insert.getQueryString());

                         } else if (Devlabel.contains("Off Street") | Devlabel.contains("Off St")) {
                         pattern = iri(addressIRI).has(hasStreet, Devlabel);
                         insert = new InsertDataQuery();
                         insert = Queries.INSERT_DATA(pattern);
                         insert.prefix(PREFIX_ICONTACT);
                         kbClient.executeUpdate(insert.getQueryString());

                         } else if (Devlabel.contains("Blk") & (Devlabel.contains("St") | Devlabel.contains("Street") | Devlabel.contains("Ave") | Devlabel.contains("Avenue"))) {
                         String[] listOfLabel = Devlabel.split(" ");
                         String blkAndBlkNum = listOfLabel[0] + " " + listOfLabel[1];
                         int a = 2;
                         if (StringUtils.isNumeric(listOfLabel[2])) {
                         blkAndBlkNum = blkAndBlkNum + " " + listOfLabel[2];
                         a = 3;
                         }
                         String streetAndStreetNum = "";
                         for (int k = a; k < listOfLabel.length; k ++) {
                         streetAndStreetNum = streetAndStreetNum + " " + listOfLabel[k];
                         streetAndStreetNum = streetAndStreetNum.trim();
                         }
                         pattern = iri(addressIRI).has(hasUnitName, blkAndBlkNum);
                         insert = new InsertDataQuery();
                         insert = Queries.INSERT_DATA(pattern);
                         insert.prefix(PREFIX_ONTOBUILTENV);
                         kbClient.executeUpdate(insert.getQueryString());

                         if (!streetAndStreetNum.contains("/")){
                         String street = "";
                         String[] listOfStreetAndStreetNum = streetAndStreetNum.split(" ");
                         //Ang Mo Kio Ave 6, street number is at the last index
                         String streetNum = null;
                         if (StringUtils.isNumeric(listOfStreetAndStreetNum[listOfStreetAndStreetNum.length - 1])) {
                         streetNum = listOfStreetAndStreetNum[listOfStreetAndStreetNum.length - 1];
                         }
                         for (int l = 0; l < listOfStreetAndStreetNum.length - 1; l++) {
                         street = street + " " + listOfStreetAndStreetNum[l];
                         street = street.trim();
                         }
                         if (streetNum != null) {
                         pattern = iri(addressIRI).has(hasStreetNumber, streetNum).andHas(hasStreet, street);
                         } else {
                         pattern = iri(addressIRI).has(hasStreet, street);
                         }
                         insert = new InsertDataQuery();
                         insert = Queries.INSERT_DATA(pattern);
                         insert.prefix(PREFIX_ICONTACT);
                         kbClient.executeUpdate(insert.getQueryString());

                         } else if (streetAndStreetNum.contains("/")) {
                         String addressIRI2 = OntoCarpark_NS + "Address_" + UUID.randomUUID();
                         String[] listOfStreetAndStreetNum = streetAndStreetNum.split("/");

                         // if the street and street number string is something like this "Ang Mo Kio Ave 6/8"
                         // listOfStreetAndStreetNum[1] = "8" and it's length = 1
                         if (listOfStreetAndStreetNum[1].length() <= 2) {
                         String [] firstStreetAndStreetNum = listOfStreetAndStreetNum[0].split(" ");
                         String streetNum = firstStreetAndStreetNum[firstStreetAndStreetNum.length - 1];
                         String street = "" ;

                         for (int l = 0; l < firstStreetAndStreetNum.length - 1; l++) {
                         street = street + " " + firstStreetAndStreetNum[l];
                         street = street.trim();
                         }

                         pattern = iri(addressIRI).has(hasStreetNumber, streetNum).andHas(hasStreet, street);
                         insert = new InsertDataQuery();
                         insert = Queries.INSERT_DATA(pattern);
                         insert.prefix(PREFIX_ICONTACT);
                         kbClient.executeUpdate(insert.getQueryString());

                         String streetNum2 = listOfStreetAndStreetNum[1] ;
                         pattern = iri(buildingIRI).has(hasAddress, iri(addressIRI2));
                         TriplePattern pattern2 = iri(addressIRI2).isA(Address).andHas(hasUnitName, blkAndBlkNum).andHas(hasStreet, street).andHas(hasStreetNumber, streetNum2).andHas(latitude, lat).andHas(longitude, lon);
                         insert = new InsertDataQuery();
                         insert = Queries.INSERT_DATA(pattern, pattern2);
                         insert.prefix(PREFIX_ICONTACT, PREFIX_GEO, PREFIX_ONTOBUILTENV);
                         kbClient.executeUpdate(insert.getQueryString());
                         } else {
                         //Yishun Street 71,72/Avenue 5
                         //Bedok south road/Ave 3
                         if (StringUtils.isNumeric(listOfStreetAndStreetNum[1].split(" ")[1])) {
                         String streetNum2 = listOfStreetAndStreetNum[1].split(" ")[1];
                         String street2 = listOfStreetAndStreetNum[1].split(" ")[0];
                         String [] firstStreetAndStreetNum = listOfStreetAndStreetNum[0].split(" ");
                         String streetNum = null;
                         if (StringUtils.isNumeric(firstStreetAndStreetNum[firstStreetAndStreetNum.length - 1].split(",")[0])) {
                         streetNum = firstStreetAndStreetNum[firstStreetAndStreetNum.length - 1];
                         }
                         String street = "" ;

                         for (int l = 0; l < firstStreetAndStreetNum.length - 1; l++) {
                         street = street + " " + firstStreetAndStreetNum[l];
                         street = street.trim();
                         }

                         if (street.contains("Yishun")) {
                         street2 = "Yishun" + street2 ;
                         } else if (street.contains("Bedok")) {
                         street2 = "Bedok" + street2 ;
                         }

                         if (streetNum != null) {
                         pattern = iri(addressIRI).has(hasStreetNumber, streetNum).andHas(hasStreet, street);
                         insert = new InsertDataQuery();
                         insert = Queries.INSERT_DATA(pattern);
                         insert.prefix(PREFIX_ICONTACT);
                         kbClient.executeUpdate(insert.getQueryString());
                         } else {
                         pattern = iri(addressIRI).has(hasStreet, street);
                         insert = new InsertDataQuery();
                         insert = Queries.INSERT_DATA(pattern);
                         insert.prefix(PREFIX_ICONTACT);
                         kbClient.executeUpdate(insert.getQueryString());
                         }

                         pattern = iri(buildingIRI).has(hasAddress, iri(addressIRI2));
                         TriplePattern pattern2 = iri(addressIRI2).isA(Address).andHas(hasUnitName, blkAndBlkNum).andHas(hasStreet, street).andHas(hasStreetNumber, streetNum2).andHas(latitude, lat).andHas(longitude, lon);
                         insert = new InsertDataQuery();
                         insert = Queries.INSERT_DATA(pattern, pattern2);
                         insert.prefix(PREFIX_ICONTACT, PREFIX_GEO, PREFIX_ONTOBUILTENV);
                         kbClient.executeUpdate(insert.getQueryString());

                         } else {
                         //Yishun Street 11/Ring Road
                         String street2 = listOfStreetAndStreetNum[1];
                         String streetNum = null;
                         String [] firstStreetAndStreetNum = listOfStreetAndStreetNum[0].split(" ");
                         if (StringUtils.isNumeric(firstStreetAndStreetNum[firstStreetAndStreetNum.length - 1])) {
                         streetNum = firstStreetAndStreetNum[firstStreetAndStreetNum.length - 1];
                         }
                         String street = "" ;

                         for (int l = 0; l < firstStreetAndStreetNum.length - 1; l++) {
                         street = street + " " + firstStreetAndStreetNum[l];
                         street = street.trim();
                         }

                         if (street.contains("Yishun")) {
                         street2 = "Yishun" + street2 ;
                         } else if (street.contains("Bedok")) {
                         street2 = "Bedok" + street2 ;
                         }

                         if (streetNum != null) {
                         pattern = iri(addressIRI).has(hasStreetNumber, streetNum).andHas(hasStreet, street);
                         insert = new InsertDataQuery();
                         insert = Queries.INSERT_DATA(pattern);
                         insert.prefix(PREFIX_ICONTACT);
                         kbClient.executeUpdate(insert.getQueryString());
                         } else {
                         pattern = iri(addressIRI).has(hasStreet, street);
                         insert = new InsertDataQuery();
                         insert = Queries.INSERT_DATA(pattern);
                         insert.prefix(PREFIX_ICONTACT);
                         kbClient.executeUpdate(insert.getQueryString());
                         }
                         pattern = iri(buildingIRI).has(hasAddress, iri(addressIRI2));
                         TriplePattern pattern2 = iri(addressIRI2).isA(Address).andHas(hasUnitName, blkAndBlkNum).andHas(hasStreet, street).andHas(latitude, lat).andHas(longitude, lon);
                         insert = new InsertDataQuery();
                         insert = Queries.INSERT_DATA(pattern, pattern2);
                         insert.prefix(PREFIX_ICONTACT, PREFIX_GEO, PREFIX_ONTOBUILTENV);
                         kbClient.executeUpdate(insert.getQueryString());

                         }
                         }
                         } else {
                         pattern = iri(addressIRI).has(hasStreet, streetAndStreetNum);
                         insert = new InsertDataQuery();
                         insert = Queries.INSERT_DATA(pattern);
                         insert.prefix(PREFIX_ICONTACT);
                         kbClient.executeUpdate(insert.getQueryString());
                         }
                         } else if (Devlabel.contains("Blk") & (Devlabel.contains("Road") | (Devlabel.contains("Rd")))) {
                         String[] listOfLabel = Devlabel.split(" ");
                         String blkAndBlkNum = listOfLabel[0] +  " " + listOfLabel[1];
                         int a = 2;
                         //Blk 22/24, 59/63, 803/805 Chai Chee Road
                         if (listOfLabel[1].lastIndexOf(",") == listOfLabel[1].length()-1) {
                         blkAndBlkNum = blkAndBlkNum + " " + listOfLabel[2];
                         a = 3;
                         if (listOfLabel[2].lastIndexOf(",") == listOfLabel[2].length()-1) {
                         blkAndBlkNum = blkAndBlkNum + " " + listOfLabel[3];
                         a = 4;
                         }
                         //Blk 85 To 94/92A Pipit Road
                         } else if (listOfLabel[2].contains("To")) {
                         blkAndBlkNum = blkAndBlkNum + " " + listOfLabel[2] + " " + listOfLabel[3];
                         a = 4;
                         }

                         String streets = "";
                         for (int k = a; k < listOfLabel.length; k ++) {
                         streets = streets + " " + listOfLabel[k];
                         streets = streets.trim();
                         }

                         pattern = iri(addressIRI).has(hasUnitName, blkAndBlkNum);
                         insert = new InsertDataQuery();
                         insert = Queries.INSERT_DATA(pattern);
                         insert.prefix(PREFIX_ONTOBUILTENV);
                         kbClient.executeUpdate(insert.getQueryString());

                         if (streets.contains("/")) {
                         //Blk 101/129 Gangsa Road/Pending Road
                         //Blk 141-151 Gangsa/Petir Road
                         String firstStreet = streets.split("/")[0];
                         String secondStreet = streets.split("/")[1];
                         if (!firstStreet.contains("Road")) {
                         firstStreet = firstStreet + " Road" ;
                         }
                         String addressIRI2 = OntoCarpark_NS + "Address_" + UUID.randomUUID();
                         pattern = iri(addressIRI).has(hasStreet, firstStreet);
                         insert = new InsertDataQuery();
                         insert = Queries.INSERT_DATA(pattern);
                         insert.prefix(PREFIX_ICONTACT);
                         kbClient.executeUpdate(insert.getQueryString());

                         pattern = iri(buildingIRI).has(hasAddress, iri(addressIRI2));
                         TriplePattern pattern2 = iri(addressIRI2).isA(Address).andHas(hasUnitName, blkAndBlkNum).andHas(hasStreet, secondStreet).andHas(latitude, lat).andHas(longitude, lon);
                         insert = new InsertDataQuery();
                         insert = Queries.INSERT_DATA(pattern, pattern2);
                         insert.prefix(PREFIX_ICONTACT, PREFIX_GEO, PREFIX_ONTOBUILTENV);
                         kbClient.executeUpdate(insert.getQueryString());

                         } else {
                         pattern = iri(addressIRI).has(hasStreet, streets);
                         insert = new InsertDataQuery();
                         insert = Queries.INSERT_DATA(pattern);
                         insert.prefix(PREFIX_ICONTACT);
                         kbClient.executeUpdate(insert.getQueryString());
                         }
                         } else if (Devlabel.contains("Car Park")) {
                         String[] listOfLabel = Devlabel.split(" ");
                         //Blk 270/271 Albert Centre Basement Car Park
                         if (Devlabel.contains("Blk")) {

                         String blkAndBlkNum = listOfLabel[0] + " " + listOfLabel[1];

                         String building = "" ;
                         for (int k = 2; k < listOfLabel.length - 2; k ++) {
                         building = building + " " + listOfLabel[k];
                         building = building.trim();
                         }

                         pattern = iri(addressIRI).has(hasBuilding, building).andHas(hasUnitName, blkAndBlkNum);
                         insert = new InsertDataQuery();
                         insert = Queries.INSERT_DATA(pattern);
                         insert.prefix(PREFIX_ICONTACT, PREFIX_ONTOBUILTENV);
                         kbClient.executeUpdate(insert.getQueryString());

                         } else {
                         String building = "" ;
                         for (int k = 0; k < listOfLabel.length - 2; k ++) {
                         building = building + " " + listOfLabel[k];
                         building = building.trim();
                         }
                         pattern = iri(addressIRI).has(hasBuilding, building);
                         insert = new InsertDataQuery();
                         insert = Queries.INSERT_DATA(pattern);
                         insert.prefix(PREFIX_ICONTACT);
                         kbClient.executeUpdate(insert.getQueryString());
                         }
                         } else if (Devlabel.contains("Blk")) {
                         String[] listOfLabel = Devlabel.split(" ");
                         String blkAndBlkNum = listOfLabel[0] + " " + listOfLabel[1];
                         String street = "" ;
                         for (int k = 2; k < listOfLabel.length; k ++) {
                         street = street + " " + listOfLabel[k];
                         street = street.trim();
                         }

                         pattern = iri(addressIRI).has(hasStreet, street).andHas(hasUnitName, blkAndBlkNum);
                         insert = new InsertDataQuery();
                         insert = Queries.INSERT_DATA(pattern);
                         insert.prefix(PREFIX_ICONTACT, PREFIX_ONTOBUILTENV);
                         kbClient.executeUpdate(insert.getQueryString());

                         } else {
                         String street = Devlabel;
                         pattern = iri(addressIRI).has(hasStreet, street);
                         insert = new InsertDataQuery();
                         insert = Queries.INSERT_DATA(pattern);
                         insert.prefix(PREFIX_ICONTACT);
                         kbClient.executeUpdate(insert.getQueryString());
                         }
                         }
                         }
                         } catch (Exception e) {
                         throw new JPSRuntimeException("Unable to execute query: " + query.getQueryString(), e);
                         }

                         */

                        //FuzzyMatching for the carpark Prices

                        String saturdayRate = "Carpark prices unavailable", weekday = "Carpark prices unavailable", sundayAndPHRates = "Carpark prices unavailable";
                        int check = 0;

                        for (int i = 0; i < carparkRates.length(); i++) {
                            JSONObject currentCarpark = carparkRates.getJSONObject(i);
                            String currentName = currentCarpark.getString("carpark");

                            //is currentCarpark same as the Devlabel of the IRI
                            if (!Devlabel.equals("") && FuzzySearch.tokenSetRatio(currentName.toLowerCase(), Devlabel.toLowerCase()) > 90 && FuzzySearch.partialRatio(currentName.toLowerCase(), Devlabel.toLowerCase()) > 75 && FuzzySearch.tokenSortRatio(currentName.toLowerCase(), Devlabel.toLowerCase()) > 83)
                            //if(!Devlabel.equals("") && FuzzySearch.tokenSetRatio(currentName,Devlabel.toLowerCase(Locale.ROOT))>52 && FuzzySearch.weightedRatio(currentName,Devlabel.toLowerCase(Locale.ROOT))>75 && FuzzySearch.partialRatio(currentName,Devlabel.toLowerCase(Locale.ROOT))>59 && FuzzySearch.tokenSortRatio(currentName,Devlabel.toLowerCase(Locale.ROOT))>52)
                            {
                                check = 1;

                                saturdayRate = currentCarpark.getString("saturday_rate");
                                sundayAndPHRates = currentCarpark.getString("sunday_publicholiday_rate");
                                String weekday1 = currentCarpark.getString("weekdays_rate_1");
                                String weekday2 = currentCarpark.getString("weekdays_rate_2");

                                if (!(weekday2.equals("-")) && !(weekday2.equals(weekday1))) {
                                    weekday = weekday1 + ";" + weekday2;
                                } else {
                                    weekday = weekday1;
                                }

                                if (sundayAndPHRates.equals("Same as Saturday")) {
                                    sundayAndPHRates = saturdayRate;
                                }

                                if (sundayAndPHRates.equals("Same as wkdays") || sundayAndPHRates.equals("Same as weekdays")) {
                                    sundayAndPHRates = weekday;
                                }

                                if (saturdayRate.equals("Same as wkdays") || saturdayRate.equals("Same as weekdays")) {
                                    saturdayRate = weekday;
                                }
                            }
                            if (check == 1)
                                i = carparkRates.length();
                        }

                        TriplePattern pattern9 = iri(carparkIRI).has(hasWeekdayRates, weekday);
                        InsertDataQuery insert11 = Queries.INSERT_DATA(pattern9);
                        insert11.prefix(PREFIX_ONTOCARPARK);
                        kbClient.executeUpdate(insert11.getQueryString());

                        TriplePattern pattern10 = iri(carparkIRI).has(hasSaturdayRates, saturdayRate);
                        InsertDataQuery insert12 = Queries.INSERT_DATA(pattern10);
                        insert12.prefix(PREFIX_ONTOCARPARK);
                        kbClient.executeUpdate(insert12.getQueryString());

                        TriplePattern pattern11 = iri(carparkIRI).has(hasSundayAndPHRates, sundayAndPHRates);
                        InsertDataQuery insert13 = Queries.INSERT_DATA(pattern11);
                        insert13.prefix(PREFIX_ONTOCARPARK);
                        kbClient.executeUpdate(insert13.getQueryString());
                    }
                }
            }
        }
    }
}
