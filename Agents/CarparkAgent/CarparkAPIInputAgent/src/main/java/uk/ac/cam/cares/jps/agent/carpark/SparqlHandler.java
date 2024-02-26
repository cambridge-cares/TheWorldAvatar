package uk.ac.cam.cares.jps.agent.carpark;

import org.json.JSONArray;
import org.json.JSONObject;
import org.springframework.core.io.ClassPathResource;

import com.cmclinnovations.stack.clients.ontop.OntopClient;

import uk.ac.cam.cares.jps.base.util.JSONKeyToIRIMapper;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.nio.file.Path;
import java.sql.Connection;
import java.util.*;

import me.xdrop.fuzzywuzzy.FuzzySearch;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import org.eclipse.rdf4j.sparqlbuilder.core.query.DeleteDataQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.InsertDataQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;

public class SparqlHandler {
    private static final Logger LOGGER = LogManager.getLogger(SparqlHandler.class);
    RemoteStoreClient kbClient;

    /**
     * Error messages
     */
    private static final String CREATEGEOSPATIAL_ERROR_MSG = "Unable to instantiate geospatial information for the following ";

    /**
     * Namespaces for ontologies
     */
    private static final String OntoCarpark_NS = "https://www.theworldavatar.com/kg/ontocarpark/";
    private static final String RDFS_NS = "http://www.w3.org/2000/01/rdf-schema#";

    /**
     * Prefixes
     */
    private static final Prefix PREFIX_ONTOCARPARK = SparqlBuilder.prefix("ontocarpark", iri(OntoCarpark_NS));
    private static final Prefix PREFIX_RDFS = SparqlBuilder.prefix("rdfs", iri(RDFS_NS));

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

    /**
     * Classes
     */
    private static final Iri AvailableLots = PREFIX_ONTOCARPARK.iri("AvailableLots");
    private static final Iri Carpark = PREFIX_ONTOCARPARK.iri("Carpark");
    private static final Iri Cars = PREFIX_ONTOCARPARK.iri("Cars");
    private static final Iri Motorcycles = PREFIX_ONTOCARPARK.iri("Motorcycles");
    private static final Iri HeavyVehicles = PREFIX_ONTOCARPARK.iri("HeavyVehicles");
    

    BuildingMatchingClient buildingMatchingClient;

    private final List<JSONKeyToIRIMapper> mappings;

    /**
     * Constructor for SparqlHandler
     * @param mappings mappings of keys to data IRIs
     * @param kbClient remoteStoreClient instance
     */
    public SparqlHandler(List<JSONKeyToIRIMapper> mappings, RemoteStoreClient kbClient) {
        this.kbClient = kbClient;
        this.mappings = mappings;
    }

    /**
     * Main method that instantiates all the ABoxes for the carpark if not instantiated yet
     * @param carparkReadings readings retrieved via API that contains the carpark general information and available lots
     * @param ratesReadings readings retrieved via API that contains the carpark rates
     */
    public void instantiateIfNotInstantiated(JSONObject carparkReadings, JSONObject ratesReadings) {
        List<String> iris;
        String carparkLabel;
        String agency;
        JSONArray carparkRates = ratesReadings.getJSONObject("result").getJSONArray("records");
        for (JSONKeyToIRIMapper mapping : mappings) {
            iris = mapping.getAllIRIs();
            for (String iri : iris) {
                if (!iri.contains("carpark_time_")) {
                    //Check for rdf:type ontocarpark:AvailableLots for each data IRI and instantiate if not exist
                    Boolean check = instantiateAvailableLotsIfNotExist(iri);

                    //Retrieve carpark ID and lot type from data IRI
                    StringTokenizer stringTokenizer = new StringTokenizer(iri, "_");
                    for (int i = 0; i < 2; i++) {
                        stringTokenizer.nextToken();
                    }
                    String carparkID = stringTokenizer.nextToken();
                    String lotType = stringTokenizer.nextToken();

                    JSONArray jsArr = carparkReadings.getJSONArray("value");
                    for (int i = 0; i < jsArr.length(); i++) {
                        String carparkIRI = null;
                        JSONObject currentObject = jsArr.getJSONObject(i);
                        String ID = currentObject.getString("CarParkID");
                        //Check for the correct ID and then store the Location and Agency
                        if (ID.equals(carparkID)) {
                            //check == false indicate that the carpark ABoxes have not been created for the data IRI
                            if (check == false) {
                                agency = currentObject.getString("Agency");
                                carparkLabel = currentObject.getString("Development");
                                String location = currentObject.getString("Location");
                                String latitude = location.split(" ")[0];
                                String longitude = location.split(" ")[1];

                                //Instantiate lot type IRI and link to data IRI
                                String lotTypeIri = instantiateLotTypeAndAttributesIfNotExist(iri, lotType);

                                //Instantiate carpark IRI and it's attributes
                                carparkIRI = instantiateCarparkAndAttributesIfnotExist(carparkID, carparkLabel, lotTypeIri, agency, latitude, longitude);

                                //FuzzyMatching for the carpark rates
                                Map<String, String> map = new HashMap<>();

                                for (int j = 0; j < carparkRates.length(); j++) {
                                    JSONObject currentCarpark = carparkRates.getJSONObject(j);
                                    String currentName = currentCarpark.getString("carpark");

                                    //Match carpark name retrieved from carpark rates JSONObject with the carpark names retrieved from available lots JSONObject
                                    if (FuzzySearch.tokenSetRatio(currentName.toLowerCase(), carparkLabel.toLowerCase()) > 90 && FuzzySearch.partialRatio(currentName.toLowerCase(), carparkLabel.toLowerCase()) > 75 && FuzzySearch.tokenSortRatio(currentName.toLowerCase(), carparkLabel.toLowerCase()) > 83) {
                                        //if(!Devlabel.equals("") && FuzzySearch.tokenSetRatio(currentName,Devlabel.toLowerCase(Locale.ROOT))>52 && FuzzySearch.weightedRatio(currentName,Devlabel.toLowerCase(Locale.ROOT))>75 && FuzzySearch.partialRatio(currentName,Devlabel.toLowerCase(Locale.ROOT))>59 && FuzzySearch.tokenSortRatio(currentName,Devlabel.toLowerCase(Locale.ROOT))>52)
                                            
                                        //Retrieve carpark rates and place into a map
                                        map = parseCarparkRates(currentCarpark);

                                        //Check whether there are existing carpark rates and are they the same as those retrieved from the JSONObject
                                        checkAndUpdateRates(map, carparkIRI);
                                    }
                                }   
                            } else {
                                //Carpark ABoxes have already been created
                                //check whether a carpark instance has already been created for a particular carpark ID
                                Variable carpark = SparqlBuilder.var("carpark");
                                SelectQuery selectQuery = Queries.SELECT();
                                TriplePattern triplePattern = carpark.has(hasID, carparkID);
                                selectQuery.prefix(PREFIX_ONTOCARPARK).select(carpark).where(triplePattern);
                                kbClient.setQuery(selectQuery.getQueryString());
                                JSONArray queryResult = kbClient.executeQuery();
                                carparkIRI = queryResult.getJSONObject(0).getString("carpark");
                                //FuzzyMatching for the carpark rates
                                Map<String, String> map = new HashMap<>();

                                for (int j = 0; j < carparkRates.length(); j++) {
                                    JSONObject currentCarpark = carparkRates.getJSONObject(j);
                                    String currentName = currentCarpark.getString("carpark");
                                    carparkLabel = currentObject.getString("Development");

                                    //Match carpark name retrieved from carpark rates JSONObject with the carpark names retrieved from available lots JSONObject
                                    if (FuzzySearch.tokenSetRatio(currentName.toLowerCase(), carparkLabel.toLowerCase()) > 90 && FuzzySearch.partialRatio(currentName.toLowerCase(), carparkLabel.toLowerCase()) > 75 && FuzzySearch.tokenSortRatio(currentName.toLowerCase(), carparkLabel.toLowerCase()) > 83) {
                                        //if(!Devlabel.equals("") && FuzzySearch.tokenSetRatio(currentName,Devlabel.toLowerCase(Locale.ROOT))>52 && FuzzySearch.weightedRatio(currentName,Devlabel.toLowerCase(Locale.ROOT))>75 && FuzzySearch.partialRatio(currentName,Devlabel.toLowerCase(Locale.ROOT))>59 && FuzzySearch.tokenSortRatio(currentName,Devlabel.toLowerCase(Locale.ROOT))>52)
                                            
                                        //Retrieve carpark rates and place into a map
                                        map = parseCarparkRates(currentCarpark);

                                        //Check whether there are existing carpark rates and are they the same as those retrieved from the JSONObject
                                        checkAndUpdateRates(map, carparkIRI);
                                    }
                                } 
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Instantiate rdf:type ontocarpark:AvailableLots for each data IRI if not yet instantiated
     * @param iri data IRI
     * @return True if data IRI already has rdf:type ontocarpark:AvailableLots else return false
     */
    private Boolean instantiateAvailableLotsIfNotExist(String iri) {
        Boolean check = true;
        Variable rdf_type = SparqlBuilder.var("rdf_type");
        SelectQuery selectQuery = Queries.SELECT();
        //SELECT ?rdf_type WHERE { <iri> rdf:type ?rdf_type }
        TriplePattern triplePattern = iri(iri).isA(rdf_type);
        selectQuery.prefix(PREFIX_ONTOCARPARK).select(rdf_type).where(triplePattern);
        kbClient.setQuery(selectQuery.getQueryString());
        try {
            JSONArray queryResult = kbClient.executeQuery();
            if (queryResult.isEmpty()) {
                triplePattern = iri(iri).isA(AvailableLots);
                // INSERT DATA { <iri> rdf:type ontocarpark:AvailableLots }
                InsertDataQuery insertQuery = Queries.INSERT_DATA(triplePattern);
                insertQuery.prefix(PREFIX_ONTOCARPARK);
                kbClient.executeUpdate(insertQuery.getQueryString());
                check = false;
            }
        } catch (Exception e) {
            throw new JPSRuntimeException("Could not check for an rdf:type for the Data IRI: " + iri);
        }
        return check;
    }

    /**
     * Instantiate appropriate lot type (car, motorcycle, heavy vehicles etc) for each data IRI if not yet instantiated
     * @param iri data IRI
     * @param lotType lot type (car, motorcycle, heavy vehicle etc)
     * @return lot type IRI
     */
    private String instantiateLotTypeAndAttributesIfNotExist(String iri, String lotType) {
        TriplePattern triplePattern;
        TriplePattern triplePattern2;
        InsertDataQuery insertQuery;
        SelectQuery selectQuery;
        String lotTypeIRI;
        //Check for ?lotType ontocarpark:hasLots <iri>
        Variable lotTypeIRIVar = SparqlBuilder.var("lotTypeIRIVar");
        triplePattern = (lotTypeIRIVar).has(hasLots, iri(iri));
        selectQuery = Queries.SELECT(lotTypeIRIVar).where(triplePattern);
        selectQuery.prefix(PREFIX_ONTOCARPARK);
        kbClient.setQuery(selectQuery.getQueryString());
        JSONArray queryResult = kbClient.executeQuery();
        try {
            if (queryResult.isEmpty()) {
                lotTypeIRI = OntoCarpark_NS + "Carpark_LotType_" + UUID.randomUUID();
                if (lotType.equalsIgnoreCase("C")) {
                    //INSERT DATA { <lotTypeIRI> rdf:type ontocarpark:Cars .
                    //              <lotTypeIRI> rdfs:label "Cars"
                    //              <iri> rdfs:label "Cars Available Lots" .}
                    triplePattern = iri(lotTypeIRI).isA(Cars).andHas(label, "Cars");
                    triplePattern2 = iri(iri).has(label, "Cars Available Lots");
                    insertQuery = Queries.INSERT_DATA(triplePattern, triplePattern2);
                    insertQuery.prefix(PREFIX_ONTOCARPARK, PREFIX_RDFS);
                    kbClient.executeUpdate(insertQuery.getQueryString());
                } else if (lotType.equalsIgnoreCase("H")) {
                    //INSERT DATA { <lotTypeIRI> rdf:type ontocarpark:HeavyVehicles.
                    //              <lotTypeIRI> rdfs:label "Heavy Vehicles" .
                    //              <iri> rdfs:label "Heavy Vehicles Available Lots" . }
                    triplePattern = iri(lotTypeIRI).isA(HeavyVehicles).andHas(label, "Heavy Vehicles");
                    triplePattern2 = iri(iri).has(label, "Heavy Vehicles Available Lots");
                    insertQuery = Queries.INSERT_DATA(triplePattern, triplePattern2);
                    insertQuery.prefix(PREFIX_ONTOCARPARK, PREFIX_RDFS);
                    kbClient.executeUpdate(insertQuery.getQueryString());
                } else {
                    //INSERT DATA { <lotTypeIRI> rdf:type ontocarpark:Motorcycles.
                    //              <lotTypeIRI> rdfs:label "Motorcycles" .
                    //              <iri> rdfs:label "Motorcycles Available Lots" . }
                    triplePattern = iri(lotTypeIRI).isA(Motorcycles).andHas(label, "Motorcycles");
                    triplePattern2 = iri(iri).has(label, "Motorcycles Available Lots");
                    insertQuery = Queries.INSERT_DATA(triplePattern, triplePattern2);
                    insertQuery.prefix(PREFIX_ONTOCARPARK, PREFIX_RDFS);
                    kbClient.executeUpdate(insertQuery.getQueryString());
                }
                //TriplePattern to link LotType IRI to data IRI
                //INSERT DATA { <lotTypeIRI> ontocarpark:hasLots <iri> }
                triplePattern = iri(lotTypeIRI).has(hasLots, iri(iri));
                insertQuery = new InsertDataQuery();
                insertQuery = Queries.INSERT_DATA(triplePattern);
                insertQuery.prefix(PREFIX_ONTOCARPARK);
                kbClient.executeUpdate(insertQuery.getQueryString());
            } else {
                lotTypeIRI = queryResult.getJSONObject(0).getString("lotTypeIRIVar");
            }
        } catch (Exception e) {
            throw new JPSRuntimeException("Could not check for lot type IRI for the Data IRI: " + iri, e);
        }
        return lotTypeIRI;
    }

    /**
     * Instantiate carpark and it's attributes if not yet instantiated
     * @param carparkID carpark ID
     * @param carparkName carpark label
     * @param lotTypeIRI lot type IRI
     * @param agency agency in charged of the carpark
     * @param latitude latitude of carpark's coordinate
     * @param longitude longitude of carpark's coordinate
     * @return carpark IRI
     */
    private String instantiateCarparkAndAttributesIfnotExist(String carparkID, String carparkName, String lotTypeIRI, String agency, String latitude, String longitude) {
        //check whether a carpark instance has already been created for a particular carpark ID
        Variable carpark = SparqlBuilder.var("carpark");
        SelectQuery selectQuery = Queries.SELECT();
        //SELECT ?carpark WHERE { ?carpark ontocarpark:hasID "carparkID" }
        TriplePattern triplePattern = carpark.has(hasID, carparkID);
        selectQuery.prefix(PREFIX_ONTOCARPARK).select(carpark).where(triplePattern);
        kbClient.setQuery(selectQuery.getQueryString());
        String carparkIRI;
    
        try {
            JSONArray queryResult = kbClient.executeQuery();

            if (!queryResult.isEmpty()) {
                //reuse the carpark instance created previously and link it to the different lotTypes IRIs
                carparkIRI = queryResult.getJSONObject(0).getString("carpark");
            } else {
                //if queryresult is empty, create a carpark instance
                carparkIRI = OntoCarpark_NS + "Carpark_" + UUID.randomUUID();
            }
            //instantiate rdf:type for carpark
            instantiateCarpark(carparkIRI);

            //instantiate carpark label via rdfs:label
            instantiateCarparkLabel(carparkIRI, carparkName);

            //instantiate carpark ID via ontocarpark:hasID
            instantiateCarparkID(carparkIRI, carparkID);

            //Link Carpark IRI to LotType IRI via ontocarpark:hasLotType
            instantiateHasLotType(carparkIRI, lotTypeIRI);

            //Instantiate carpark Agency via ontocarpark:hasAgency
            instantiateCarparkAgency(carparkIRI, agency);

            //Instantiate carpark uuid, carpark iri, name, lat long coordinates via GeospatialClient
            instantiateGeoSpatialInfoIfNotExist(carparkIRI, Double.parseDouble(latitude), Double.parseDouble(longitude), carparkName);

        } catch (Exception e) {
            throw new JPSRuntimeException(e);
        }
        return carparkIRI;
    }

    /**
     * Instantiate geospatial information via the CarparkGeospatialClient
     * @param carparkIRI carpark IRI
     * @param latitude latitude of carpark's coordinate
     * @param longitude longitude of carpark's coordinate
     * @param carparkName name of carpark
     */
    private void instantiateGeoSpatialInfoIfNotExist(String carparkIRI, Double latitude, Double longitude, String carparkName) {
        //postgisClient is used to interact with the stack's database that will store the geolocation information
        CarparkPostGISClient postgisClient = new CarparkPostGISClient();

        try (Connection conn = postgisClient.getConnection()) {
            JSONObject response = new JSONObject();
            CarparkGeospatialClient geospatialClient = new CarparkGeospatialClient();
			if (!postgisClient.checkTableExists(CarparkAgent.LAYERNAME, conn)) {
                LOGGER.info("The table " + CarparkAgent.LAYERNAME + " does not exist");
                // add ontop mapping file
			    Path obda_file = new ClassPathResource("ontop.obda").getFile().toPath();
                OntopClient ontopClient = OntopClient.getInstance();
			    ontopClient.updateOBDA(obda_file);
                
                geospatialClient.createGeospatialInformation(latitude, longitude, carparkName, carparkIRI);
				response.put("message", "Geospatial information instantiated for the following: " + carparkName);
			} else {
				// table exists, check table contents for an equivalent point or carpark
                LOGGER.info("Checking for existing carparks to prevent duplicates...");
				if (!postgisClient.checkCarparkExists(carparkIRI, conn)) {
                    LOGGER.info("No existing carpark instance found for " + carparkIRI);
                    geospatialClient.createGeospatialInformation(latitude, longitude, carparkName, carparkIRI);
					response.put("message", "Geospatial information instantiated for the following: " + carparkName);
				} else {
					response.put("message", "A carpark already exist for the following: " + carparkIRI);
				}
			}
		} catch (Exception e) {
			LOGGER.error(CREATEGEOSPATIAL_ERROR_MSG + carparkName, e);
            throw new JPSRuntimeException(CREATEGEOSPATIAL_ERROR_MSG + carparkName);
		}
    }

    /**
     * Instantiate rdf:type ontocarpark:Carpark for a carpark IRI
     * @param carparkIRI carpark IRI
     */
    private void instantiateCarpark(String carparkIRI) {
        //instantiate rdf:type ontocarpark:Carpark for the carpark instance
        //INSERT DATA { <carparkIRI> rf:type ontocarpark:Carpark }
        TriplePattern triplePattern = iri(carparkIRI).isA(Carpark);
        InsertDataQuery insertQuery;
        insertQuery = new InsertDataQuery();
        insertQuery = Queries.INSERT_DATA(triplePattern);
        insertQuery.prefix(PREFIX_ONTOCARPARK);
        kbClient.executeUpdate(insertQuery.getQueryString());
    }

    /**
     * Instantiate rdfs:label for a carpark IRI
     * @param carparkIRI carpark IRI
     * @param carparkLabel carpark label
     */
    private void instantiateCarparkLabel(String carparkIRI, String carparkLabel) {
        carparkLabel = capitaliseFirstLetterOfEachWord(carparkLabel);
        TriplePattern triplePattern;
        InsertDataQuery insertQuery;
        //INSERT DATA { <carparkIRI> rdfs:label "carparkLabel" }
        triplePattern = iri(carparkIRI).has(label, carparkLabel);
        insertQuery = new InsertDataQuery();
        insertQuery = Queries.INSERT_DATA(triplePattern);
        insertQuery.prefix(PREFIX_RDFS);
        kbClient.executeUpdate(insertQuery.getQueryString());
    }

    /**
     * Instantiate carpark ID for a carpark IRI
     * @param carparkIRI carpark IRI
     * @param carparkID carpark ID
     */
    private void instantiateCarparkID(String carparkIRI, String carparkID) {
        TriplePattern triplePattern;
        InsertDataQuery insertQuery;
        //INSERT DATA { <carparkIRI> ontocarpark:hasID "carparkID" }
        triplePattern = iri(carparkIRI).has(hasID, carparkID);
        insertQuery = new InsertDataQuery();
        insertQuery = Queries.INSERT_DATA(triplePattern);
        insertQuery.prefix(PREFIX_ONTOCARPARK);
        kbClient.executeUpdate(insertQuery.getQueryString());
    }

    /**
     * Instantiate carpark Agency for a carpark IRI
     * @param carparkIRI carpark IRI
     * @param Agency agency in charge of the carpark
     */
    private void instantiateCarparkAgency(String carparkIRI, String Agency) {
        //INSERT DATA { <carparkIRI> ontocarpark:hasAgency "Agency" }
        TriplePattern triplePattern = iri(carparkIRI).has(hasAgency, Agency);
        InsertDataQuery insertQuery = new InsertDataQuery();
        insertQuery = Queries.INSERT_DATA(triplePattern);
        insertQuery.prefix(PREFIX_ONTOCARPARK);
        kbClient.executeUpdate(insertQuery.getQueryString());
    }

    /**
     * Instantiate the link between a lot type IRI and it's corresponding carpark
     * @param carparkIRI carpark IRI
     * @param lotTypeIRI lot type IRI
     */
    private void instantiateHasLotType(String carparkIRI, String lotTypeIRI) {
        //INSERT DATA { <carparkIRI> ontocarpark:hasLotType <lotTypeIRI> }
        TriplePattern triplePattern = iri(carparkIRI).has(hasLotType, iri(lotTypeIRI));
        InsertDataQuery insert = Queries.INSERT_DATA(triplePattern);
        insert.prefix(PREFIX_ONTOCARPARK);
        kbClient.executeUpdate(insert.getQueryString());
    }

    /**
     * Retrieve and parse carpark rates from JSONObject
     * @param carparkRatesData JSONObject containing carpark rates
     * @return map containing weekday, saturday, sunday and PH rates
     */
    private Map<String, String> parseCarparkRates(JSONObject carparkRatesData) {
        Map<String, String> map = new HashMap<>();
        String saturdayRate = carparkRatesData.getString("saturday_rate");
        String sundayAndPHRates = carparkRatesData.getString("sunday_publicholiday_rate");
        String weekdayRate1 = carparkRatesData.getString("weekdays_rate_1");
        String weekdayRate2 = carparkRatesData.getString("weekdays_rate_2");
        String weekdayRate;

        if (!(weekdayRate2.equals("-")) && !(weekdayRate2.equals(weekdayRate1))) {
            weekdayRate = weekdayRate1 + ";" + weekdayRate2;
        } else {
            weekdayRate = weekdayRate1;
        }

        if (sundayAndPHRates.equals("Same as Saturday")) {
            sundayAndPHRates = saturdayRate;
        }

        if (sundayAndPHRates.equals("Same as wkdays") || sundayAndPHRates.equals("Same as weekdays")) {
            sundayAndPHRates = weekdayRate;
        }

        if (saturdayRate.equals("Same as wkdays") || saturdayRate.equals("Same as weekdays")) {
            saturdayRate = weekdayRate;
        }

        map.put("Weekday rates", weekdayRate);
        map.put("Saturday rates", saturdayRate);
        map.put("Sunday and PH rates", sundayAndPHRates);
        return map;
    }

    /**
     * Check for existing carpark rates attached to carpark and update rates if they are not equivalent
     * @param map map containing weekday, saturday, sunday and PH rates
     * @param carparkIRI carpark IRI
     */
    private void checkAndUpdateRates(Map<String, String> map, String carparkIRI) {

        Variable weekdayRate = SparqlBuilder.var("weekdayRate");
        //SELECT ?weekdayRate WHERE { <carparkIRI> ontocarpark:hasWeekdayRates ?weekdayRate }
        TriplePattern triplePattern = iri(carparkIRI).has(hasWeekdayRates, weekdayRate);
        SelectQuery selectQuery = Queries.SELECT();
        selectQuery.prefix(PREFIX_ONTOCARPARK).select(weekdayRate).where(triplePattern);
        String queriedWeekdayRate;
        InsertDataQuery insertQuery;
        DeleteDataQuery deleteQuery;
        JSONArray queryResult = kbClient.executeQuery(selectQuery.getQueryString());
        if (queryResult.isEmpty()) {
            //INSERT DATA { <carparkIRI> ontocarpark:hasWeekdayRates "Weekday rates" }
            triplePattern = iri(carparkIRI).has(hasWeekdayRates, map.get("Weekday rates"));
            insertQuery = new InsertDataQuery();
            insertQuery = Queries.INSERT_DATA(triplePattern);
            insertQuery.prefix(PREFIX_ONTOCARPARK);
            kbClient.executeUpdate(insertQuery.getQueryString());
        } else {
            //Check and compare weekday rates with those retrieved via API
            queriedWeekdayRate = queryResult.getJSONObject(0).getString("weekdayRate");
            //If not equal, update the weekday rates in the triple store
            if (queriedWeekdayRate != map.get("Weekday rates")) {
                //DELETE DATA { <carparkIRI> ontocarpark:hasWeekdayRates "weekdayRate"}
                triplePattern = iri(carparkIRI).has(hasWeekdayRates, queriedWeekdayRate);
                deleteQuery = Queries.DELETE_DATA(triplePattern);
                deleteQuery.prefix(PREFIX_ONTOCARPARK);
                kbClient.executeUpdate(deleteQuery.getQueryString());
                //INSERT DATA { <carparkIRI> ontocarpark:hasWeekdayRates "Weekday rates" }
                triplePattern = iri(carparkIRI).has(hasWeekdayRates, map.get("Weekday rates"));
                insertQuery = new InsertDataQuery();
                insertQuery = Queries.INSERT_DATA(triplePattern);
                insertQuery.prefix(PREFIX_ONTOCARPARK);
                kbClient.executeUpdate(insertQuery.getQueryString());
            }
        }

        Variable saturdayRate = SparqlBuilder.var("saturdayRate");
        //SELECT ?saturdayRate WHERE { <carparkIRI> ontocarpark:hasSaturdayRates ?saturdayRate }
        triplePattern = iri(carparkIRI).has(hasSaturdayRates, saturdayRate);
        selectQuery.prefix(PREFIX_ONTOCARPARK).select(saturdayRate).where(triplePattern);
        String queriedSaturdayRate;
        queryResult = kbClient.executeQuery(selectQuery.getQueryString());
        if (queryResult.isEmpty()) {
            //INSERT DATA { <carparkIRI> ontocarpark:hasSaturdayRates "Saturday rates" }
            triplePattern = iri(carparkIRI).has(hasSaturdayRates, map.get("Saturday rates"));
            insertQuery = new InsertDataQuery();
            insertQuery = Queries.INSERT_DATA(triplePattern);
            insertQuery.prefix(PREFIX_ONTOCARPARK);
            kbClient.executeUpdate(insertQuery.getQueryString());
        } else {
            //Check and compare saturday rates with those retrieved via API
            queriedSaturdayRate = queryResult.getJSONObject(0).getString("saturdayRate");
            //If not equal, update the saturday rates in the triple store
            if (queriedSaturdayRate != map.get("Saturday rates")) {
                //DELETE DATA { <carparkIRI> ontocarpark:hasSaturdayRates "saturdayRate" }
                triplePattern = iri(carparkIRI).has(hasSaturdayRates, queriedSaturdayRate);
                deleteQuery = Queries.DELETE_DATA(triplePattern);
                deleteQuery.prefix(PREFIX_ONTOCARPARK);
                kbClient.executeUpdate(deleteQuery.getQueryString());
                //INSERT DATA { <carparkIRI> ontocarpark:hasSaturdayRates "Saturday rates" }
                triplePattern = iri(carparkIRI).has(hasSaturdayRates, map.get("Saturday rates"));
                insertQuery = new InsertDataQuery();
                insertQuery = Queries.INSERT_DATA(triplePattern);
                insertQuery.prefix(PREFIX_ONTOCARPARK);
                kbClient.executeUpdate(insertQuery.getQueryString());
            }
        }

        Variable sundayAndPHRate = SparqlBuilder.var("sundayAndPHRate");
        //SELECT ?sundayAndPHRate WHERE { <carparkIRI> ontocarpark:hasSundayAndPHRates ?sundayAndPHRate }
        triplePattern = iri(carparkIRI).has(hasSundayAndPHRates, sundayAndPHRate);
        selectQuery.prefix(PREFIX_ONTOCARPARK).select(sundayAndPHRate).where(triplePattern);
        String queriedSundayAndPHRate;
        queryResult = kbClient.executeQuery(selectQuery.getQueryString());
        if (queryResult.isEmpty()) {
            //INSERT DATA { <carparkIRI> ontocarpark:hasSundayAndPHRates "Sunday and PH rates" }
            triplePattern = iri(carparkIRI).has(hasSundayAndPHRates, map.get("Sunday and PH rates"));
            insertQuery = new InsertDataQuery();
            insertQuery = Queries.INSERT_DATA(triplePattern);
            insertQuery.prefix(PREFIX_ONTOCARPARK);
            kbClient.executeUpdate(insertQuery.getQueryString());
        } else {
            //Check and compare sunday and PH rates with those retrieved via API
            queriedSundayAndPHRate = queryResult.getJSONObject(0).getString("sundayAndPHRate");
            //If not equal, update the sunday and PH rates in the triple store
            if (queriedSundayAndPHRate != map.get("Sunday and PH rates")) {
                //DELETE DATA { <carparkIRI> ontocarpark:hasSundayAndPHRates "sundayAndPHRate" }
                triplePattern = iri(carparkIRI).has(hasSundayAndPHRates, queriedSundayAndPHRate);
                deleteQuery = Queries.DELETE_DATA(triplePattern);
                deleteQuery.prefix(PREFIX_ONTOCARPARK);
                kbClient.executeUpdate(deleteQuery.getQueryString());
                //INSERT DATA { <carparkIRI> ontocarpark:hasSundayAndPHRates "Sunday and PH rates" }
                triplePattern = iri(carparkIRI).has(hasSundayAndPHRates, map.get("Sunday and PH rates"));
                insertQuery = new InsertDataQuery();
                insertQuery = Queries.INSERT_DATA(triplePattern);
                insertQuery.prefix(PREFIX_ONTOCARPARK);
                kbClient.executeUpdate(insertQuery.getQueryString());
            }
        }
    }

    /**
     * Capitalise the first letter of each word
     * @param word word to modify
     * @return modified word as String
     */
    private String capitaliseFirstLetterOfEachWord(String word) {
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
}
