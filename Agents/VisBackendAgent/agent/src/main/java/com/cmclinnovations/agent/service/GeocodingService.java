package com.cmclinnovations.agent.service;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.cmclinnovations.agent.model.GeoLocationType;
import com.cmclinnovations.agent.model.SparqlBinding;
import com.cmclinnovations.agent.utils.ShaclResource;
import com.cmclinnovations.agent.utils.StringResource;

@Service
public class GeocodingService {
  private final FileService fileService;
  private final KGService kgService;

  private static final String ADDRESS_VAR = "address";
  private static final String LOCATION_VAR = "location";
  private static final String BLOCK_VAR = "block";
  private static final String CITY_VAR = "city";
  private static final String COUNTRY_VAR = "country";
  private static final String STREET_VAR = "street";
  private static final Logger LOGGER = LogManager.getLogger(GeocodingService.class);

  /**
   * Constructs a new service with the following dependencies.
   * 
   * @param fileService File service for accessing file resources.
   * @param kgService   KG service for performing the query.
   */
  public GeocodingService(FileService fileService, KGService kgService) {
    this.fileService = fileService;
    this.kgService = kgService;
  }

  /**
   * Retrieve the address based on the postal code.
   * 
   * @param postalCode Postal code identifier.
   */
  public ResponseEntity<?> getAddress(String postalCode) {
    LOGGER.debug("Retrieving geocoding endpoint...");
    // The geocoding endpoint must be added as the value of the "geocode" field
    ResponseEntity<String> geocodingEndpointResponse = this.fileService.getTargetFileName("geocode");
    // Return the BAD REQUEST response directly if there is no associated url
    if (geocodingEndpointResponse.getStatusCode().equals(HttpStatus.BAD_REQUEST)) {
      return geocodingEndpointResponse;
    }
    LOGGER.debug("Generating query template to search for address...");
    String query = this.genSearchQueryTemplate(postalCode);
    LOGGER.debug("Retrieving address for postal code: {} ...", postalCode);
    Queue<SparqlBinding> results = this.kgService.query(query, geocodingEndpointResponse.getBody());
    if (results.isEmpty()) {
      LOGGER.info("No address found!");
      return new ResponseEntity<>(
          "There are no address associated with the parameters in the knowledge graph.",
          HttpStatus.NOT_FOUND);
    } else {
      LOGGER.info("Found address(es) associated with the request!");
      List<Map<String, String>> parsedResults = new ArrayList<>();
      while (!results.isEmpty()) {
        SparqlBinding addressInstance = results.poll();
        Map<String, String> address = new HashMap<>();
        // Block is optional which results in a null
        if (addressInstance.getFieldValue(BLOCK_VAR) != null) {
          address.put(BLOCK_VAR, addressInstance.getFieldValue(BLOCK_VAR));
        }
        address.put(STREET_VAR, addressInstance.getFieldValue(STREET_VAR));
        address.put(CITY_VAR, addressInstance.getFieldValue(CITY_VAR));
        address.put(COUNTRY_VAR, addressInstance.getFieldValue(COUNTRY_VAR));
        parsedResults.add(address);
      }
      return new ResponseEntity<>(
          parsedResults,
          HttpStatus.OK);
    }
  }

  /**
   * Retrieve the coordinates based on the street address or postal code.
   * 
   * @param block      The street block identifier.
   * @param street     The street name.
   * @param city       The city name.
   * @param country    The country IRI based on
   *                   https://www.omg.org/spec/LCC/Countries/ISO3166-1-CountryCodes.
   * @param postalCode Postal code identifier.
   */
  public ResponseEntity<?> getCoordinates(String block, String street, String city, String country, String postalCode) {
    LOGGER.debug("Retrieving geocoding endpoint...");
    // The geocoding endpoint must be added as the value of the "geocode" field
    ResponseEntity<String> geocodingEndpointResponse = this.fileService.getTargetFileName("geocode");
    // Return the BAD REQUEST response directly if there is no associated url
    if (geocodingEndpointResponse.getStatusCode().equals(HttpStatus.BAD_REQUEST)) {
      return geocodingEndpointResponse;
    }
    LOGGER.debug("Generating query template for retrieving coordinates...");
    String query = this.genCoordinateQueryTemplate(block, street, city, country, postalCode);
    LOGGER.debug("Retrieving coordinates for postal code: {} ...", postalCode);
    Queue<SparqlBinding> results = this.kgService.query(query, geocodingEndpointResponse.getBody());
    if (results.isEmpty()) {
      LOGGER.info("No coordinates found...");
      return new ResponseEntity<>(
          "There are no coordinates associated with the parameters in the knowledge graph.",
          HttpStatus.NOT_FOUND);
    } else {
      LOGGER.info("Found geocoordinates associated with the request!");
      // Returns the first geoPoint as the same address may have multiple results
      String geoPoint = results.poll().getFieldValue(LOCATION_VAR);
      return new ResponseEntity<>(
          this.parseCoordinates(geoPoint),
          HttpStatus.OK);
    }
  }

  /**
   * Generates the query template for searching addresses based on the postal
   * code.
   * 
   * @param postalCode Search parameter for postal code.
   */
  private String genSearchQueryTemplate(String postalCode) {
    String selectVars = ShaclResource.VARIABLE_MARK + CITY_VAR +
        " " + ShaclResource.VARIABLE_MARK + COUNTRY_VAR +
        " " + ShaclResource.VARIABLE_MARK + STREET_VAR +
        " " + ShaclResource.VARIABLE_MARK + BLOCK_VAR;
    String queryFilters = this.getPredicate(GeoLocationType.POSTAL_CODE) + " " + StringResource.parseLiteral(postalCode)
        + ";";
    queryFilters += this.getPredicate(GeoLocationType.CITY) + " " + ShaclResource.VARIABLE_MARK + CITY_VAR + ";";
    queryFilters += this.getPredicate(GeoLocationType.COUNTRY) + " " + ShaclResource.VARIABLE_MARK + COUNTRY_VAR + ";";
    queryFilters += this.getPredicate(GeoLocationType.STREET) + " " + ShaclResource.VARIABLE_MARK + STREET_VAR
        + ShaclResource.FULL_STOP;
    // Block numbers are optional
    queryFilters += StringResource.genOptionalClause(ShaclResource.VARIABLE_MARK + ADDRESS_VAR + " " +
        this.getPredicate(GeoLocationType.BLOCK) + " " + ShaclResource.VARIABLE_MARK + BLOCK_VAR
        + ShaclResource.FULL_STOP);
    return this.genQueryTemplate(selectVars, queryFilters);
  }

  /**
   * Generates the query template with the specific location identifiers such as
   * postal code, street names, block, city, and country.
   * 
   * @param block      The street block identifier.
   * @param street     The street name.
   * @param city       The city name.
   * @param country    The country IRI based on
   *                   https://www.omg.org/spec/LCC/Countries/ISO3166-1-CountryCodes.
   * @param postalCode Postal code identifier.
   */
  private String genCoordinateQueryTemplate(String block, String street, String city, String country,
      String postalCode) {
    String queryFilters = "fibo-fnd-arr-id:isIndexTo/geo:asWKT " + ShaclResource.VARIABLE_MARK + LOCATION_VAR + ";";
    if (postalCode != null) {
      queryFilters += this.getPredicate(GeoLocationType.POSTAL_CODE);
      queryFilters += " " + StringResource.parseLiteral(postalCode) + ";";
    }
    if (city != null) {
      queryFilters += this.getPredicate(GeoLocationType.CITY);
      queryFilters += " " + StringResource.parseLiteral(city) + ";";
    }
    if (country != null) {
      queryFilters += this.getPredicate(GeoLocationType.COUNTRY);
      queryFilters += " " + StringResource.parseIriForQuery(country) + ";";
    }

    if (street != null) {
      queryFilters += this.getPredicate(GeoLocationType.STREET);
      queryFilters += " " + StringResource.parseLiteral(street) + ";";
      // Block will only be included if there is a corresponding street
      if (block != null) {
        queryFilters += this.getPredicate(GeoLocationType.BLOCK);
        queryFilters += " " + StringResource.parseLiteral(block) + ";";
      }
    }
    String selectVar = ShaclResource.VARIABLE_MARK + LOCATION_VAR;
    // Limit the query return to one result to improve performance
    return this.genQueryTemplate(selectVar, queryFilters) + "LIMIT 1";
  }

  /**
   * Get predicates based on the geolocation type.
   * 
   * @param geoType The types of geolocation.
   */
  private String getPredicate(GeoLocationType geoType) {
    switch (geoType) {
      case GeoLocationType.POSTAL_CODE:
        return "fibo-fnd-plc-adr:hasPostalCode";
      case GeoLocationType.BLOCK:
        return "fibo-fnd-plc-adr:hasStreetAddress/fibo-fnd-plc-adr:hasPrimaryAddressNumber/fibo-fnd-rel-rel:hasTag";
      case GeoLocationType.STREET:
        return "fibo-fnd-plc-adr:hasStreetAddress/fibo-fnd-plc-adr:hasStreetName/fibo-fnd-rel-rel:hasTag";
      case GeoLocationType.CITY:
        return "fibo-fnd-plc-loc:hasCityName";
      case GeoLocationType.COUNTRY:
        return "fibo-fnd-plc-loc:hasCountry";
      default:
        String errorMessage = MessageFormat.format("Invalid geolocation Type: {}!", geoType);
        LOGGER.error(errorMessage);
        throw new IllegalArgumentException(errorMessage);
    }
  }

  /**
   * Generates the query template with the required variables and where clause
   * lines.
   * 
   * @param selectVariables  The variables in the SELECT clause.
   * @param whereClauseLines The query lines for the WHERE clause.
   */
  private String genQueryTemplate(String selectVariables, String whereClauseLines) {
    return "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> " +
        "PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#> " +
        "PREFIX fibo-fnd-arr-id:<https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/IdentifiersAndIndices/> " +
        "PREFIX fibo-fnd-plc-adr:<https://spec.edmcouncil.org/fibo/ontology/FND/Places/Addresses/> " +
        "PREFIX fibo-fnd-plc-loc:<https://spec.edmcouncil.org/fibo/ontology/FND/Places/Locations/> " +
        "PREFIX fibo-fnd-rel-rel:<https://spec.edmcouncil.org/fibo/ontology/FND/Relations/Relations/> " +
        "PREFIX geo:<http://www.opengis.net/ont/geosparql#> " +
        "PREFIX xsd:<http://www.w3.org/2001/XMLSchema#> " +
        "SELECT DISTINCT " + selectVariables + " WHERE {" +
        ShaclResource.VARIABLE_MARK + ADDRESS_VAR + " a fibo-fnd-plc-adr:ConventionalStreetAddress;" +
        whereClauseLines +
        "}";
  }

  /**
   * Parses the coordinates into longitude and latitude.
   * 
   * @param geoPoint The coordinates stored as a geoPoint.
   */
  private double[] parseCoordinates(String geoPoint) {
    // REGEX for `POINT(Longitude Latitude)` format
    Pattern pattern = Pattern.compile("POINT\\((\\d+\\.\\d+) (\\d+\\.\\d+)\\)");
    Matcher matcher = pattern.matcher(geoPoint);

    if (matcher.matches()) {
      double longitude = Double.parseDouble(matcher.group(1));
      double latitude = Double.parseDouble(matcher.group(2));
      return new double[] { longitude, latitude };
    }
    LOGGER.warn("Unable to parse geoPoint into valid coordinates...");
    return new double[] {}; // Returns empty array if no result is found
  }
}