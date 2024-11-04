package com.cmclinnovations.agent.service;

import java.text.MessageFormat;
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

@Service
public class GeocodingService {
  private final FileService fileService;
  private final KGService kgService;

  private static final String LOCATION_VAR = "location";
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
    String query = this.genQueryTemplate(block, street, city, country, postalCode);
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
  private String genQueryTemplate(String block, String street, String city, String country, String postalCode) {
    String queryFilters = "";
    if (postalCode != null) {
      queryFilters += this.genQueryFilter(postalCode, GeoLocationType.POSTAL_CODE);
    }
    if (city != null) {
      queryFilters += this.genQueryFilter(city, GeoLocationType.CITY);
    }
    if (country != null) {
      queryFilters += this.genQueryFilter(country, GeoLocationType.COUNTRY);
    }

    if (street != null) {
      queryFilters += this.genQueryFilter(street, GeoLocationType.STREET);
      // Block will only be included if there is a corresponding street
      if (block != null) {
        queryFilters += this.genQueryFilter(block, GeoLocationType.BLOCK);
      }
    }
    return "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> " +
        "PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#> " +
        "PREFIX fibo-fnd-arr-id:<https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/IdentifiersAndIndices/> " +
        "PREFIX fibo-fnd-plc-adr:<https://spec.edmcouncil.org/fibo/ontology/FND/Places/Addresses/> " +
        "PREFIX fibo-fnd-plc-loc:<https://spec.edmcouncil.org/fibo/ontology/FND/Places/Locations/> " +
        "PREFIX fibo-fnd-rel-rel:<https://spec.edmcouncil.org/fibo/ontology/FND/Relations/Relations/> " +
        "PREFIX geo:<http://www.opengis.net/ont/geosparql#> " +
        "PREFIX xsd:<http://www.w3.org/2001/XMLSchema#> " +
        "SELECT ?" + LOCATION_VAR + " WHERE {" +
        "?address a fibo-fnd-plc-adr:ConventionalStreetAddress;" +
        "fibo-fnd-arr-id:isIndexTo/geo:asWKT ?" + LOCATION_VAR + ";" +
        queryFilters +
        "}";
  }

  /**
   * Generates a query filter line depending on the geolocation type.
   * 
   * @param queryVal The value of the query line, which may either be an IRI or
   *                 literal.
   * @param geoType  The types of geolocation.
   */
  private String genQueryFilter(String queryVal, GeoLocationType geoType) {
    switch (geoType) {
      case GeoLocationType.POSTAL_CODE:
        return "fibo-fnd-plc-adr:hasPostalCode \"" + queryVal + "\";";
      case GeoLocationType.BLOCK:
        return " fibo-fnd-plc-adr:hasStreetAddress/fibo-fnd-plc-adr:hasPrimaryAddressNumber/fibo-fnd-rel-rel:hasTag \""
            + queryVal + "\";";
      case GeoLocationType.STREET:
        return " fibo-fnd-plc-adr:hasStreetAddress/fibo-fnd-plc-adr:hasStreetName/fibo-fnd-rel-rel:hasTag \""
            + queryVal
            + "\";";
      case GeoLocationType.CITY:
        return "fibo-fnd-plc-loc:hasCityName \"" + queryVal + "\";";
      case GeoLocationType.COUNTRY:
        return "fibo-fnd-plc-loc:hasCountry  <" + queryVal + ">;";
      default:
        String errorMessage = MessageFormat.format("Invalid geolocation Type: {}!", geoType);
        LOGGER.error(errorMessage);
        throw new IllegalArgumentException(errorMessage);
    }
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