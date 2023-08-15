package uk.ac.cam.cares.jps.agent.osmagent;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.json.JSONArray;
import org.postgis.Point;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.*;
import java.util.concurrent.locks.*;

@WebServlet(urlPatterns = "/update")

public class OSMAgent extends JPSAgent {
    private RemoteStoreClient storeClient;

    public void init() {
        EndpointConfig endpointConfig = new EndpointConfig();
        storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
    }

    protected void doPost(HttpServletRequest request, HttpServletResponse response)  throws ServletException, IOException {

        processRequestQueue(request, response);
    }

    private void processRequestQueue(HttpServletRequest request, HttpServletResponse response) throws IOException {


        

        //Populate ontobuilt column as according to OntoBuilt category

        //Match each row in planet_osm_polygon, planet_osm_point to JingYa's Postgis building IRI
        //Populate building_iri column (PENDING ON GEROGE TO FIX ONTOP on non-postgres-named-database)

        /**
         * For each unique building_iri, which returns multiple rows, 
         * 1) Check for duplications of OntoBuilt type. For each row of the same OntoBuilt type, assign same propetyusage_iri. 
         * 2) Calculate sum of OntoBuilt type, divide by the number of OntoBuilt types for on the same building_iri. 
        */

    }

}