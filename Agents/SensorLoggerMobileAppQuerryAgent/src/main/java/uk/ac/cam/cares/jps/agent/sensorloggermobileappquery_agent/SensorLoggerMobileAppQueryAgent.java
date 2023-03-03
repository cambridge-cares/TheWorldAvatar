package uk.ac.cam.cares.jps.agent.sensorloggermobileappquery_agent;


import com.cmclinnovations.stack.clients.postgis.PostGISClient;
import com.cmclinnovations.swagger.podman.model.Config;
import org.apache.hc.core5.http.ContentType;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.jooq.impl.DSL;
import org.json.JSONException;
import org.json.JSONObject;
import org.postgis.Polygon;
import org.springframework.core.io.ClassPathResource;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

import org.jooq.DSLContext;
import org.jooq.InsertValuesStepN;
import org.jooq.SQLDialect;
import org.jooq.Table;
import org.jooq.impl.DSL;
import org.postgis.Polygon;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;


import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Path;
import java.sql.SQLException;
import java.time.Instant;


@WebServlet("/geo")
public class SensorLoggerMobileAppQueryAgent extends JPSAgent {

    private static final Logger LOGGER = LogManager.getLogger(SensorLoggerMobileAppQueryAgent.class);

    protected void doPost(HttpServletRequest req, HttpServletResponse response)
            throws ServletException, IOException {

        response.setStatus(HttpServletResponse.SC_OK);
        PrintWriter out = response.getWriter();
        out.println("HTTP POST request processed.");


        // EWKT literal for the scope to create
        String ewkt = req.getParameter("ewkt");
        int nx = Integer.parseInt(req.getParameter("nx"));
        int ny = Integer.parseInt(req.getParameter("ny"));

        Polygon polygonProvided = null;
        try {
            polygonProvided = new Polygon(ewkt);
        } catch (SQLException e) {
            LOGGER.error("Failed to parse given EWKT literal", e);
        }



        if (polygonProvided != null) {
            String scopeIri = null;
            Polygon polygon4326 = null;
            try (Connection conn = dispersionPostGISClient.getConnection()) {
                if (!dispersionPostGISClient.tableExists(Config.SCOPE_TABLE_NAME, conn)) {
                    // first time initialisation
                    dispersionPostGISClient.createTable(Config.SCOPE_TABLE_NAME, conn);

                    // add ontop mapping
                    Path obdaFile = new ClassPathResource("ontop.obda").getFile().toPath();
                    new OntopClient().updateOBDA(obdaFile);

                    // adds OntoAgent instance
                    queryClient.initialiseAgent();
                }

                if (polygonProvided.getSrid() != 4326) {
                    polygon4326 = dispersionPostGISClient.getPolygonAs4326(polygonProvided, conn);
                } else {
                    polygon4326 = polygonProvided;
                }

                if (!dispersionPostGISClient.scopeExists(polygon4326, conn)) {
                    scopeIri = dispersionPostGISClient.addScope(polygon4326, conn);
                } else {
                    String responseString = "Given EWKT literal already exists in the database, or the scopeExists query failed, check logs";
                    response.getWriter().write(String.format("Created scope <%s>", scopeIri));
                    LOGGER.warn(responseString);
                }
            } catch (SQLException e) {
                LOGGER.error("SQL state {}", e.getSQLState());
                LOGGER.error(e.getMessage());
                LOGGER.error("Probably failed to close SQL connection or failed to connect");
            } catch (IOException e) {
                LOGGER.error(e.getMessage());
                LOGGER.error("Probably failed to add ontop mapping");
            }

            if (scopeIri != null && polygon4326 != null) {
                String weatherStation = createVirtualWeatherStation(polygon4326);

                String derivation = queryClient.initialiseScopeDerivation(scopeIri, weatherStation, nx, ny);
                try {
                    response.getWriter().print(new JSONObject().put("derivation", derivation));
                    response.setContentType(ContentType.APPLICATION_JSON.getMimeType());
                    response.setCharacterEncoding("UTF-8");
                } catch (IOException e) {
                    LOGGER.error(e.getMessage());
                    LOGGER.error("Failed to write HTTP response");
                } catch (JSONException e) {
                    LOGGER.error(e.getMessage());
                    LOGGER.error("Failed to create JSON object for HTTP response");
                }
            }
        }
    }

    private RemoteStoreClient storeClient;
    private RemoteStoreClient ontopStoreClient;
    private RemoteRDBStoreClient rdbStoreClient;
    private TimeSeriesClient<Long> tsClientLong;
    private TimeSeriesClient<Instant> tsClientInstant;

    public SensorLoggerMobileAppQueryAgent(RemoteStoreClient storeClient, RemoteStoreClient ontopStoreClient, RemoteRDBStoreClient rdbStoreClient) {
        this.storeClient = storeClient;
        this.ontopStoreClient = ontopStoreClient;
        this.tsClientLong = new TimeSeriesClient<>(storeClient, Long.class);
        this.tsClientInstant = new TimeSeriesClient<>(storeClient, Instant.class);
        this.rdbStoreClient = rdbStoreClient;
    }



}









//    List<Ship> getShipsWithinTimeAndScopeViaTsClient(long simulationTime, Geometry scope) {
//        long simTimeUpperBound = simulationTime + 1800; // +30 minutes
//        long simTimeLowerBound = simulationTime - 1800; // -30 minutes
//
//        Map<String,String> measureToShipMap = getMeasureToShipMap();
//        List<String> measures = new ArrayList<>(measureToShipMap.keySet());
//
//        List<Ship> ships = new ArrayList<>();
//        try (Connection conn = rdbStoreClient.getConnection()) {
//            measures.stream().forEach(measure -> {
//                TimeSeries<Long> ts = tsClientLong.getTimeSeriesWithinBounds(List.of(measure), simTimeLowerBound, simTimeUpperBound, conn);
//                if (ts.getValuesAsPoint(measure).size() > 1) {
//                    LOGGER.warn("More than 1 point within this time inverval");
//                } else if (ts.getValuesAsPoint(measure).isEmpty()) {
//                    return;
//                }
//
//                try {
//                    // this is to convert from org.postgis.Point to the Geometry class
//                    Point postgisPoint = ts.getValuesAsPoint(measure).get(0);
//                    String wktLiteral = postgisPoint.getTypeString() + postgisPoint.getValue();
//
//                    Geometry point = new org.locationtech.jts.io.WKTReader().read(wktLiteral);
//
//                    if (scope.covers(point)) {
//                        // measureToShipMap.get(measure) gives the iri
//                        Ship ship = new Ship(measureToShipMap.get(measure));
//                        ship.setLocation(postgisPoint);
//                        ships.add(ship);
//                    }
//                } catch (ParseException e) {
//                    LOGGER.error("Failed to parse WKT literal of point");
//                    LOGGER.error(e.getMessage());
//                    return;
//                }
//
//            });
//        } catch (SQLException e) {
//            LOGGER.error("Probably failed at closing connection");
//            LOGGER.error(e.getMessage());
//        }
//
//        return ships;
//    }
