package com.cmclinnovations.dispersion;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.http.client.methods.HttpPut;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import org.json.JSONTokener;
import org.postgis.Polygon;
import org.springframework.core.io.ClassPathResource;

import com.cmclinnovations.stack.clients.ontop.OntopClient;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.util.CRSTransformer;

/**
 * a separate mapping is required for each SRID, currently only supports 32630
 */
@WebServlet(urlPatterns = {"/InitialiseSimulation"})
public class InitialiseSimulation extends HttpServlet {
    private static final Random RAND = new Random();
    private static final Logger LOGGER = LogManager.getLogger(InitialiseSimulation.class);

    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        // EWKT literal for the scope to create
        String ewkt = req.getParameter("ewkt");

        Polygon polygon = null;
        try {
            polygon = new Polygon(ewkt);
        } catch (SQLException e) {
            LOGGER.error("Failed to parse given EWKT literal", e);
        }

        if (polygon != null) {
            EndpointConfig endpointConfig = Config.ENDPOINT_CONFIG;
            DispersionPostGISClient dispersionPostGISClient = new DispersionPostGISClient(endpointConfig.getDburl(), endpointConfig.getDbuser(), endpointConfig.getDbpassword());
            RemoteStoreClient storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
            QueryClient queryClient = new QueryClient(storeClient);

            String scopeIri = null;
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
    
                if (!dispersionPostGISClient.scopeExists(polygon, conn)) {
                    scopeIri = dispersionPostGISClient.addScope(polygon, conn);
                } else {
                    String responseString = "Given EWKT literal already exists in the database, or the scopeExists query failed, check logs";
                    resp.getWriter().write(String.format("Created scope <%s>", scopeIri));
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

            if (scopeIri != null) {
                String weatherStation1 = createVirtualWeatherStation(polygon);
                String weatherStation2 = createVirtualWeatherStation(polygon);

                List<String> weatherStations = new ArrayList<>();
                weatherStations.add(weatherStation1);
                weatherStations.add(weatherStation2);

                queryClient.initialiseScopeDerivation(scopeIri, weatherStations);
                try {
                    resp.getWriter().write(String.format("Created scope <%s>", scopeIri));
                } catch (IOException e) {
                    LOGGER.error(e.getMessage());
                    LOGGER.error("Failed to write HTTP response");
                }
            }
        }
    }

    /**
     * creates a weather station at a random location within the polyon
     * @param polygon
     * @return
     */
    String createVirtualWeatherStation(Polygon polygon) {
        // generating the random point
        double lowerX; 
        double upperX; 
        double lowerY;
        double upperY;
        if (polygon.getPoint(0).getX() > polygon.getPoint(2).getX()) {
            lowerX = polygon.getPoint(2).getX();
            upperX = polygon.getPoint(0).getX();
        } else {
            lowerX = polygon.getPoint(0).getX();
            upperX = polygon.getPoint(2).getX();
        }

        if (polygon.getPoint(0).getY() > polygon.getPoint(2).getY()) {
            lowerY = polygon.getPoint(2).getY();
            upperY = polygon.getPoint(0).getY();
        } else {
            lowerY = polygon.getPoint(0).getY();
            upperY = polygon.getPoint(2).getY();
        }

        double newY = ((upperY - lowerY) * RAND.nextDouble()) + lowerY;
        double newX = ((upperX - lowerX) * RAND.nextDouble()) + lowerX;

        double[] transformedXY = CRSTransformer.transform("EPSG:" + polygon.getSrid(), "EPSG:4326", newX, newY);
        double lon = transformedXY[0];
        double lat = transformedXY[1];

        HttpPut httpPut;
        // station IRI
        String station = null;
        try {
            URIBuilder builder = new URIBuilder(Config.WEATHER_AGENT_URL);
            builder.addParameter("lon",String.valueOf(lon));
            builder.addParameter("lat",String.valueOf(lat));
            httpPut = new HttpPut(builder.build());

            try (CloseableHttpClient httpClient = HttpClients.createDefault()) {
                JSONTokener tokener = new JSONTokener(httpClient.execute(httpPut).getEntity().getContent());
                JSONObject json = new JSONObject(tokener);
                station = json.getString("station");
            }
        } catch (URISyntaxException e) {
            LOGGER.error("Failed to build URI for weather agent post request");
            LOGGER.error(e.getMessage());
        } catch (IOException e) {
            LOGGER.error("Http PUT request failed for weather agent");
            LOGGER.error(e.getMessage());
        }

        return station;
    }
}