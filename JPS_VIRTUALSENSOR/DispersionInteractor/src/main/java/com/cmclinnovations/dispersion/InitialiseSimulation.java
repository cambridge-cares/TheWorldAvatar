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
import org.apache.http.entity.ContentType;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONTokener;
import org.postgis.Polygon;
import org.springframework.core.io.ClassPathResource;

import com.cmclinnovations.stack.clients.ontop.OntopClient;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * a separate mapping is required for each SRID, currently only supports 4326
 */
@WebServlet(urlPatterns = { "/InitialiseSimulation" })
public class InitialiseSimulation extends HttpServlet {
    private static final Random RAND = new Random();
    private static final Logger LOGGER = LogManager.getLogger(InitialiseSimulation.class);
    private QueryClient queryClient;
    private DispersionPostGISClient dispersionPostGISClient;

    @Override
    public void init() throws ServletException {
        EndpointConfig endpointConfig = Config.ENDPOINT_CONFIG;
        dispersionPostGISClient = new DispersionPostGISClient(endpointConfig.getDburl(), endpointConfig.getDbuser(),
                endpointConfig.getDbpassword());
        RemoteStoreClient storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        RemoteRDBStoreClient remoteRDBStoreClient = new RemoteRDBStoreClient(endpointConfig.getDburl(),
                endpointConfig.getDbuser(), endpointConfig.getDbpassword());
        TimeSeriesClient<Long> tsClient = new TimeSeriesClient<>(storeClient, Long.class);
        queryClient = new QueryClient(storeClient, remoteRDBStoreClient, tsClient);
    }

    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        // EWKT literal for the scope to create
        String ewkt = req.getParameter("ewkt");
        int nx = Integer.parseInt(req.getParameter("nx"));
        int ny = Integer.parseInt(req.getParameter("ny"));
        String citiesNamespace = req.getParameter("citiesnamespace");
        String scopeLabel = req.getParameter("label");
        String[] zArray = req.getParameterValues("z");
        String simulationTimeIri = req.getParameter("simulationTimeIri"); // optional

        List<Integer> zList = new ArrayList<>();
        if (zArray == null) {
            zList.add(0);
        } else {
            for (int i = 0; i < zArray.length; i++) {
                int zInt = Integer.parseInt(zArray[i]);
                if (zList.contains(zInt)) {
                    LOGGER.warn("Duplicate value given for z = {}, will be ignored", zInt);
                } else {
                    zList.add(zInt);
                }
            }
        }

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
                    OntopClient ontopClient = OntopClient.getInstance();
                    ontopClient.updateOBDA(obdaFile);

                    // adds OntoAgent instance
                    queryClient.initialiseAgent();
                }

                if (polygonProvided.getSrid() != 4326) {
                    polygon4326 = dispersionPostGISClient.getPolygonAs4326(polygonProvided, conn);
                } else {
                    polygon4326 = polygonProvided;
                }

                // returns null if there are no matches
                scopeIri = dispersionPostGISClient.getScopeIri(polygon4326, conn);
                String derivation = null;

                if (scopeIri == null) {
                    scopeIri = dispersionPostGISClient.addScope(polygon4326, conn);

                    String weatherStation = createVirtualWeatherStation(polygon4326);

                    derivation = queryClient.initialiseScopeDerivation(scopeIri, scopeLabel, weatherStation, nx,
                            ny, citiesNamespace, zList, simulationTimeIri);
                } else {
                    derivation = queryClient.getDerivationWithScope(scopeIri);
                }

                resp.getWriter().print(new JSONObject().put("derivation", derivation));
                resp.setContentType(ContentType.APPLICATION_JSON.getMimeType());
                resp.setCharacterEncoding("UTF-8");

            } catch (SQLException e) {
                LOGGER.error("SQL state {}", e.getSQLState());
                LOGGER.error(e.getMessage());
                LOGGER.error("Probably failed to close SQL connection or failed to connect");
            } catch (IOException e) {
                LOGGER.error(e.getMessage());
                LOGGER.error("Probably failed to add ontop mapping");
            } catch (JSONException e) {
                LOGGER.error(e.getMessage());
            }
        }

    }

    /**
     * creates a weather station at a random location within the polyon
     * 
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

        double lat = ((upperY - lowerY) * RAND.nextDouble()) + lowerY;
        double lon = ((upperX - lowerX) * RAND.nextDouble()) + lowerX;

        HttpPut httpPut;
        // station IRI
        String station = null;
        try {
            URIBuilder builder = new URIBuilder(Config.WEATHER_AGENT_URL);
            builder.addParameter("lon", String.valueOf(lon));
            builder.addParameter("lat", String.valueOf(lat));
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