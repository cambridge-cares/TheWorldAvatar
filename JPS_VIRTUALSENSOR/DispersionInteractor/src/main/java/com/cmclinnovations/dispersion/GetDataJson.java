package com.cmclinnovations.dispersion;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.SQLException;
import java.time.Instant;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.IOUtils;
import org.apache.http.entity.ContentType;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.locationtech.jts.geom.Point;
import org.locationtech.jts.io.ParseException;
import org.locationtech.jts.io.WKTReader;
import org.apache.logging.log4j.LogManager;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

@WebServlet(urlPatterns = { "/GetDataJson" })
public class GetDataJson extends HttpServlet {
    private static final Logger LOGGER = LogManager.getLogger(GetDataJson.class);
    QueryClient queryClient;
    DispersionPostGISClient dispersionPostGISClient;

    @Override
    protected void doGet(HttpServletRequest req, HttpServletResponse resp) {
        String pollutant = req.getParameter("pollutant");
        String timestep = req.getParameter("timestep");
        String scopeLabel = req.getParameter("scopeLabel");
        String derivationIri = req.getParameter("derivationIri");
        String pollutantLabel = req.getParameter("pollutantLabel");
        JSONObject weatherStation = new JSONObject(req.getParameter("weatherStation"));
        int z = Integer.parseInt(req.getParameter("z"));
        boolean pirmasensVis = Boolean.parseBoolean(req.getParameter("pirmasensVis"));

        JSONObject dataJson = null;
        try (Connection conn = dispersionPostGISClient.getConnection()) {
            List<Boolean> layers = queryClient.getLayers(Instant.parse(timestep).getEpochSecond(), derivationIri, conn);

            dataJson = createDataJson(pollutantLabel, scopeLabel, weatherStation, layers, conn, pollutant,
                    derivationIri, Instant.parse(timestep).getEpochSecond(), z, pirmasensVis);
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }

        try {
            resp.getWriter().print(dataJson);
            resp.setContentType(ContentType.APPLICATION_JSON.getMimeType());
            resp.setCharacterEncoding("UTF-8");
        } catch (IOException e) {
            LOGGER.error(e.getMessage());
            LOGGER.error("Failed to write HTTP response");
        } catch (JSONException e) {
            LOGGER.error(e.getMessage());
            LOGGER.error("Failed to create JSON object for HTTP response");
        }
    }

    @Override
    public void init() throws ServletException {
        EndpointConfig endpointConfig = Config.ENDPOINT_CONFIG;
        RemoteStoreClient storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        RemoteRDBStoreClient remoteRDBStoreClient = new RemoteRDBStoreClient(endpointConfig.getDburl(),
                endpointConfig.getDbuser(), endpointConfig.getDbpassword());
        TimeSeriesClient<Long> tsClient = new TimeSeriesClient<>(storeClient, Long.class);
        TimeSeriesClient<Instant> tsClientInstant = new TimeSeriesClient<>(storeClient,
                Instant.class);
        queryClient = new QueryClient(storeClient, remoteRDBStoreClient, tsClient, tsClientInstant);
        dispersionPostGISClient = new DispersionPostGISClient(endpointConfig.getDburl(), endpointConfig.getDbuser(),
                endpointConfig.getDbpassword());
    }

    /**
     * Index of hasLayers: 0) ships, 1) buildings, 2) static point
     */
    private JSONObject createDataJson(String pollutantLabel, String scopeLabel, JSONObject weatherStation,
            List<Boolean> hasLayers, Connection conn, String pollutant, String derivationIri, long timestep, int z,
            boolean pirmasensVis) {
        JSONObject hideVisility = new JSONObject();
        hideVisility.put("visibility", "none");

        JSONObject visibility = new JSONObject();
        visibility.put("visibility", "visible");

        String dispWms = Config.STACK_URL + "/geoserver/" + Config.GEOSERVER_WORKSPACE +
                "/wms?service=WMS&version=1.1.0&request=GetMap&width=256&height=256&srs=EPSG:3857&format=application/vnd.mapbox-vector-tile&transparent=true"
                + "&bbox={bbox-epsg-3857}"
                + String.format("&layers=%s:%s", Config.GEOSERVER_WORKSPACE, Config.DISPERSION_CONTOURS_TABLE)
                + String.format("&CQL_FILTER=pollutant='%s' AND derivation='%s' AND time=%d AND z=%d", pollutant,
                        derivationIri, timestep, z);
        JSONObject group = new JSONObject();
        JSONArray groups = new JSONArray();
        groups.put(group);
        group.put("name", "Dispersion");
        group.put("stack", Config.STACK_URL);

        JSONArray sources = new JSONArray();
        JSONArray layers = new JSONArray();

        JSONObject dispersionSource = new JSONObject();
        dispersionSource.put("id", "dispersion-source");
        dispersionSource.put("type", "vector");
        dispersionSource.put("tiles", new JSONArray().put(dispWms));
        sources.put(dispersionSource);

        JSONObject weatherStationSource = new JSONObject();
        weatherStationSource.put("id", "weather-source");
        weatherStationSource.put("type", "geojson");
        weatherStationSource.put("data", createWeatherStationGeoJSON(weatherStation));
        sources.put(weatherStationSource);

        group.put("sources", sources);
        JSONObject dispersionLayer = new JSONObject();
        dispersionLayer.put("id", "dispersion-layer");
        dispersionLayer.put("type", "fill");
        dispersionLayer.put("name", pollutantLabel);
        dispersionLayer.put("source", "dispersion-source");
        dispersionLayer.put("source-layer", Config.DISPERSION_CONTOURS_TABLE);
        dispersionLayer.put("minzoom", 4);
        dispersionLayer.put("layout", visibility);

        JSONObject dispersionPaint = new JSONObject();
        dispersionPaint.put("fill-color", new JSONArray().put("get").put("fill"));
        dispersionPaint.put("fill-opacity", 0.3);
        dispersionPaint.put("fill-outline-color", new JSONArray().put("get").put("stroke"));
        dispersionLayer.put("paint", dispersionPaint);

        JSONObject weatherLayer = new JSONObject();
        weatherLayer.put("id", "weather-layer");
        weatherLayer.put("type", "circle");
        weatherLayer.put("name", "Weather station");
        weatherLayer.put("source", "weather-source");
        weatherLayer.put("layout", hideVisility);

        JSONObject weatherPaint = new JSONObject();
        weatherPaint.put("circle-color", "blue");
        weatherLayer.put("paint", weatherPaint);

        layers.put(dispersionLayer).put(weatherLayer);

        group.put("layers", layers);

        // optional layers
        if (Boolean.TRUE.equals(hasLayers.get(0))) {
            long timeBuffer = 1800; // 30 minutes
            String shipWms = Config.STACK_URL + "/geoserver/" + Config.GEOSERVER_WORKSPACE +
                    "/wms?service=WMS&version=1.1.0&request=GetMap&width=256&height=256&srs=EPSG:3857&format=application/vnd.mapbox-vector-tile"
                    + "&bbox={bbox-epsg-3857}"
                    + String.format("&layers=%s:%s", Config.GEOSERVER_WORKSPACE, Config.SHIPS_LAYER_NAME)
                    + String.format("&CQL_FILTER=time<%d AND time>%d", timestep + timeBuffer, timestep - timeBuffer);
            JSONObject shipSource = new JSONObject();
            shipSource.put("id", "ship-source");
            shipSource.put("type", "vector");
            shipSource.put("tiles", new JSONArray().put(shipWms));
            sources.put(shipSource);

            JSONObject shipLayer = new JSONObject();
            shipLayer.put("id", "ships-layer");
            shipLayer.put("type", "circle");
            shipLayer.put("name", "Ships");
            shipLayer.put("source", "ship-source");
            shipLayer.put("source-layer", Config.SHIPS_LAYER_NAME);
            shipLayer.put("minzoom", 4);
            shipLayer.put("layout", visibility);

            JSONObject shipPaint = new JSONObject();
            shipPaint.put("circle-color", "black");
            shipLayer.put("paint", shipPaint);

            layers.put(shipLayer);
        }

        if (pirmasensVis) {
            // extract buildings group from pirmasensData.json template
            try (InputStream is = getClass().getClassLoader().getResourceAsStream("pirmasensData.json")) {
                String templateContent = IOUtils.toString(is, StandardCharsets.UTF_8);
                templateContent = templateContent.replace("http://localhost:3838", Config.STACK_URL);

                JSONObject buildingjson = new JSONObject(templateContent);
                JSONArray buildingGroups = buildingjson.getJSONArray("groups");

                for (int i = 0; i < buildingGroups.length(); i++) {
                    groups.put(buildingGroups.getJSONObject(i));
                }
            } catch (IOException e) {
                LOGGER.error(e.getMessage());
            }
        }

        if (Boolean.TRUE.equals(hasLayers.get(1))) {
            String buildingWms = Config.STACK_URL + "/geoserver/" + Config.GEOSERVER_WORKSPACE +
                    "/wms?service=WMS&version=1.1.0&request=GetMap&width=256&height=256&srs=EPSG:3857&format=application/vnd.mapbox-vector-tile&transparent=true"
                    +
                    "&bbox={bbox-epsg-3857}"
                    + String.format("&layers=%s:%s", "the_world_avatar", "building-pirmasens_gsl");
            // + String.format("&CQL_FILTER=derivation='%s' AND time=%d",
            // derivationIri, timestep);
            JSONObject buildingSource = new JSONObject();
            buildingSource.put("id", "building-source");
            buildingSource.put("type", "vector");
            buildingSource.put("tiles", new JSONArray().put(buildingWms));
            sources.put(buildingSource);

            JSONObject buildingLayer = new JSONObject();
            buildingLayer.put("id", "building-layer");
            buildingLayer.put("type", "fill-extrusion");
            buildingLayer.put("name", "Buildings");
            buildingLayer.put("source", "building-source");
            buildingLayer.put("source-layer", "building-pirmasens_gsl");
            buildingLayer.put("minzoom", 4);
            if (pirmasensVis) {
                buildingLayer.put("layout", hideVisility);
            } else {
                buildingLayer.put("layout", visibility);
            }

            JSONObject buildingLayerPaint = new JSONObject();
            buildingLayerPaint.put("fill-extrusion-base", 0);
            buildingLayerPaint.put("fill-extrusion-color", "#666666");
            buildingLayerPaint.put("fill-extrusion-height", new JSONArray().put("get").put("building_height"));
            buildingLayer.put("paint", buildingLayerPaint);

            layers.put(buildingLayer);
        }

        if (Boolean.TRUE.equals(hasLayers.get(2))) {
            String staticSourceWMS = Config.STACK_URL + "/geoserver/" + Config.GEOSERVER_WORKSPACE +
                    "/wms?service=WMS&version=1.1.0&request=GetMap&width=256&height=256&srs=EPSG:3857&format=application/vnd.mapbox-vector-tile"
                    + "&bbox={bbox-epsg-3857}"
                    + String.format("&layers=%s:%s", Config.GEOSERVER_WORKSPACE, Config.STATIC_SOURCE_TABLE)
                    + String.format("&CQL_FILTER=derivation='%s' AND time=%d", derivationIri, timestep);
            JSONObject staticSource = new JSONObject();
            staticSource.put("id", "static-source");
            staticSource.put("type", "vector");
            staticSource.put("tiles", new JSONArray().put(staticSourceWMS));
            sources.put(staticSource);

            JSONObject staticLayer = new JSONObject();
            staticLayer.put("id", "static-layer");
            staticLayer.put("type", "circle");
            staticLayer.put("name", "Static emission source");
            staticLayer.put("source", "static-source");
            staticLayer.put("source-layer", Config.STATIC_SOURCE_TABLE);
            staticLayer.put("minzoom", 4);
            staticLayer.put("layout", visibility);

            JSONObject staticPaint = new JSONObject();
            staticPaint.put("circle-color", "black");
            staticLayer.put("paint", staticPaint);

            layers.put(staticLayer);
        }

        // elevation
        if (Boolean.TRUE.equals(hasLayers.get(3))) {
            String elevationWms = Config.STACK_URL + "/geoserver/" + Config.GEOSERVER_WORKSPACE +
                    "/wms?service=WMS&version=1.1.0&request=GetMap&width=256&height=256&srs=EPSG:3857&format=application/vnd.mapbox-vector-tile"
                    + "&bbox={bbox-epsg-3857}"
                    + String.format("&layers=%s:%s", Config.GEOSERVER_WORKSPACE, Config.ELEVATION_CONTOURS_TABLE)
                    + String.format("&CQL_FILTER=derivation='%s'", derivationIri);
            JSONObject elevationSource = new JSONObject();
            elevationSource.put("id", "elevation-source");
            elevationSource.put("type", "vector");
            elevationSource.put("tiles", new JSONArray().put(elevationWms));
            sources.put(elevationSource);

            JSONObject elevationLayer = new JSONObject();
            elevationLayer.put("id", "elevation-layer");
            elevationLayer.put("type", "fill");
            elevationLayer.put("name", "Elevation");
            elevationLayer.put("source", "elevation-source");
            elevationLayer.put("source-layer", Config.ELEVATION_CONTOURS_TABLE);
            elevationLayer.put("minzoom", 4);
            elevationLayer.put("layout", hideVisility);

            JSONObject elevationPaint = new JSONObject();
            elevationPaint.put("fill-color", new JSONArray().put("get").put("fill"));
            elevationPaint.put("fill-opacity", 0.3);
            elevationPaint.put("fill-outline-color", new JSONArray().put("get").put("stroke"));
            elevationLayer.put("paint", elevationPaint);

            layers.put(elevationLayer);
        }

        if (dispersionPostGISClient.tableExists(Config.SENSORS_TABLE_NAME, conn)) {
            String sensorWms = Config.STACK_URL + "/geoserver/" + Config.GEOSERVER_WORKSPACE +
                    "/wms?service=WMS&version=1.1.0&request=GetMap&width=256&height=256&srs=EPSG:3857&format=application/vnd.mapbox-vector-tile"
                    + "&bbox={bbox-epsg-3857}"
                    + String.format("&layers=%s:%s", Config.GEOSERVER_WORKSPACE, Config.SENSORS_TABLE_NAME);

            JSONObject sensorSource = new JSONObject();
            sensorSource.put("id", "sensor-source");
            sensorSource.put("type", "vector");
            sensorSource.put("tiles", new JSONArray().put(sensorWms));
            sources.put(sensorSource);

            JSONObject sensorLayer = new JSONObject();
            sensorLayer.put("id", "sensor-layer");
            sensorLayer.put("type", "circle");
            sensorLayer.put("name", "Virtual sensors");
            sensorLayer.put("source", "sensor-source");
            sensorLayer.put("source-layer", Config.SENSORS_TABLE_NAME);
            sensorLayer.put("minzoom", 4);
            sensorLayer.put("layout", visibility);

            JSONObject sensorPaint = new JSONObject();
            sensorPaint.put("circle-color", "red");
            sensorLayer.put("paint", sensorPaint);

            layers.put(sensorLayer);
        }

        JSONObject data = new JSONObject();
        data.put("name", "All Data");
        data.put("groups", groups);

        return data;
    }

    JSONObject createWeatherStationGeoJSON(JSONObject weatherStation) {
        WKTReader reader = new WKTReader();
        Point point;

        try {
            point = (Point) reader.read(weatherStation.getString("wkt"));
        } catch (ParseException e) {
            LOGGER.error(e.getMessage());
            throw new RuntimeException(e);
        }

        JSONObject geoJson = new JSONObject();
        geoJson.put("type", "Feature");

        JSONObject geometry = new JSONObject();
        geometry.put("type", "Point");
        JSONArray coordinates = new JSONArray();
        coordinates.put(point.getX()).put(point.getY());
        geometry.put("coordinates", coordinates);
        geoJson.put("geometry", geometry);

        JSONObject properties = new JSONObject();
        properties.put("iri", weatherStation.getString("iri"));
        properties.put("name", "Virtual weather station");
        geoJson.put("properties", properties);

        return geoJson;
    }
}
