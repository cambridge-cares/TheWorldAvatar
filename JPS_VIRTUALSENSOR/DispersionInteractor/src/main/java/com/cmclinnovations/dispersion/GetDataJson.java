package com.cmclinnovations.dispersion;

import java.io.IOException;
import java.time.Instant;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

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

    @Override
    protected void doGet(HttpServletRequest req, HttpServletResponse resp) {
        String pollutant = req.getParameter("pollutant");
        String timestep = req.getParameter("timestep");
        String scopeLabel = req.getParameter("scopeLabel");
        String derivationIri = req.getParameter("derivationIri");
        String pollutantLabel = req.getParameter("pollutantLabel");
        JSONObject weatherStation = new JSONObject(req.getParameter("weatherStation"));

        List<String> layers = queryClient.getLayers(pollutant,
                Instant.parse(timestep).getEpochSecond(), derivationIri);

        String dispersionLayer = layers.get(0);
        String shipsLayer = layers.get(1);
        String buildingsLayer = layers.get(2);
        String staticPointSourceLayer = layers.get(3);

        JSONObject dataJson = createDataJson(pollutantLabel, scopeLabel, weatherStation, dispersionLayer, shipsLayer,
                buildingsLayer, staticPointSourceLayer);

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
    }

    private JSONObject createDataJson(String pollutantLabel, String scopeLabel, JSONObject weatherStation,
            String dispersionLayerName, String shipLayerName, String buildingLayerName, String staticSourceLayerName) {
        String dispWms = Config.GEOSERVER_URL + "/" + Config.GEOSERVER_WORKSPACE +
                "/wms?service=WMS&version=1.1.0&request=GetMap&width=256&height=256&srs=EPSG:3857&format=application/vnd.mapbox-vector-tile&transparent=true"
                +
                "&bbox={bbox-epsg-3857}"
                + String.format("&layers=%s:%s", Config.GEOSERVER_WORKSPACE, dispersionLayerName);

        JSONObject group = new JSONObject();
        group.put("name", scopeLabel);
        group.put("stack", "http://localhost:3838");

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

        JSONObject visibility = new JSONObject();
        visibility.put("visibility", "visible");

        group.put("sources", sources);
        JSONObject dispersionLayer = new JSONObject();
        dispersionLayer.put("id", "dispersion-layer");
        dispersionLayer.put("type", "fill");
        dispersionLayer.put("name", pollutantLabel);
        dispersionLayer.put("source", "dispersion-source");
        dispersionLayer.put("source-layer", dispersionLayerName);
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
        weatherLayer.put("layout", new JSONObject().put("visibility", "none"));

        JSONObject weatherPaint = new JSONObject();
        weatherPaint.put("circle-color", "blue");
        weatherLayer.put("paint", weatherPaint);

        layers.put(dispersionLayer).put(weatherLayer);

        group.put("layers", layers);

        // optional layers
        if (shipLayerName != null) {
            String shipWms = Config.GEOSERVER_URL + "/" + Config.GEOSERVER_WORKSPACE +
                    "/wms?service=WMS&version=1.1.0&request=GetMap&width=256&height=256&srs=EPSG:3857&format=application/vnd.mapbox-vector-tile"
                    + "&bbox={bbox-epsg-3857}"
                    + String.format("&layers=%s:%s", Config.GEOSERVER_WORKSPACE, shipLayerName);
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
            shipLayer.put("source-layer", shipLayerName);
            shipLayer.put("minzoom", 4);
            shipLayer.put("layout", visibility);

            JSONObject shipPaint = new JSONObject();
            shipPaint.put("circle-color", "black");
            shipLayer.put("paint", shipPaint);

            layers.put(shipLayer);
        }

        if (buildingLayerName != null) {
            String buildingWms = Config.GEOSERVER_URL + "/" + Config.GEOSERVER_WORKSPACE +
                    "/wms?service=WMS&version=1.1.0&request=GetMap&width=256&height=256&srs=EPSG:3857&format=application/vnd.mapbox-vector-tile&transparent=true"
                    +
                    "&bbox={bbox-epsg-3857}"
                    + String.format("&layers=%s:%s", Config.GEOSERVER_WORKSPACE, buildingLayerName);
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
            buildingLayer.put("source-layer", buildingLayerName);
            buildingLayer.put("minzoom", 4);
            buildingLayer.put("layout", visibility);

            JSONObject buildingLayerPaint = new JSONObject();
            buildingLayerPaint.put("fill-extrusion-base", new JSONArray().put("get").put("base"));
            buildingLayerPaint.put("fill-extrusion-color", new JSONArray().put("get").put("color"));
            buildingLayerPaint.put("fill-extrusion-height", new JSONArray().put("get").put("height"));
            buildingLayer.put("paint", buildingLayerPaint);

            layers.put(buildingLayer);
        }

        if (staticSourceLayerName != null) {
            String staticSourceWMS = Config.GEOSERVER_URL + "/" + Config.GEOSERVER_WORKSPACE +
                    "/wms?service=WMS&version=1.1.0&request=GetMap&width=256&height=256&srs=EPSG:3857&format=application/vnd.mapbox-vector-tile"
                    + "&bbox={bbox-epsg-3857}"
                    + String.format("&layers=%s:%s", Config.GEOSERVER_WORKSPACE, staticSourceLayerName);
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
            staticLayer.put("source-layer", staticSourceLayerName);
            staticLayer.put("minzoom", 4);
            staticLayer.put("layout", visibility);

            JSONObject staticPaint = new JSONObject();
            staticPaint.put("circle-color", "black");
            staticLayer.put("paint", staticPaint);

            layers.put(staticLayer);
        }

        JSONObject data = new JSONObject();
        data.put("name", "All Data");
        data.put("groups", new JSONArray().put(group));

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
