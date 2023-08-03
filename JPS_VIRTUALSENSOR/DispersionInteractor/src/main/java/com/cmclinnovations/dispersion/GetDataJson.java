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

        List<String> dispersionAndShipLayer = queryClient.getDispersionAndShipLayer(pollutant,
                Instant.parse(timestep).getEpochSecond(), derivationIri);

        JSONObject dataJson = createDataJson(pollutantLabel, dispersionAndShipLayer, scopeLabel);

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

    private JSONObject createDataJson(String pollutantLabel, List<String> dispersionAndShipLayerName,
            String scopeLabel) {
        String dispWms = Config.GEOSERVER_URL + "/" + Config.GEOSERVER_WORKSPACE +
                "/wms?service=WMS&version=1.1.0&request=GetMap&width=256&height=256&srs=EPSG:3857&format=application/vnd.mapbox-vector-tile&transparent=true"
                +
                "&bbox={bbox-epsg-3857}"
                + String.format("&layers=%s:%s", Config.GEOSERVER_WORKSPACE, dispersionAndShipLayerName.get(0));

        String shipWms = Config.GEOSERVER_URL + "/" + Config.GEOSERVER_WORKSPACE +
                "/wms?service=WMS&version=1.1.0&request=GetMap&width=256&height=256&srs=EPSG:3857&format=application/vnd.mapbox-vector-tile"
                + "&bbox={bbox-epsg-3857}"
                + String.format("&layers=%s:%s", Config.GEOSERVER_WORKSPACE, dispersionAndShipLayerName.get(1));

        JSONObject group = new JSONObject();
        group.put("name", scopeLabel);
        group.put("stack", "http://localhost:3838");

        JSONArray sources = new JSONArray();
        JSONObject dispersionSource = new JSONObject();
        dispersionSource.put("id", "dispersion-source");
        dispersionSource.put("type", "vector");
        dispersionSource.put("tiles", new JSONArray().put(dispWms));
        sources.put(dispersionSource);

        JSONObject shipSource = new JSONObject();
        shipSource.put("id", "ship-source");
        shipSource.put("type", "vector");
        shipSource.put("tiles", new JSONArray().put(shipWms));
        sources.put(shipSource);

        group.put("sources", sources);
        JSONArray layers = new JSONArray();
        JSONObject dispersionLayer = new JSONObject();
        dispersionLayer.put("id", "dispersion-layer");
        dispersionLayer.put("type", "fill");
        dispersionLayer.put("name", pollutantLabel);
        dispersionLayer.put("source", "dispersion-source");
        dispersionLayer.put("source-layer", dispersionAndShipLayerName.get(0));
        dispersionLayer.put("minzoom", 4);
        dispersionLayer.put("layout", new JSONObject().put("visibility", "visible"));

        JSONObject dispersionPaint = new JSONObject();
        JSONArray properties = new JSONArray();
        properties.put("get");
        properties.put("fill");
        dispersionPaint.put("fill-color", properties);
        dispersionPaint.put("fill-opacity", 0.5);
        properties.put(1, "stroke");
        dispersionPaint.put("fill-outline-color", properties);
        dispersionLayer.put("paint", dispersionPaint);

        JSONObject shipLayer = new JSONObject();
        shipLayer.put("id", "ships-layer");
        shipLayer.put("type", "circle");
        shipLayer.put("name", "Ships");
        shipLayer.put("source", "ship-source");
        shipLayer.put("source-layer", dispersionAndShipLayerName.get(1));
        shipLayer.put("minzoom", 4);
        shipLayer.put("layout", new JSONObject().put("visibility", "visible"));

        layers.put(dispersionLayer).put(shipLayer);

        group.put("layers", layers);

        JSONObject data = new JSONObject();
        data.put("name", "All Data");
        data.put("groups", new JSONArray().put(group));

        return data;

    }
}
