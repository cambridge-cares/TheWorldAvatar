package com.cmclinnovations.dispersion;

import java.io.IOException;
import java.time.Instant;

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

        String dispersionLayer = queryClient.getDispersionLayer(pollutant, Instant.parse(timestep).getEpochSecond(),
                derivationIri);

        JSONObject dataJson = createDataJson(pollutant, dispersionLayer, scopeLabel);

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

    private JSONObject createDataJson(String pollutant, String dispersionLayerName, String scopeLabel) {
        String dispWms = Config.GEOSERVER_URL + "/" + Config.GEOSERVER_WORKSPACE +
                "/wms?service=WMS&version=1.1.0&request=GetMap&width=256&height=256&srs=EPSG:3857&format=application/vnd.mapbox-vector-tile&transparent=true"
                +
                "&bbox={bbox-epsg-3857}"
                + String.format("&layers=%s:%s", Config.GEOSERVER_WORKSPACE, dispersionLayerName);

        JSONObject group = new JSONObject();
        group.put("name", scopeLabel);
        group.put("stack", "http://localhost:3838");

        JSONArray sources = new JSONArray();
        JSONObject dispersionSource = new JSONObject();
        dispersionSource.put("id", "dispersion-source_" + dispersionLayerName);
        dispersionSource.put("type", "vector");
        dispersionSource.put("tiles", new JSONArray().put(dispWms));
        sources.put(dispersionSource);

        group.put("sources", sources);
        JSONArray layers = new JSONArray();
        JSONObject dispersionLayer = new JSONObject();
        dispersionLayer.put("id", dispersionLayerName);
        dispersionLayer.put("type", "fill");
        dispersionLayer.put("name", pollutant);
        dispersionLayer.put("source", "dispersion-source_" + dispersionLayerName);
        dispersionLayer.put("source-layer", dispersionLayerName);
        dispersionLayer.put("minzoom", 4);
        dispersionLayer.put("layout", new JSONObject().put("visibility", "visible"));

        JSONObject paint = new JSONObject();
        JSONArray properties = new JSONArray();
        properties.put("get");
        properties.put("fill");
        paint.put("fill-color", properties);
        paint.put("fill-opacity", 0.5);
        properties.put(1, "stroke");
        paint.put("fill-outline-color", properties);
        dispersionLayer.put("paint", paint);
        layers.put(dispersionLayer);

        group.put("layers", layers);

        JSONObject data = new JSONObject();
        data.put("name", "All Data");
        data.put("groups", new JSONArray().put(group));

        return data;

    }
}
