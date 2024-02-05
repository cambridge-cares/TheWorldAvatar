package uk.ac.cam.cares.jps.routing.ui.manager;

import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;

import com.mapbox.bindgen.Expected;
import com.mapbox.bindgen.None;
import com.mapbox.bindgen.Value;
import com.mapbox.maps.LayerPosition;
import com.mapbox.maps.MapView;

import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.Objects;

import uk.ac.cam.cares.jps.model.Route;
import uk.ac.cam.cares.jps.routing.viewmodel.RoutingViewModel;

public class RouteManager {
    private Logger LOGGER = Logger.getLogger(RouteManager.class);
    private RoutingViewModel routingViewModel;

    public RouteManager(MapView mapView, Fragment fragment) {

        routingViewModel = new ViewModelProvider(fragment).get(RoutingViewModel.class);
        routingViewModel.showRoute.observe(fragment, showRoute -> mapView.getMapboxMap().getStyle(style -> {
            Expected<String, None> removeLayerSuccess = style.removeStyleLayer("route_layer");
            Expected<String, None> removeSourceSuccess = style.removeStyleSource("route");
            LOGGER.debug("Route: layer and source removed result " + (removeSourceSuccess.isError() && removeLayerSuccess.isError() ? removeSourceSuccess.getError() + "\n" + removeLayerSuccess.getError() : "success"));

            if (!showRoute) {
                return;
            }

            Route route = routingViewModel.routeGeoJsonData.getValue();
            if (route == null || route.getGeojsonString().isEmpty()) {
                LOGGER.info("should show route, but route is null or data is empty");
                return;
            }

            String data = route.getGeojsonString();
            JSONObject sourceJson = new JSONObject();
            try {
                sourceJson.put("type", "geojson");
                sourceJson.put("data", data);
            } catch (JSONException e) {
                throw new RuntimeException(e);
            }
            Expected<String, None> success = style.addStyleSource("route", Objects.requireNonNull(Value.fromJson(sourceJson.toString()).getValue()));
            LOGGER.debug("route: source created " + (success.isError() ? success.getError() : "success"));

            JSONObject layerJson = new JSONObject();
            try {
                layerJson.put("id", "route_layer");
                layerJson.put("type", "line");
                layerJson.put("source", "route");
            } catch (JSONException e) {
                throw new RuntimeException(e);
            }
            Expected<String, None> layerSuccess = style.addStyleLayer(Objects.requireNonNull(Value.fromJson(layerJson.toString()).getValue()), new LayerPosition(null, null, null));
            LOGGER.debug("route: layer created " + (layerSuccess.isError() ? layerSuccess.getError() : "success"));
        }));
    }
}
