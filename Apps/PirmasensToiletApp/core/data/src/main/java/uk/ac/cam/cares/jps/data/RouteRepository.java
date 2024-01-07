package uk.ac.cam.cares.jps.data;

import org.apache.log4j.Logger;

import io.reactivex.Completable;
import io.reactivex.disposables.Disposable;
import io.reactivex.exceptions.UndeliverableException;
import io.reactivex.plugins.RxJavaPlugins;
import uk.ac.cam.cares.jps.network.route.RouteNetworkSource;
import uk.ac.cam.cares.jps.network.route.VertexNetworkSource;

public class RouteRepository {
    private Logger LOGGER = Logger.getLogger(RouteRepository.class);
    private VertexNetworkSource vertexNetworkSource;
    private RouteNetworkSource routeNetworkSource;
    private String startPointId;
    private String endPointId;

    public RouteRepository(VertexNetworkSource vertexNetworkSource, RouteNetworkSource routeNetworkSource) {
        this.vertexNetworkSource = vertexNetworkSource;
        this.routeNetworkSource = routeNetworkSource;
    }

    public void getRouteGeoJsonUrl(double startLng, double startLat, double endLng, double endLat, RepositoryCallback<String> callback) {
        RxJavaPlugins.setErrorHandler(throwable -> {
            if (throwable instanceof UndeliverableException) {
                LOGGER.info("Both network call failed. Ignore this RxJava exception.");
            } else {
                if (Thread.currentThread().getUncaughtExceptionHandler() != null) {
                    Thread.currentThread().getUncaughtExceptionHandler().uncaughtException(Thread.currentThread(), throwable);
                }
            }
        });

        Completable startPointNetworkCall = Completable.create(emitter -> vertexNetworkSource.getVertexId(startLng, startLat,
                vertexId -> {
                    startPointId = vertexId;
                    emitter.onComplete();
                },
                emitter::onError));

        Completable endPointNetworkCall = Completable.create(emitter -> vertexNetworkSource.getVertexId(endLng, endLat,
                vertexId -> {
                    endPointId = vertexId;
                    emitter.onComplete();
                },
                emitter::onError));

        Completable combinedCompletable = Completable.mergeArray(startPointNetworkCall, endPointNetworkCall);
        Disposable disposable = combinedCompletable.subscribe(
                () -> {
                    LOGGER.info("Get both points id");
                    routeNetworkSource.getRouteGeoJsonData(startPointId, endPointId, callback::onSuccess, callback::onFailure);
                },
                error -> {
                    LOGGER.error(error.getMessage());
                    callback.onFailure(error);
                }
        );
    }

    public void getRouteGeoJsonUrl(double lng, double lat, boolean isStart, RepositoryCallback<String> callback) {
        vertexNetworkSource.getVertexId(lng, lat,
                vertexId -> {
                    if (isStart) {
                        startPointId = vertexId;
                    } else {
                        endPointId = vertexId;
                    }

                    routeNetworkSource.getRouteGeoJsonData(startPointId, endPointId, callback::onSuccess, callback::onFailure);
                },
                callback::onFailure);
    }
    private String getRouteGeoJsonUrl() {
        return String.format("http://localhost:3838/geoserver/wfs?service=WFS&version=1.0.0&" +
                "request=GetFeature&" +
                "typeName=pirmasens:routing_pointsofinterest&" +
                "outputformat=application/json&" +
                "viewparams=source:%s;target:%s;", startPointId, endPointId);
    }
}
