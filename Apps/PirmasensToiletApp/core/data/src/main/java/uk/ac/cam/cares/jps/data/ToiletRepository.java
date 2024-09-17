package uk.ac.cam.cares.jps.data;

import org.apache.log4j.Logger;

import java.util.List;

import javax.inject.Inject;

import io.reactivex.Completable;
import io.reactivex.disposables.Disposable;
import io.reactivex.exceptions.UndeliverableException;
import io.reactivex.plugins.RxJavaPlugins;
import uk.ac.cam.cares.jps.model.Toilet;
import uk.ac.cam.cares.jps.network.toilet.ToiletBuildingInfoNetworkSource;
import uk.ac.cam.cares.jps.network.toilet.ToiletInfoNetworkSource;
import uk.ac.cam.cares.jps.network.toilet.ToiletNetworkSource;

public class ToiletRepository {
    private Logger LOGGER = Logger.getLogger(ToiletRepository.class);
    private ToiletNetworkSource toiletNetworkSource;
    private ToiletInfoNetworkSource toiletInfoNetworkSource;
    private ToiletBuildingInfoNetworkSource toiletBuildingInfoNetworkSource;

    private Toilet toiletInfo;
    private Toilet toiletBuildingInfo;

    @Inject
    public ToiletRepository(ToiletNetworkSource toiletNetworkSource,
                            ToiletInfoNetworkSource toiletInfoNetworkSource,
                            ToiletBuildingInfoNetworkSource toiletBuildingInfoNetworkSource) {
        this.toiletNetworkSource = toiletNetworkSource;
        this.toiletInfoNetworkSource = toiletInfoNetworkSource;
        this.toiletBuildingInfoNetworkSource = toiletBuildingInfoNetworkSource;
    }

    public void getToiletInfo(String id, RepositoryCallback<Toilet> callback) {
        RxJavaPlugins.setErrorHandler(throwable -> {
            if (throwable instanceof UndeliverableException) {
                LOGGER.info("Both network call failed. Ignore this RxJava exception.");
            } else {
                if (Thread.currentThread().getUncaughtExceptionHandler() != null) {
                    Thread.currentThread().getUncaughtExceptionHandler().uncaughtException(Thread.currentThread(), throwable);
                }
            }
        });

        Completable toiletNetworkCall = Completable.create(emitter -> toiletInfoNetworkSource.getToiletInfoData(id, toilet -> {
            toiletInfo = toilet;
            emitter.onComplete();
        }, error -> {
            LOGGER.error("failed to get toilet info");
            emitter.onError(error);
        }));

//        Completable buildingNetworkCall = Completable.create(emitter -> toiletBuildingInfoNetworkSource.getBuildingInfoData(lng, lat, toilet -> {
//            toiletBuildingInfo = toilet;
//            emitter.onComplete();
//        }, error -> {
//            LOGGER.error("failed to get toilet building info");
//            emitter.onError(error);
//        }));

        Completable combinedCompletable = Completable.mergeArray(
                toiletNetworkCall
//              , buildingNetworkCall
        );
        Disposable disposable = combinedCompletable.subscribe(
                () -> {
//                    toiletInfo.setName(toiletBuildingInfo.getName());
//                    toiletInfo.setAddress(toiletBuildingInfo.getAddress());
                    LOGGER.debug("TOILET INFO "+toiletInfo);
                    callback.onSuccess(toiletInfo);
                },
                callback::onFailure
        );

    }

    public void getAllToilets(RepositoryCallback<List<Toilet>> callback) {
        toiletNetworkSource.getToiletsData(callback::onSuccess, callback::onFailure);
    }

    public String getRouteGeoJsonUrl() {
        return toiletNetworkSource.getRequestUri();
    }
}
