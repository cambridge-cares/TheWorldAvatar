package uk.ac.cam.cares.jps.data;

import org.apache.log4j.Logger;

import java.util.List;

import javax.inject.Inject;

import uk.ac.cam.cares.jps.model.Toilet;
import uk.ac.cam.cares.jps.network.toilet.ToiletInfoNetworkSource;
import uk.ac.cam.cares.jps.network.toilet.ToiletNetworkSource;

public class ToiletRepository {
    private Logger LOGGER = Logger.getLogger(ToiletRepository.class);
    private ToiletNetworkSource toiletNetworkSource;
    private ToiletInfoNetworkSource toiletInfoNetworkSource;

    @Inject
    public ToiletRepository(ToiletNetworkSource toiletNetworkSource, ToiletInfoNetworkSource toiletInfoNetworkSource) {
        this.toiletNetworkSource = toiletNetworkSource;
        this.toiletInfoNetworkSource = toiletInfoNetworkSource;
    }

    public void getToiletInfo(double lng, double lat, RepositoryCallback<Toilet> callback) {
        toiletInfoNetworkSource.getToiletInfoData(lng, lat, callback::onSuccess, callback::onFailure);
    }

    public void getAllToilets(RepositoryCallback<List<Toilet>> callback) {
        toiletNetworkSource.getToiletsData(callback::onSuccess, callback::onFailure);
    }

    public String getRouteGeoJsonUrl() {
        return toiletNetworkSource.getRequestUri();
    }
}
