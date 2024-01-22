package uk.ac.cam.cares.jps.data;

import org.apache.log4j.Logger;

import java.util.List;

import uk.ac.cam.cares.jps.model.Toilet;
import uk.ac.cam.cares.jps.network.poi.ToiletNetworkSource;

public class ToiletRepository {
    private Logger LOGGER = Logger.getLogger(ToiletRepository.class);
    private ToiletNetworkSource toiletNetworkSource;

    public ToiletRepository(ToiletNetworkSource toiletNetworkSource){
        this.toiletNetworkSource = toiletNetworkSource;
    }
    public void getRouteGeoJsonUrl(RepositoryCallback<List<Toilet>> callback) {
        toiletNetworkSource.getToiletsData(callback::onSuccess, callback::onFailure);
    }

    public String getRouteGeoJsonUrl() {
        return toiletNetworkSource.getRequestUri();
    }
}
