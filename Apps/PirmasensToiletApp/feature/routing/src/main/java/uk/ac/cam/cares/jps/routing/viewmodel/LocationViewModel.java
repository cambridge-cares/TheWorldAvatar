package uk.ac.cam.cares.jps.routing.viewmodel;

import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import com.mapbox.geojson.Point;

import javax.annotation.Nullable;


public class LocationViewModel extends ViewModel {
    private MutableLiveData<Point> currentLocation = new MutableLiveData<>();

    public MutableLiveData<Point> getCurrentLocationLiveData() {
        return currentLocation;
    }

    public void setCurrentLocation(Point point) {
        currentLocation.setValue(point);
    }

    @Nullable
    public Point getCurrentLocationValue() {
        return currentLocation.getValue();
    }
}
