package uk.ac.cam.cares.jps.timeline.viewmodel;

import android.content.Context;
import android.net.ConnectivityManager;
import android.net.NetworkCapabilities;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import dagger.hilt.android.qualifiers.ApplicationContext;

/**
 * ViewModel that checks the device network connection
 */
@HiltViewModel
public class ConnectionViewModel extends ViewModel {
    private ConnectivityManager connectivityManager;
    private MutableLiveData<Boolean> _hasConnection = new MutableLiveData<>();
    private LiveData<Boolean> hasConnection = _hasConnection;

    /**
     * Constructor of the class. Instantiation is done with ViewProvider and dependency injection
     * @param context fragment context
     */
    @Inject
    public ConnectionViewModel(@ApplicationContext Context context) {
        connectivityManager = context.getSystemService(ConnectivityManager.class);
    }

    /**
     * Check network connection
     */
    public void checkNetworkConnection() {
        NetworkCapabilities networkCapabilities = connectivityManager.getNetworkCapabilities(connectivityManager.getActiveNetwork());
        _hasConnection.setValue(networkCapabilities != null &&
                networkCapabilities.hasCapability(NetworkCapabilities.NET_CAPABILITY_INTERNET) &&
                networkCapabilities.hasCapability(NetworkCapabilities.NET_CAPABILITY_VALIDATED) &&
                (networkCapabilities.hasTransport(NetworkCapabilities.TRANSPORT_WIFI) ||
                        networkCapabilities.hasTransport(NetworkCapabilities.TRANSPORT_VPN) ||
                        networkCapabilities.hasTransport(NetworkCapabilities.TRANSPORT_CELLULAR) ||
                        networkCapabilities.hasTransport(NetworkCapabilities.TRANSPORT_ETHERNET)));
    }

    /**
     * Get hasConnection object
     * @return hasConnection object
     */
    public LiveData<Boolean> getHasConnection() {
        return hasConnection;
    }
}
