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

// todo: need to refactor this class to remove the framework dependent component
@HiltViewModel
public class ConnectionViewModel extends ViewModel {
    private ConnectivityManager connectivityManager;
    private MutableLiveData<Boolean> _hasConnection = new MutableLiveData<>();
    private LiveData<Boolean> hasConnection = _hasConnection;

    @Inject
    public ConnectionViewModel(@ApplicationContext Context context) {
        connectivityManager = context.getSystemService(ConnectivityManager.class);
    }

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

    public LiveData<Boolean> getHasConnection() {
        return hasConnection;
    }
    public void setHasConnection(Boolean hasConnection) {
        _hasConnection.setValue(hasConnection);
    }
}
