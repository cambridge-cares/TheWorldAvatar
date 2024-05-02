package uk.ac.cam.cares.jps.network;

import com.android.volley.Response;

/**
 * An interface for retrieving data from a network source.
 */
public interface NetworkSource<T> {
    /**
     * Retrieves the specified data from the network.
     *
     * @param id                The id path for the network url.
     * @param onSuccessCallback A callback from the repository to handle the successful http request response.
     * @param onFailureCallback A callback from the repository to handle the error response for the failed http request.
     */
    void getData(String id, Response.Listener<T> onSuccessCallback, Response.ErrorListener onFailureCallback);
}
