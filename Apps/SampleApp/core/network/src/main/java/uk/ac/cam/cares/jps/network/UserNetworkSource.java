package uk.ac.cam.cares.jps.network;

import android.content.Context;

import com.android.volley.RequestQueue;
import com.android.volley.Response;
import com.android.volley.toolbox.StringRequest;

import org.apache.log4j.Logger;
import org.json.JSONException;

import okhttp3.HttpUrl;
import uk.ac.cam.cares.jps.model.User;

public class UserNetworkSource implements NetworkSource<User> {
    private static final Logger LOGGER = Logger.getLogger(UserNetworkSource.class);
    private final RequestQueue requestQueue;
    private final String baseUrl;

    /**
     * Constructor.
     *
     * @param requestQueue The RequestQueue used for making network requests.
     * @param context      The Context used to access resources such as URLs.
     */
    public UserNetworkSource(RequestQueue requestQueue, Context context) {
        this.requestQueue = requestQueue;
        this.baseUrl = context.getString(uk.ac.cam.cares.jps.utils.R.string.users_url);
    }

    /**
     * Get user from https://jsonplaceholder.typicode.com/users/{id} with the specified id.
     *
     * @param id                The id of the todos retrieved.
     * @param onSuccessCallback A callback from the repository to handle the successful http request response.
     * @param onFailureCallback A callback from the repository to handle the error response for the failed http request.
     */
    public void getData(String id, Response.Listener<User> onSuccessCallback, Response.ErrorListener onFailureCallback) {
        String url = HttpUrl.get(this.baseUrl).newBuilder()
                .addPathSegments(id)
                .build().toString();
        LOGGER.info("Retrieving User data for id: " + id);
        StringRequest request = new StringRequest(url,
                response -> {
                    // The network source should process the raw results and pass back the processed object to the repository
                    try {
                        User user = NetworkResponseParser.parseUserResponse(response);
                        onSuccessCallback.onResponse(user);
                    } catch (JSONException e) {
                        throw new RuntimeException(e);
                    }
                },
                volleyError -> {
                    LOGGER.error(volleyError.getMessage());
                    onFailureCallback.onErrorResponse(volleyError);
                });
        requestQueue.add(request);
    }
}
