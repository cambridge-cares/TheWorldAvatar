package uk.ac.cam.cares.jps.network;

import android.content.Context;

import com.android.volley.RequestQueue;
import com.android.volley.Response;
import com.android.volley.toolbox.StringRequest;

import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import okhttp3.HttpUrl;
import uk.ac.cam.cares.jps.model.User;

public class UserNetworkSource {
    private static final Logger LOGGER = Logger.getLogger(UserNetworkSource.class);
    private RequestQueue requestQueue;
    private Context context;

    /**
     * Constructor.
     *
     * @param requestQueue The RequestQueue used for making network requests.
     * @param context      The Context used to access resources such as URLs.
     */
    public UserNetworkSource(RequestQueue requestQueue, Context context) {
        this.requestQueue = requestQueue;
        this.context = context;
    }

    /**
     * Get user from https://jsonplaceholder.typicode.com/users/{id} with the specified id
     *
     * @param id             The id of the user to be retrieved
     * @param onSuccessUpper A callback passed from repository to be called when the http request is returned, so the upper level get notified of the result
     * @param onFailureUpper A callback passed from repository to be called when the http request is failed, so the upper level get notified of the failure
     */
    public void getUser(String id, Response.Listener<User> onSuccessUpper, Response.ErrorListener onFailureUpper) {
        String url = HttpUrl.get(context.getString(uk.ac.cam.cares.jps.utils.R.string.users_url)).newBuilder()
                .addPathSegments(id)
                .build().toString();
        StringRequest request = new StringRequest(url,
                s -> {
                    // The network source should process the raw results and pass back the processed object to the repository
                    try {
                        JSONObject result = new JSONObject(s);
                        User user = new User(result.optString("id"),
                                result.optString("name"),
                                result.optString("username"),
                                result.optString("email"));
                        onSuccessUpper.onResponse(user);
                    } catch (JSONException e) {
                        throw new RuntimeException(e);
                    }
                },
                volleyError -> {
                    LOGGER.error(volleyError.getMessage());
                    onFailureUpper.onErrorResponse(volleyError);
                });
        requestQueue.add(request);
    }
}
