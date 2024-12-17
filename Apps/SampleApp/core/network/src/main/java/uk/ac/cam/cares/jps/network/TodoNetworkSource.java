package uk.ac.cam.cares.jps.network;

import android.content.Context;

import com.android.volley.RequestQueue;
import com.android.volley.Response;
import com.android.volley.toolbox.StringRequest;

import org.apache.log4j.Logger;
import org.json.JSONException;

import okhttp3.HttpUrl;
import uk.ac.cam.cares.jps.model.Todo;

public class TodoNetworkSource implements NetworkSource<Todo> {
    private static final Logger LOGGER = Logger.getLogger(TodoNetworkSource.class);
    private final RequestQueue requestQueue;
    private final String baseUrl;

    /**
     * Constructor.
     *
     * @param requestQueue The RequestQueue used for making network requests.
     * @param context      The Context used to access resources such as URLs.
     */
    public TodoNetworkSource(RequestQueue requestQueue, Context context) {
        this.requestQueue = requestQueue;
        this.baseUrl = context.getString(uk.ac.cam.cares.jps.utils.R.string.todos_url);
    }

    /**
     * Get todos from https://jsonplaceholder.typicode.com/todos/{id} with the specified id.
     *
     * @param id                The id of the todos retrieved.
     * @param onSuccessCallback A callback from the repository to handle the successful http request response.
     * @param onFailureCallback A callback from the repository to handle the error response for the failed http request.
     */
    public void getData(String id, Response.Listener<Todo> onSuccessCallback, Response.ErrorListener onFailureCallback) {
        String url = HttpUrl.get(this.baseUrl).newBuilder()
                .addPathSegments(id)
                .build().toString();
        LOGGER.info("Retrieving Todo data for id: " + id);
        StringRequest request = new StringRequest(url,
                response -> {
                    // The network source should process the raw results and pass back the processed object to the repository
                    try {
                        Todo todo = NetworkResponseParser.parseTodoResponse(response);
                        onSuccessCallback.onResponse(todo);
                    } catch (JSONException e) {
                        LOGGER.error(e);
                        throw new RuntimeException(e);
                    }
                },
                // Error listener for failed request
                volleyError -> {
                    LOGGER.error(volleyError.getMessage());
                    onFailureCallback.onErrorResponse(volleyError);
                });
        requestQueue.add(request);
    }
}