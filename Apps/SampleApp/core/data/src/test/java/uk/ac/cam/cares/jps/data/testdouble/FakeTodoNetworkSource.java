package uk.ac.cam.cares.jps.data.testdouble;

import com.android.volley.Response;
import com.android.volley.VolleyError;

import uk.ac.cam.cares.jps.model.Todo;
import uk.ac.cam.cares.jps.network.NetworkSource;

public class FakeTodoNetworkSource implements NetworkSource<Todo> {
    private static final String SAMPLE_USER_ID = "6165";
    private static final String SAMPLE_ID = "7";
    private static final String SAMPLE_TITLE = "Testing the network source";
    private static final boolean SAMPLE_COMPLETED = false;
    private static final String ERROR_MESSAGE = "Invalid ID: Failed to retrieve to do data";

    /**
     * Fake network source does not require any parameters in its constructor
     */
    public FakeTodoNetworkSource() {
    }

    /**
     * A method to retrieve hardcoded to do data for testing.
     */
    public void getData(String id, Response.Listener<Todo> onSuccessCallback, Response.ErrorListener onFailureCallback) {
        if (id.equals(SAMPLE_ID)) {
            Todo fakeTodo = new Todo(SAMPLE_USER_ID, SAMPLE_ID, SAMPLE_TITLE, SAMPLE_COMPLETED);
            onSuccessCallback.onResponse(fakeTodo);
        } else {
            onFailureCallback.onErrorResponse(new VolleyError(ERROR_MESSAGE));
        }
    }

    public String getId() {
        return SAMPLE_ID;
    }

    public String getUserId() {
        return SAMPLE_USER_ID;
    }

    public String getTitle() {
        return SAMPLE_TITLE;
    }

    public boolean getCompleted() {
        return SAMPLE_COMPLETED;
    }

    public String getErrorMessage() {
        return ERROR_MESSAGE;
    }
}