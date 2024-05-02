package uk.ac.cam.cares.jps.data.testdouble;

import com.android.volley.Response;
import com.android.volley.VolleyError;

import uk.ac.cam.cares.jps.model.User;
import uk.ac.cam.cares.jps.network.NetworkSource;

public class FakeUserNetworkSource implements NetworkSource<User> {
    private static final String SAMPLE_ID = "7";
    private static final String SAMPLE_NAME = "Unknown";
    private static final String SAMPLE_USERNAME = "user553";
    private static final String SAMPLE_EMAIL = "unknown@user.com";
    private static final String ERROR_MESSAGE = "Invalid ID: Failed to retrieve user data";

    /**
     * Fake network source does not require any parameters in its constructor
     */
    public FakeUserNetworkSource() {
    }

    /**
     * A method to retrieve hardcoded user data for testing.
     */
    public void getData(String id, Response.Listener<User> onSuccessCallback, Response.ErrorListener onFailureCallback) {
        if (id.equals(SAMPLE_ID)) {
            User fakeUser = new User(SAMPLE_ID, SAMPLE_NAME, SAMPLE_USERNAME, SAMPLE_EMAIL);
            onSuccessCallback.onResponse(fakeUser);
        } else {
            onFailureCallback.onErrorResponse(new VolleyError(ERROR_MESSAGE));
        }
    }

    public String getId() {
        return SAMPLE_ID;
    }

    public String getName() {
        return SAMPLE_NAME;
    }

    public String getUserName() {
        return SAMPLE_USERNAME;
    }

    public String getEmail() {
        return SAMPLE_EMAIL;
    }

    public String getErrorMessage() {
        return ERROR_MESSAGE;
    }
}