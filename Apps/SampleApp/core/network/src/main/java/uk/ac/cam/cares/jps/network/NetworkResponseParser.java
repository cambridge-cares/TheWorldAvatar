package uk.ac.cam.cares.jps.network;

import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.model.Todo;
import uk.ac.cam.cares.jps.model.User;

/**
 * This class provides static methods to parse JSON responses into specific data models.
 */
public class NetworkResponseParser {

    /**
     * Private constructor to prevent instantiation of the class.
     */
    private NetworkResponseParser() {
    }

    /**
     * Parses a JSON response into a Todo data model.
     *
     * @param response The JSON response to parse.
     * @return the data in todo data model format.
     */
    public static Todo parseTodoResponse(String response) throws JSONException {
        JSONObject serialisedJson = new JSONObject(response);
        return new Todo(serialisedJson.optString("userId"),
                serialisedJson.optString("id"),
                serialisedJson.optString("title"),
                serialisedJson.optBoolean("completed"));
    }

    /**
     * Parses a JSON response into a User data model.
     *
     * @param response The JSON response to parse.
     * @return the data in user data model format.
     */
    public static User parseUserResponse(String response) throws JSONException {
        JSONObject serialisedJson = new JSONObject(response);
        return new User(serialisedJson.optString("id"),
                serialisedJson.optString("name"),
                serialisedJson.optString("username"),
                serialisedJson.optString("email"));
    }
}
