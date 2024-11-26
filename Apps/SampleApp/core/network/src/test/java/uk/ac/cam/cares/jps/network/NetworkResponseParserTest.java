package uk.ac.cam.cares.jps.network;

import static org.junit.Assert.assertEquals;

import org.json.JSONException;
import org.junit.Test;

import uk.ac.cam.cares.jps.model.Todo;
import uk.ac.cam.cares.jps.model.User;

public class NetworkResponseParserTest {
    private static final String SAMPLE_USER_ID = "51023";
    private static final String SAMPLE_ID = "1";
    private static final String SAMPLE_TITLE = "Do the dishes";
    private static final String SAMPLE_NAME = "John Doe";
    private static final String SAMPLE_USERNAME = "user193";
    private static final String SAMPLE_EMAIL = "john@user.com";
    private static final boolean SAMPLE_COMPLETED = false;

    @Test
    public void parseTodoResponse_Success() throws JSONException {
        // Set up
        String sampleResponse = "{\"userId\":\"" + SAMPLE_USER_ID +
                "\",\"id\":\"" + SAMPLE_ID +
                "\",\"title\":\"" + SAMPLE_TITLE +
                "\",\"completed\":" + SAMPLE_COMPLETED + "}";
        // Execute
        Todo todo = NetworkResponseParser.parseTodoResponse(sampleResponse);
        // Verify
        assertEquals(SAMPLE_USER_ID, todo.getUserId());
        assertEquals(SAMPLE_ID, todo.getId());
        assertEquals(SAMPLE_TITLE, todo.getTitle());
        assertEquals(SAMPLE_COMPLETED, todo.getCompleted());
    }

    @Test
    public void parseTodoResponse_MissingParameters() throws JSONException {
        // Set up
        String sampleResponse = "{\"userId\":\"" + SAMPLE_USER_ID +
                "\",\"id\":\"" + SAMPLE_ID +
                "\",\"completed\":" + SAMPLE_COMPLETED + "}";
        // Execute
        Todo todo = NetworkResponseParser.parseTodoResponse(sampleResponse);
        // Verify
        assertEquals(SAMPLE_USER_ID, todo.getUserId());
        assertEquals(SAMPLE_ID, todo.getId());
        assertEquals("", todo.getTitle());
        assertEquals(SAMPLE_COMPLETED, todo.getCompleted());
    }

    @Test
    public void parseUserResponse_Success() throws JSONException {
        // Set up
        String sampleResponse = "{\"id\":\"" + SAMPLE_ID +
                "\",\"name\":\"" + SAMPLE_NAME +
                "\",\"username\":\"" + SAMPLE_USERNAME +
                "\",\"email\":" + SAMPLE_EMAIL + "}";
        // Execute
        User user = NetworkResponseParser.parseUserResponse(sampleResponse);
        // Verify
        assertEquals(SAMPLE_ID, user.getId());
        assertEquals(SAMPLE_NAME, user.getName());
        assertEquals(SAMPLE_USERNAME, user.getUsername());
        assertEquals(SAMPLE_EMAIL, user.getEmail());
    }

    @Test
    public void parseUserResponse_MissingParameters() throws JSONException {
        // Set up
        String sampleResponse = "{\"id\":\"" + SAMPLE_ID +
                "\",\"username\":\"" + SAMPLE_USERNAME +
                "\",\"email\":" + SAMPLE_EMAIL + "}";
        // Execute
        User user = NetworkResponseParser.parseUserResponse(sampleResponse);
        // Verify
        assertEquals(SAMPLE_ID, user.getId());
        assertEquals("", user.getName());
        assertEquals(SAMPLE_USERNAME, user.getUsername());
        assertEquals(SAMPLE_EMAIL, user.getEmail());
    }
}