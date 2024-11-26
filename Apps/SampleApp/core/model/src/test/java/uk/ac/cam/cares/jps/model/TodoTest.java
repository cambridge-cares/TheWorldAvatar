package uk.ac.cam.cares.jps.model;

import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Test;

public class TodoTest {
    private static Todo sampleInstance;
    private static final String SAMPLE_USER_ID = "user123";
    private static final String SAMPLE_ID = "todo123";
    private static final String SAMPLE_TITLE = "Test Todo";
    private static final boolean SAMPLE_COMPLETED_INDICATOR = false;

    @Before
    public void setup() {
        sampleInstance = new Todo(SAMPLE_USER_ID, SAMPLE_ID, SAMPLE_TITLE, SAMPLE_COMPLETED_INDICATOR);
    }

    @Test
    public void getUserId() {
        // Verify if the constructed object has the required value
        assertEquals(SAMPLE_USER_ID, sampleInstance.getUserId());
    }

    @Test
    public void getId() {
        // Verify if the constructed object has the required value
        assertEquals(SAMPLE_ID, sampleInstance.getId());
    }

    @Test
    public void getTitle() {
        // Verify if the constructed object has the required value
        assertEquals(SAMPLE_TITLE, sampleInstance.getTitle());
    }

    @Test
    public void getCompleted() {
        // Verify if the constructed object has the required value
        assertEquals(SAMPLE_COMPLETED_INDICATOR, sampleInstance.getCompleted());
    }
}