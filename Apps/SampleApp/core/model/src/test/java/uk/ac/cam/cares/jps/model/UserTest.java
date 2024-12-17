package uk.ac.cam.cares.jps.model;

import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Test;

public class UserTest {

    private static User sampleInstance;
    private static final String SAMPLE_ID = "1";
    private static final String SAMPLE_NAME = "User One";
    private static final String SAMPLE_USERNAME = "user123";
    private static final String SAMPLE_EMAIL = "User@april.biz";

    @Before
    public void setup() {
        sampleInstance = new User(SAMPLE_ID, SAMPLE_NAME, SAMPLE_USERNAME, SAMPLE_EMAIL);
    }

    @Test
    public void getId() {
        // Verify if the constructed object has the required value
        assertEquals(SAMPLE_ID, sampleInstance.getId());
    }

    @Test
    public void getName() {
        // Verify if the constructed object has the required value
        assertEquals(SAMPLE_NAME, sampleInstance.getName());
    }

    @Test
    public void getUsername() {
        // Verify if the constructed object has the required value
        assertEquals(SAMPLE_USERNAME, sampleInstance.getUsername());
    }

    @Test
    public void getEmail() {
        // Verify if the constructed object has the required value
        assertEquals(SAMPLE_EMAIL, sampleInstance.getEmail());
    }
}