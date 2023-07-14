package uk.ac.cam.cares.jsp.integration;

import java.io.IOException;
import org.junit.jupiter.api.Test;

public class ConfigTest {
    @Test
    void testRetrieveSQLConfig() throws IOException {
        Config c = new Config();
        String[] configInfo = c.retrieveSQLConfig();
        System.out.println(configInfo);
        
    }

}
