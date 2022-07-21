package uk.ac.cam.cares.jps.agent.weather;


import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

/**
 * in order to run this you must have a valid API key stored in credentials.properties
 * @author Kok Foong Lee
 *
 */
@Disabled
public class WeatherAPIConnectorTest {
	@Test
    public void testAPI() {
    	WeatherAPIConnector.getWeatherDataFromOpenWeather(0, 0);
    }
}
