package uk.ac.cam.cares.jps.base.scenario;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.net.URISyntaxException;

import org.junit.Test;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class ScenarioHelperTest {

    /*
     * skip the test for getJpsWorkingDir() and getScenarioWorkingDir(), because
     * they return constant
     * 
     * skip the test for getScenarioBucket(), because it accesses file system
     */

    @Test
    public void testGetScenarioPath() {
        String scenarioName = "testscenario";
        assertEquals(ScenarioHelper.SCENARIO_COMP_URL + "/" + scenarioName,
                ScenarioHelper.getScenarioPath(scenarioName));
    }

    @Test
    public void testGetHashedResource() {
        String fragment = "#res";
        String path = "/kb/powerplants/test.owl";
        String host = "www.theworldavatar.com";
        String scheme = "http:";
        String hashedHost = Integer.toString(host.hashCode());

        assertEquals(hashedHost + path,
                ScenarioHelper.getHashedResource(scheme + "//" + host + path + fragment));
    }

    @Test(expected = JPSRuntimeException.class)
    public void testGetHashedResourceWithImproperResource() {
        String improperResource = "resource";
        ScenarioHelper.getHashedResource(improperResource);
    }

    @Test
    public void testFileNameWithinBucket() {
        String fragment = "#res";
        String path = "/kb/powerplants/test.owl";
        String host = "www.theworldavatar.com";
        String scheme = "http:";
        String hashedHost = Integer.toString(host.hashCode());

        String resource = scheme + "//" + host + path + fragment;
        String hashedResource = hashedHost + path;
        String scenarioBucket = "scenario-bucket";

        assertEquals(scenarioBucket + "/" + hashedResource,
                ScenarioHelper.getFileNameWithinBucket(resource, scenarioBucket));
    }

    @Test
    public void testDividePath() throws URISyntaxException {
        String scenarioName = "foo1234567";
        String[] paths = ScenarioHelper.dividePath("/" + scenarioName);

        assertEquals(scenarioName, paths[0]);
        assertNull(paths[1]);
    }

    @Test
    public void testCutHash() {
        String fileName = "http://www.theworldavatar.com/kb/powerplants/test.owl";
        String fragment = "#test";
        assertEquals(fileName, ScenarioHelper.cutHash(fileName + fragment));
    }

    @Test
    public void testCutHashUsingFileNameWithoutFragment() {
        String fileName = "http://www.theworldavatar.com/kb/powerplants/test.owl";
        assertEquals(fileName, ScenarioHelper.cutHash(fileName));
    }

    @Test
    public void testCutHashUsingImproperFileName() {
        String fileName = "http://www.theworldavatar.com/kb/powerplants";
        assertEquals(fileName, ScenarioHelper.cutHash(fileName));
    }

}
