package uk.ac.cam.cares.jps.agent.fumehood;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class EmailBuilderTest {
    Map<String, List<String>> map1;

    @Before
    public void createMockMaps() throws IOException {
        map1 = new HashMap<>();
        map1.put("FHandWFH", new ArrayList<>());
        map1.put("SashOpeningTsData", new ArrayList<>());
        map1.put("OccupiedStateTsData", new ArrayList<>());
        map1.put("Label", new ArrayList<>());
        map1.put("OccupiedStateTimeStamps", new ArrayList<>());
        map1.put("SashOpeningTimeStamps", new ArrayList<>());
        map1.get("FHandWFH").add("test IRI 1");
        map1.get("FHandWFH").add("test IRI 2");
        map1.get("FHandWFH").add("test IRI 3");
        map1.get("FHandWFH").add("test IRI 4");
        map1.get("FHandWFH").add("test IRI 5");
        map1.get("Label").add("FH01");
        map1.get("Label").add("FH02");
        map1.get("Label").add("FH03");
        map1.get("Label").add("FH04");
        map1.get("Label").add("FH05");
        map1.get("SashOpeningTsData").add("99.03234");
        map1.get("OccupiedStateTsData").add("This device does not have an occupied state.");
        map1.get("SashOpeningTsData").add("This device does not have a sash opening.");
        map1.get("OccupiedStateTsData").add("1.0");
        map1.get("SashOpeningTsData").add("49.0");
        map1.get("OccupiedStateTsData").add("0.0");
        map1.get("SashOpeningTsData").add("This device does not have a sash opening.");
        map1.get("OccupiedStateTsData").add("This device does not have an occupied state.");
        map1.get("SashOpeningTsData").add("79.03234");
        map1.get("OccupiedStateTsData").add("0.0");
        map1.get("SashOpeningTimeStamps").add("2023-05-03 06:18:50 AM");
        map1.get("OccupiedStateTimeStamps").add("Not applicable");
        map1.get("SashOpeningTimeStamps").add("Not applicable");
        map1.get("OccupiedStateTimeStamps").add("2023-05-03 06:18:50 AM");
        map1.get("SashOpeningTimeStamps").add("2023-05-03 06:18:50 AM");
        map1.get("OccupiedStateTimeStamps").add("2023-05-03 06:18:50 AM");
        map1.get("SashOpeningTimeStamps").add("Not applicable");
        map1.get("OccupiedStateTimeStamps").add("Not applicable");
        map1.get("SashOpeningTimeStamps").add("2023-05-03 06:18:50 AM");
        map1.get("OccupiedStateTimeStamps").add("2023-05-03 06:18:50 AM");
    }

    @Test
    public void testParsesMapAndPostProcessing() {
        EmailBuilder emailBuilder = new EmailBuilder();
        Double threshold = 48.0;
        String emailContent = emailBuilder.parsesMapAndPostProcessing(map1, threshold);
        //FH03 and FH05 should have their values highlighted in red
        Assert.assertTrue(emailContent.contains("The following devices (FH03 , FH05  ) are unoccupied and have a sash opening above the threshold of 48.0%."));
        Assert.assertTrue(emailContent.contains("<td style=\"color:Red;\"> FH03</td><td style=\"color:Red;\"> Not occupied since the following timestamp: 2023-05-03 06:18:50 AM</td><td style=\"color:Red;\"> 49.00 % since the following timestamp: 2023-05-03 06:18:50 AM</td></tr>"));
        Assert.assertTrue(emailContent.contains("<td style=\"color:Red;\"> FH05</td><td style=\"color:Red;\"> Not occupied since the following timestamp: 2023-05-03 06:18:50 AM</td><td style=\"color:Red;\"> 79.03 % since the following timestamp: 2023-05-03 06:18:50 AM</td></tr>"));

        //FH01, FH02 and FH04 should be printed accordingly in the email content
        Assert.assertTrue(emailContent.contains("<tr><td> FH01</td><td> This device does not have an occupied state.</td><td> 99.03 % since the following timestamp: 2023-05-03 06:18:50 AM</td></tr>"));
        Assert.assertTrue(emailContent.contains("<tr><td> FH02</td><td> Occupied since the following timestamp: 2023-05-03 06:18:50 AM</td><td> This device does not have a sash opening.</td></tr>"));
        Assert.assertTrue(emailContent.contains("<tr><td> FH04</td><td> This device does not have an occupied state.</td><td> This device does not have a sash opening.</td></tr>"));

        threshold = 50.0;
        emailContent = emailBuilder.parsesMapAndPostProcessing(map1, threshold);
        //only FH05 should have their values highlighted in red
        Assert.assertTrue(emailContent.contains("The following devices (FH05  ) are unoccupied and have a sash opening above the threshold of 50.0%."));
        Assert.assertTrue(!emailContent.contains("<td style=\"color:Red;\"> FH03</td><td style=\"color:Red;\"> Not occupied since the following timestamp: 2023-05-03 06:18:50 AM</td><td style=\"color:Red;\"> 49.00 % since the following timestamp: 2023-05-03 06:18:50 AM</td></tr>"));
        Assert.assertTrue(emailContent.contains("<td style=\"color:Red;\"> FH05</td><td style=\"color:Red;\"> Not occupied since the following timestamp: 2023-05-03 06:18:50 AM</td><td style=\"color:Red;\"> 79.03 % since the following timestamp: 2023-05-03 06:18:50 AM</td></tr>"));
    }
}
