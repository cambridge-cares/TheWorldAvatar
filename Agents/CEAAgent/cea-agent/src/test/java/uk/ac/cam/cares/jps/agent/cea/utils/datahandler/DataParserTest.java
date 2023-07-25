package uk.ac.cam.cares.jps.agent.cea.utils.datahandler;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;

import org.json.JSONArray;
import org.json.JSONObject;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;

public class DataParserTest {
    @Test
    public void testGetList() {
        JSONObject testJSON = new JSONObject();

        List<String> testList = new ArrayList<>();
        testList.add("test");
        testList.add("test1");

        JSONArray testArray = new JSONArray(testList);

        testJSON.put("test", testArray);

        List<String> result = DataParser.getList(testJSON, "test");

        assertEquals(testList.size(), result.size());
        assertIterableEquals(testList, result);
    }

    @Test
    public void getTimeSeriesList() {
        JSONObject testJSON = new JSONObject();

        List<String> testList = new ArrayList<>();
        testList.add("1.5");
        testList.add("2.5");
        testList.add("3.5");

        List<String> testList1 = new ArrayList<>();
        testList1.add("4.5");
        testList1.add("5.5");

        List<List<String>> listOfLists = new ArrayList<>();
        listOfLists.add(testList);
        listOfLists.add(testList1);

        testJSON.put("testKey", listOfLists);

        List<Double> expectedList = new ArrayList<>();
        expectedList.add(4.5);
        expectedList.add(5.5);

        // test time series retrieved correctly
        List<Double> result = DataParser.getTimeSeriesList(testJSON, "testKey" , 1);
        assertIterableEquals(expectedList, result);
    }

    @Test
    public void testGetTimesList() {
        JSONObject testJSON = new JSONObject();

        List<OffsetDateTime> testList = new ArrayList<>();
        testList.add(OffsetDateTime.now());
        testList.add(OffsetDateTime.now());
        testList.add(OffsetDateTime.now());

        List<OffsetDateTime> testList1 = new ArrayList<>();
        testList1.add(OffsetDateTime.now());
        testList1.add(OffsetDateTime.now());

        testJSON.put("testKey", testList);
        testJSON.put("testKey1", testList1);

        // test times retrieved correctly
        List<OffsetDateTime> result = DataParser.getTimesList(testJSON, "testKey" );
        assertEquals(testList.size(), result.size());
        assertIterableEquals(testList, result);
    }

    @Test
    public void testCalculateAnnual() {
        List<String> iris = new ArrayList<>();
        String iri1 = "test_iri_1";
        String iri2 = "test_iri_2";
        iris.add(iri1);
        iris.add(iri2);

        Double value1 = 1.687;
        Double value2 = 2.141;
        Double value3 = 3.621;
        Double value4 = 4.7;

        List<List<?>> values = new ArrayList<>();
        List<Double> test_list_1 = new ArrayList<>();
        test_list_1.add(value1);
        test_list_1.add(value2);

        List<Double> test_list_2 = new ArrayList<>();
        test_list_2.add(value3);
        test_list_2.add(value4);

        values.add(test_list_1);
        values.add(test_list_2);

        List<OffsetDateTime> times = new ArrayList<>();
        times.add(OffsetDateTime.now());
        times.add(OffsetDateTime.now());
        TimeSeries<OffsetDateTime> timeSeries = new TimeSeries<>(times, iris, values);

        Double expected1 = 3.83;
        Double expected2 = 8.32;

        assertEquals(expected1.toString(), DataParser.calculateAnnual(timeSeries, iri1));
        assertEquals(expected2.toString(), DataParser.calculateAnnual(timeSeries, iri2));

    }
}
