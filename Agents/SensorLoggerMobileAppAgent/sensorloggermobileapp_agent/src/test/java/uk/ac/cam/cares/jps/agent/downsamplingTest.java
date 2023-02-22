package uk.ac.cam.cares.jps.agent;

import java.io.*;
import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.*;
import org.testng.annotations.Test;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;

import com.opencsv.CSVReader;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.downsampling;

public class downsamplingTest {
    downsampling downsampling =new downsampling();

    private static TimeSeries ts;
    // Create 3 separate lists for the 3 columns
    private static List<OffsetDateTime> column1 = new ArrayList<>();
    private static List<Double> column2 = new ArrayList<>();
    private static List<Double> column3 = new ArrayList<>();
    private static List<List<?>> lolvalues= new ArrayList<>();
    private static String randomDATAIRI1="randomDATAIRI1";
    private static String randomDATAIRI2="randomDATAIRI2";
    private static List <String> dataIRIlist= new ArrayList<>();

    /**
     * Create sample timseries object through parsing a sample CSV data
     * @return
     */
    private static TimeSeries getCSV(String csvStringName){
        // Get the path of the CSV file in the resources folder
        String filePath = downsamplingTest.class.getClassLoader().getResource(csvStringName).getPath();


        // Create a CSVReader object
        CSVReader reader = null;
        try {
            reader = new CSVReader(new FileReader(filePath));
        } catch (FileNotFoundException e) {
            throw new RuntimeException(e);
        }

        // Read all the data from the CSV file into a list of String arrays
        List<String[]> data = null;
        try {
            data = reader.readAll();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }


        dataIRIlist.add(randomDATAIRI1);
        dataIRIlist.add(randomDATAIRI2);

        // Create a DateTimeFormatter for the OffsetDateTime column
        DateTimeFormatter formatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME;

        // Loop through the data and populate the 3 lists
        for (String[] row : data) {
            String dateTimeStr = row[0].replaceAll("\\P{Print}", "");
            column1.add(OffsetDateTime.parse(dateTimeStr, formatter));
            column2.add(Double.parseDouble(row[1]));
            column3.add(Double.parseDouble(row[2]));
        }
        lolvalues.add(column2);
        lolvalues.add(column3);

        // Close the reader
        try {
            reader.close();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        ts= new TimeSeries(column1, dataIRIlist, lolvalues);
        return ts;
    }

    @Test
    private void aggregationMethodMaxTest () throws Exception {
        ts= downsamplingTest.getCSV("sampleDataSet.csv");
        TimeSeries resampledTS= downsampling.aggregation(ts,5L,1);
        List List1=resampledTS.getValues(randomDATAIRI1);
        List List2=resampledTS.getValues(randomDATAIRI2);
        assertEquals((double) 5,List1.get(0));
        assertEquals((double) 10,List1.get(1));
        assertEquals((double) 15,List1.get(2));
    }

    @Test
    private void aggregationMethodMedianTest () throws Exception {
        ts= downsamplingTest.getCSV("sampleDataSet.csv");
        TimeSeries resampledTS= downsampling.aggregation(ts,5L,2);

        List List1=resampledTS.getValues(randomDATAIRI1);

        assertEquals((double) 3,List1.get(0));
        assertEquals((double) 8,List1.get(1));
        assertEquals((double) 13,List1.get(2));
    }

    @Test
    private void aggregationMethodMinTest () throws Exception {
        ts= downsamplingTest.getCSV("sampleDataSet.csv");
        TimeSeries resampledTS= downsampling.aggregation(ts,5L,3);

        List List1=resampledTS.getValues(randomDATAIRI1);

        assertEquals((double) 1,List1.get(0));
        assertEquals((double) 6,List1.get(1));
        assertEquals((double) 11,List1.get(2));
    }

//    @Test
//    private void aggregationMethodSumTest () throws Exception {
//        ts= downsamplingTest.getCSV("sampleDataSet.csv");
//        TimeSeries resampledTS= downsampling.aggregation(ts,5L,4);
//
//        List List1=resampledTS.getValues(randomDATAIRI1);
//
//        assertEquals((double) 15,List1.get(0));
//        assertEquals((double) 40,List1.get(1));
//        assertEquals((double) 65,List1.get(2));
//    }

    @Test
    private void aggregationMethodAverageTest () throws Exception {
        ts= downsamplingTest.getCSV("sampleDataSet.csv");
        TimeSeries resampledTS= downsampling.aggregation(ts,5L,5);

        List List1=resampledTS.getValues(randomDATAIRI1);

        assertEquals((double) 3,List1.get(0));
        assertEquals((double) 8,List1.get(1));
        assertEquals((double) 13,List1.get(2));
    }

    @Test
    private void aggregationCountTest () throws Exception {
        ts= downsamplingTest.getCSV("sampleDataSet.csv");
        TimeSeries resampledTS= downsampling.aggregation(ts,7L,6);

        List List1=resampledTS.getValues(randomDATAIRI1);

        assertEquals((double) 7,List1.get(0));
        assertEquals((double) 7,List1.get(1));
        //The 15th point out of bound
        try{
            assertEquals((double) 7,List1.get(2));
        }catch (Exception e) {
            assertEquals(IndexOutOfBoundsException.class, e.getClass());
        }
    }

//    @Test
//    private void instantaneousTest () throws Exception {
//        ts= downsamplingTest.getCSV("sampleDataSet2.csv");
//        TimeSeries resampledTS= downsampling.aggregation(ts,5L,7);
//
//        List List1=resampledTS.getValues(randomDATAIRI1);
//
//        assertEquals((double) 5.0000001,List1.get(0));
//        assertEquals((double) 10.05,List1.get(1));
//        assertEquals((double) 15,List1.get(2));
//    }

}







