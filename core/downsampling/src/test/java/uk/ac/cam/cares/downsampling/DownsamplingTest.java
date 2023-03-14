package uk.ac.cam.cares.downsampling;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import static org.junit.Assert.*;
import org.testng.annotations.Test;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;

/**
 * This DownsamplingTest class tests all the methods in Downsampling class.
 */
public class DownsamplingTest {
    Downsampling downsampling =new Downsampling();
    private static TimeSeries ts;
    private static String randomDATAIRI1="randomDATAIRI1";
    private static String randomDATAIRI2="randomDATAIRI2";

    /**
     * Declare sample dataset and parse into timeseries.
     * @return sample timeseries
     */
    private static TimeSeries getDataset(int dataset) {

        List<List<?>> lolvalues = new ArrayList<>();
        String randomDATAIRI1 = "randomDATAIRI1";
        String randomDATAIRI2 = "randomDATAIRI2";
        List<String> dataIRIlist = new ArrayList<>();

        dataIRIlist.add(randomDATAIRI1);
        dataIRIlist.add(randomDATAIRI2);

        List<OffsetDateTime> col1 = null;
        List<Double> col2 = null;
        List<Double> col3 = null;
        if (dataset == 1) {

            col1 = Arrays.asList(
                    OffsetDateTime.parse("2023-02-17T11:13:42.775012200+08:00"),
                    OffsetDateTime.parse("2023-02-17T11:13:43.775012200+08:00"),
                    OffsetDateTime.parse("2023-02-17T11:13:44.775012200+08:00"),
                    OffsetDateTime.parse("2023-02-17T11:13:45.775012200+08:00"),
                    OffsetDateTime.parse("2023-02-17T11:13:46.775012200+08:00"),
                    OffsetDateTime.parse("2023-02-17T11:13:47.775012200+08:00"),
                    OffsetDateTime.parse("2023-02-17T11:13:48.775012200+08:00"),
                    OffsetDateTime.parse("2023-02-17T11:13:49.775012200+08:00"),
                    OffsetDateTime.parse("2023-02-17T11:13:50.775012200+08:00"),
                    OffsetDateTime.parse("2023-02-17T11:13:51.775012200+08:00"),
                    OffsetDateTime.parse("2023-02-17T11:13:52.775012200+08:00"),
                    OffsetDateTime.parse("2023-02-17T11:13:53.775012200+08:00"),
                    OffsetDateTime.parse("2023-02-17T11:13:54.775012200+08:00"),
                    OffsetDateTime.parse("2023-02-17T11:13:55.775012200+08:00"),
                    OffsetDateTime.parse("2023-02-17T11:13:56.775012200+08:00")
            );
            col2 = Arrays.asList(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0);
            col3 = Arrays.asList(15.0, 14.0, 13.0, 12.0, 11.0, 10.0, 9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0);
        } else if (dataset == 2) {
            col1 = Arrays.asList(
                    OffsetDateTime.parse("2023-02-17T11:13:42.000000000+08:00"),
                    OffsetDateTime.parse("2023-02-17T11:13:43.000000000+08:00"),
                    OffsetDateTime.parse("2023-02-17T11:13:44.000000000+08:00"),
                    OffsetDateTime.parse("2023-02-17T11:13:45.500000000+08:00"),
                    OffsetDateTime.parse("2023-02-17T11:13:46.000000000+08:00"),
                    OffsetDateTime.parse("2023-02-17T11:13:47.000000000+08:00"),
                    OffsetDateTime.parse("2023-02-17T11:13:48.000000000+08:00"),
                    OffsetDateTime.parse("2023-02-17T11:13:49.000000000+08:00"),
                    OffsetDateTime.parse("2023-02-17T11:13:50.000000000+08:00"),
                    OffsetDateTime.parse("2023-02-17T11:13:51.100000000+08:00"),
                    OffsetDateTime.parse("2023-02-17T11:13:52.000000000+08:00"),
                    OffsetDateTime.parse("2023-02-17T11:13:53.000000000+08:00"),
                    OffsetDateTime.parse("2023-02-17T11:13:54.000000000+08:00"),
                    OffsetDateTime.parse("2023-02-17T11:13:55.000000000+08:00"),
                    OffsetDateTime.parse("2023-02-17T11:13:56.000000000+08:00")
            );
            col2 = Arrays.asList(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0);
            col3 = Arrays.asList(15.0, 14.0, 13.0, 12.0, 11.0, 10.0, 9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0);
        }
        lolvalues.add(col2);
        lolvalues.add(col3);
        ts = new TimeSeries(col1, dataIRIlist, lolvalues);
        return ts;
    }

    @Test
    public void aggregationMethodMaxTest () throws Exception {
        ts= DownsamplingTest.getDataset(1);
        TimeSeries resampledTS= downsampling.aggregation(ts,5L, Downsampling.Type.MAXIMUM);
        List List1=resampledTS.getValues(randomDATAIRI1);
        List List2=resampledTS.getValues(randomDATAIRI2);
        assertEquals((double) 5,List1.get(0));
        assertEquals((double) 10,List1.get(1));
        assertEquals((double) 15,List1.get(2));

        assertEquals((double) 15,List2.get(0));
        assertEquals((double) 10,List2.get(1));
        assertEquals((double) 5,List2.get(2));
    }
    @Test
    public void aggregationMethodMedianTest () throws Exception {
        ts= DownsamplingTest.getDataset(1);
        TimeSeries resampledTS= downsampling.aggregation(ts,5L,Downsampling.Type.MEDIAN);

        List List1=resampledTS.getValues(randomDATAIRI1);
        List List2=resampledTS.getValues(randomDATAIRI2);

        assertEquals((double) 3,List1.get(0));
        assertEquals((double) 8,List1.get(1));
        assertEquals((double) 13,List1.get(2));

        assertEquals((double) 13,List2.get(0));
        assertEquals((double) 8,List2.get(1));
        assertEquals((double) 3,List2.get(2));
    }
    @Test
    public void aggregationMethodMinTest () throws Exception {
        ts= DownsamplingTest.getDataset(1);
        TimeSeries resampledTS= downsampling.aggregation(ts,5L,Downsampling.Type.MINIMUM);

        List List1=resampledTS.getValues(randomDATAIRI1);
        List List2=resampledTS.getValues(randomDATAIRI2);

        assertEquals((double) 1,List1.get(0));
        assertEquals((double) 6,List1.get(1));
        assertEquals((double) 11,List1.get(2));

        assertEquals((double) 11,List2.get(0));
        assertEquals((double) 6,List2.get(1));
        assertEquals((double) 1,List2.get(2));
    }
    @Test
    public void aggregationMethodSumTest () throws Exception {
        ts= DownsamplingTest.getDataset(1);
        TimeSeries resampledTS= downsampling.aggregation(ts,5L,Downsampling.Type.SUM);

        List List1=resampledTS.getValues(randomDATAIRI1);
        List List2=resampledTS.getValues(randomDATAIRI2);


        assertEquals((double) 15,List1.get(0));
        assertEquals((double) 40,List1.get(1));
        assertEquals((double) 65,List1.get(2));

        assertEquals((double) 65,List2.get(0));
        assertEquals((double) 40,List2.get(1));
        assertEquals((double) 15,List2.get(2));
    }
    @Test
    public void aggregationMethodAverageTest () throws Exception {
        ts= DownsamplingTest.getDataset(1);
        TimeSeries resampledTS= downsampling.aggregation(ts,5L,Downsampling.Type.AVERAGE);

        List List1=resampledTS.getValues(randomDATAIRI1);
        List List2=resampledTS.getValues(randomDATAIRI2);

        assertEquals((double) 3,List1.get(0));
        assertEquals((double) 8,List1.get(1));
        assertEquals((double) 13,List1.get(2));

        assertEquals((double) 13,List2.get(0));
        assertEquals((double) 8,List2.get(1));
        assertEquals((double) 3,List2.get(2));
    }

    @Test
    public void aggregationCountTest () throws Exception {
        ts= DownsamplingTest.getDataset(1);
        TimeSeries resampledTS= downsampling.aggregation(ts,7L,Downsampling.Type.COUNT);

        List List1=resampledTS.getValues(randomDATAIRI1);

        assertEquals((double) 7,List1.get(0));
        assertEquals((double) 7,List1.get(1));
        //The 15th point out of bound
        try{
            assertEquals((double) 7,List1.get(2));
        }catch (Exception e) {
            assertEquals(IndexOutOfBoundsException.class, e.getClass());
        }

        List List2=resampledTS.getValues(randomDATAIRI2);
        assertEquals((double) 7,List2.get(0));
        assertEquals((double) 7,List2.get(1));

        //The 15th point out of bound
        try{
            assertEquals((double) 7,List2.get(2));
        }catch (Exception e) {
            assertEquals(IndexOutOfBoundsException.class, e.getClass());
        }
    }

    @Test
    public void instantaneousTest () throws Exception {
        ts= DownsamplingTest.getDataset(2);
        TimeSeries resampledTS= downsampling.aggregation(ts,5L,Downsampling.Type.INSTANTANEOUS);

        List List1=resampledTS.getValues(randomDATAIRI1);

        assertEquals((double) 6,List1.get(0));
        assertEquals((double) 11,List1.get(1));
        assertEquals((double) 15,List1.get(2));

        TimeSeries resampledTS2= downsampling.aggregation(ts,3L,Downsampling.Type.INSTANTANEOUS);

        List List1a=resampledTS2.getValues(randomDATAIRI1);
        assertEquals((double) 4,List1a.get(0));
        assertEquals((double) 7,List1a.get(1));
        assertEquals((double) 10,List1a.get(2));
    }
}







