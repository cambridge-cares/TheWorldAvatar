package uk.ac.cam.cares.downsampling;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import java.time.OffsetDateTime;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

/**
 * Downsampling class uses the downsampleTS method to downsample the timeseries data and retrieve the relevant dataIRI(s).
 * The downsampleTS method invokes the aggregation method to handle the downsampling based on the user-specified downsampling type and resolution.
 */

public class Downsampling {
    public enum Type{
        MAXIMUM,
        MEDIAN,
        MINIMUM,
        SUM,
        AVERAGE,
        COUNT,
        INSTANTANEOUS
    }
    /**
     * @param ts raw timeseries
     * @param resolution resolution - interval to be aggregated in seconds
     * @param type type of downsampling.
     *             Allowed values of Type enum: Type.MAXIMUM, Type.MEDIAN, Type.MINIMUM, Type.SUM, Type.AVERAGE, Type.COUNT, Type.INSTANTANEOUS
     * @return downsampled timeseries
     * @throws Exception
     */
    public static TimeSeries downsampleTS(TimeSeries ts, Long resolution, Type type) throws Exception {
        //Parsing timseries into list of list values and time list;
        List dataIRIs = ts.getDataIRIs();
        List<List<Double>> timeseriesLolValues = new ArrayList<>();

        for (Object dataIRI : dataIRIs) {
            timeseriesLolValues.add(ts.getValues(dataIRI.toString()));
        }
        List<OffsetDateTime> originalTimeList = ts.getTimes();

        List downsampledList;
        downsampledList = aggregation(originalTimeList, timeseriesLolValues, resolution, type);

        TimeSeries downsampledTS = new TimeSeries((List) downsampledList.get(0), (List<String>) dataIRIs, (List<List<?>>) downsampledList.get(1));

        return downsampledTS;
    }



    /**
     * @param originalTimeList raw timestamps list
     * @param originalValueLists raw nested lists of values
     * @param intervalInSeconds resolution - interval to be aggregated in seconds
     * @param type type of downsampling used to represent the aggregated time interval.
     *             ALlowed values:
     *             - MAXIMUM retrieves the maximum value of the points.
     *             - MEDIAN retrieves the median value of the points.
     *             - MINIMUM retrieves the minimum value of the points.
     *             - SUM retrieves the sum value of all points.
     *             - AVERAGE retrieves the average value of the points.
     *             - COUNT retrieves the total number of all points.
     *             - INSTANTANEOUS retrieves the value of the point closest to the time resolution.
     * @return a list which contains downsampled timestamps and downsampled values.
     * @throws Exception
     */
    public static List aggregation(List<OffsetDateTime> originalTimeList, List<List<Double>> originalValueLists, long intervalInSeconds, Type type) throws Exception {
        List<List<Double>> downsampledValueLists = new ArrayList<>();

        //Initialize size of downsampledValueLists for iterator
        for (int i = 0; i < originalValueLists.size(); i++) {
            downsampledValueLists.add(new ArrayList<>());
        }

        List<OffsetDateTime> downsampledTimeList = new ArrayList<>();

        OffsetDateTime startTime = originalTimeList.get(0).truncatedTo(ChronoUnit.SECONDS);
        OffsetDateTime endTime = originalTimeList.get(originalTimeList.size() - 1).truncatedTo(ChronoUnit.SECONDS);
        if(startTime==endTime){throw new Exception("The start time and end time is the same.");}

        for (OffsetDateTime currentTime = startTime; currentTime.isBefore(endTime); currentTime = currentTime.plusSeconds(intervalInSeconds)) {
            OffsetDateTime intervalEndTime = currentTime.plusSeconds(intervalInSeconds);

            Iterator<List<Double>> it1 = originalValueLists.iterator();
            Iterator<List<Double>> it2 = downsampledValueLists.iterator();

            //Max
            if(type.equals(Type.MAXIMUM)){
                while (it1.hasNext() && it2.hasNext()) {
                    List<Double> originalValueList = it1.next();
                    List<Double> downsampledValueList = it2.next();
                    double maxValue = Double.NEGATIVE_INFINITY;
                    for (int i = 0; i < originalTimeList.size(); i++) {
                        if (originalTimeList.get(i).isAfter(intervalEndTime)) {
                            break;
                        }
                        if (originalTimeList.get(i).isAfter(currentTime) && originalValueList.get(i) > maxValue) {
                            maxValue = originalValueList.get(i);
                        }
                    }
                    downsampledValueList.add(maxValue);
                }
            }
            //Median
            else if (type.equals(Type.MEDIAN)){
                    while (it1.hasNext() && it2.hasNext()) {
                    List<Double> originalValueList = it1.next();
                    List<Double> downsampledValueList = it2.next();
                    List<Double> valuesInInterval = new ArrayList<>();
                    for (int i = 0; i < originalTimeList.size(); i++) {
                        if (originalTimeList.get(i).isAfter(intervalEndTime)) {
                            break;
                        }
                        if (originalTimeList.get(i).isAfter(currentTime)) {
                            valuesInInterval.add(originalValueList.get(i));
                        }
                    }
                    if (!valuesInInterval.isEmpty()) {
                        Collections.sort(valuesInInterval);
                        double medianValue;
                        int size = valuesInInterval.size();
                        if (size % 2 == 0) {
                            int mid = size / 2;
                            medianValue = (valuesInInterval.get(mid - 1) + valuesInInterval.get(mid)) / 2;
                        } else {
                            medianValue = valuesInInterval.get(size / 2);
                        }
                        downsampledValueList.add(medianValue);
                    } else {
                        downsampledValueList.add(null);
                    }
                }

            }

            //Min
            else if (type.equals(Type.MINIMUM) ){
                    while (it1.hasNext() && it2.hasNext()) {
                    List<Double> originalValueList = it1.next();
                    List<Double> downsampledValueList = it2.next();
                    double minValue = Double.POSITIVE_INFINITY;  // initialize minValue to positive infinity
                    for (int i = 0; i < originalTimeList.size(); i++) {
                        if (originalTimeList.get(i).isAfter(intervalEndTime)) {
                            break;
                        }
                        if (originalTimeList.get(i).isAfter(currentTime) && originalValueList.get(i) < minValue) {
                            minValue = originalValueList.get(i);  // update minValue
                        }
                    }
                    if (minValue == Double.POSITIVE_INFINITY) {throw new Exception("Something went wrong here");}
                    downsampledValueList.add(minValue);  // add minValue to the downsampledValueList

                    }
                }

            //Sum
            else if (type.equals(Type.SUM)){
                while (it1.hasNext() && it2.hasNext()) {
                    List<Double> originalValueList = it1.next();
                    List<Double> downsampledValueList = it2.next();
                    double sum = 0;
                    for (int i = 0; i < originalTimeList.size(); i++) {
                        if (originalTimeList.get(i).isAfter(intervalEndTime)) {
                            break;
                        }

                        if (originalTimeList.get(i).isAfter(currentTime)){
                        sum += originalValueList.get(i);
                        }
                    }
                    downsampledValueList.add(sum);
                }
            }

            //Average
            else if (type.equals(Type.AVERAGE)){
                while (it1.hasNext() && it2.hasNext()) {
                    List<Double> originalValueList = it1.next();
                    List<Double> downsampledValueList = it2.next();
                    double sum = 0;
                    int count = 0;
                    for (int i = 0; i < originalTimeList.size(); i++) {
                        if (originalTimeList.get(i).isAfter(intervalEndTime)) {
                            break;
                        }
                        if (originalTimeList.get(i).isAfter(currentTime)){
                            sum += originalValueList.get(i);
                            count++;
                        }
                    }
                    downsampledValueList.add(sum/count);
                }
            }
            //Count
            else if (type.equals(Type.COUNT)){
                while (it1.hasNext() && it2.hasNext()) {
                    List<Double> originalValueList = it1.next();
                    List<Double> downsampledValueList = it2.next();
                    int count = 0;
                    for (int i = 0; i < originalTimeList.size(); i++) {
                        if (originalTimeList.get(i).isAfter(intervalEndTime)) {
                            break;
                        }
                        if (originalTimeList.get(i).isAfter(currentTime)){
                            count++;
                        }
                    }
                    downsampledValueList.add((double) count);
                }
            }

            //Instantaneous
            else if (type.equals(Type.INSTANTANEOUS)) {

                while (it1.hasNext() && it2.hasNext()) {
                    List<Double> originalValueList = it1.next();
                    List<Double> downsampledValueList = it2.next();
                    double closestValue = originalValueList.get(0) ;
                    for (int j = 0; j < originalTimeList.size()-1; j++) {
                        if (originalTimeList.get(j).isAfter(intervalEndTime)) {
                            break;
                        }
                        //Reached the last one
                        if (j==originalTimeList.size()-1) {
                            closestValue = originalValueList.get(j);
                            break;
                        }

                        if (Math.abs(originalTimeList.get(j).toEpochSecond() - intervalEndTime.toEpochSecond())
                                < Math.abs(originalTimeList.get(j+1).toEpochSecond() - intervalEndTime.toEpochSecond())) {
                            closestValue = originalValueList.get(j);
                        }else if (Math.abs(originalTimeList.get(j).toEpochSecond() - intervalEndTime.toEpochSecond())
                                == Math.abs(originalTimeList.get(j+1).toEpochSecond() - intervalEndTime.toEpochSecond())) {
                            closestValue = originalValueList.get(j);
                        }else {
                            closestValue = originalValueList.get(j+1);
                        }
                    }
                    downsampledValueList.add(closestValue);
                }
            }
            downsampledTimeList.add(currentTime);
        }

        List result = new ArrayList();
        result.add(downsampledTimeList);
        result.add(downsampledValueLists);

        return result;
    }
}
