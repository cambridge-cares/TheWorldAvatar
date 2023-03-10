package uk.ac.cam.cares.downsampling;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import java.time.OffsetDateTime;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

/**
 * This Downsampling class parses Timeseries using "aggregation" method, retrieving the relevant Timeseries data and dataIRIs.
 * "aggregationMethod" is then implemented to downsample the timeseries data based on the downsampling resolution and downsampling type.
 * The resampled data will then be parsed into a Timeseries class and returned.
 */

public class Downsampling {
    /**
     * @param ts Raw timeseries
     * @param resolution Resolution - interval to be aggregated in seconds
     * @param type Downsampling type
     * @return Resampled timeseries
     * @throws Exception
     */
    public static TimeSeries aggregation(TimeSeries ts, Long resolution, int type) throws Exception {
        //Parsing timseries into list of list values and time list;
        List TSDataIRIS = ts.getDataIRIs();
        List<List<Double>> TSlolValues = new ArrayList<>();

        for (Object TSDataIRI : TSDataIRIS) {
            TSlolValues.add(ts.getValues(TSDataIRI.toString()));
        }
        List<OffsetDateTime> originalTimeList = ts.getTimes();

        List ResampledList;
        ResampledList = aggregationMethod(originalTimeList, TSlolValues, resolution,type);

        TimeSeries resampledTS = new TimeSeries((List) ResampledList.get(0), (List<String>) TSDataIRIS, (List<List<?>>) ResampledList.get(1));

        return resampledTS;
    }

    /**
     * @param originalTimeList Raw timestamp list
     * @param originalValueLists Raw list of list of values
     * @param intervalInSeconds Resolution - interval to be aggregated in seconds
     * @param type Downsampling type
     * @return A list which contains a list of resampled timestamps and a resampled list of list of values.
     * @throws Exception
     */
    public static List aggregationMethod(List<OffsetDateTime> originalTimeList, List<List<Double>> originalValueLists, long intervalInSeconds, int type) throws Exception {
        List<List<Double>> resampledValueLists = new ArrayList<>();

        //Initiliaze size of resampledValueLists for iterator purpose
        for (int i = 0; i < originalValueLists.size(); i++) {
            resampledValueLists.add(new ArrayList<>());
        }

        List<OffsetDateTime> resampledTimeList = new ArrayList<>();

        OffsetDateTime startTime = originalTimeList.get(0).truncatedTo(ChronoUnit.SECONDS);
        OffsetDateTime endTime = originalTimeList.get(originalTimeList.size() - 1).truncatedTo(ChronoUnit.SECONDS);
        if(startTime==endTime){throw new Exception("The start time and end time is the same.");}

        for (OffsetDateTime currentTime = startTime; currentTime.isBefore(endTime); currentTime = currentTime.plusSeconds(intervalInSeconds)) {
            OffsetDateTime intervalEndTime = currentTime.plusSeconds(intervalInSeconds);

            Iterator<List<Double>> it1 = originalValueLists.iterator();
            Iterator<List<Double>> it2 = resampledValueLists.iterator();

            //Max
            if(type == 1){
                while (it1.hasNext() && it2.hasNext()) {
                    List<Double> originalValueList = it1.next();
                    List<Double> resampledValueList = it2.next();
                    double maxValue = Double.NEGATIVE_INFINITY;
                    for (int i = 0; i < originalTimeList.size(); i++) {
                        if (originalTimeList.get(i).isAfter(intervalEndTime)) {
                            break;
                        }
                        if (originalTimeList.get(i).isAfter(currentTime) && originalValueList.get(i) > maxValue) {
                            maxValue = originalValueList.get(i);
                        }
                    }
                    resampledValueList.add(maxValue);
                }
            }
            //Median
            else if (type == 2){
                    while (it1.hasNext() && it2.hasNext()) {
                    List<Double> originalValueList = it1.next();
                    List<Double> resampledValueList = it2.next();
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
                        resampledValueList.add(medianValue);
                    } else {
                        resampledValueList.add(null);
                    }
                }

            }

            //Min
            else if (type==3 ){
                    while (it1.hasNext() && it2.hasNext()) {
                    List<Double> originalValueList = it1.next();
                    List<Double> resampledValueList = it2.next();
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
                    resampledValueList.add(minValue);  // add minValue to the resampledValueList

                    }
                }

            //Sum
            else if (type==4){
                while (it1.hasNext() && it2.hasNext()) {
                    List<Double> originalValueList = it1.next();
                    List<Double> resampledValueList = it2.next();
                    double sum = 0;
                    for (int i = 0; i < originalTimeList.size(); i++) {
                        if (originalTimeList.get(i).isAfter(intervalEndTime)) {
                            break;
                        }

                        if (originalTimeList.get(i).isAfter(currentTime)){
                        sum += originalValueList.get(i);
                        }
                    }
                    resampledValueList.add(sum);
                }
            }

            //Average
            else if (type==5){
                while (it1.hasNext() && it2.hasNext()) {
                    List<Double> originalValueList = it1.next();
                    List<Double> resampledValueList = it2.next();
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
                    resampledValueList.add(sum/count);
                }
            }
            //Count
            else if (type==6){
                while (it1.hasNext() && it2.hasNext()) {
                    List<Double> originalValueList = it1.next();
                    List<Double> resampledValueList = it2.next();
                    int count = 0;
                    for (int i = 0; i < originalTimeList.size(); i++) {
                        if (originalTimeList.get(i).isAfter(intervalEndTime)) {
                            break;
                        }
                        if (originalTimeList.get(i).isAfter(currentTime)){
                            count++;
                        }
                    }
                    resampledValueList.add((double) count);
                }
            }

            //Instantaneous
            else if (type == 7) {

                while (it1.hasNext() && it2.hasNext()) {
                    List<Double> originalValueList = it1.next();
                    List<Double> resampledValueList = it2.next();
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
                            //
                            closestValue = originalValueList.get(j);
                        }else if (Math.abs(originalTimeList.get(j).toEpochSecond() - intervalEndTime.toEpochSecond())
                                == Math.abs(originalTimeList.get(j+1).toEpochSecond() - intervalEndTime.toEpochSecond())) {
                            closestValue = originalValueList.get(j);
                        }else {
                            closestValue = originalValueList.get(j+1);
                        }
                    }
                    resampledValueList.add(closestValue);
                }
            }
            resampledTimeList.add(currentTime);
        }

        List result = new ArrayList();
        result.add(resampledTimeList);
        result.add(resampledValueLists);

        return result;
    }
}
