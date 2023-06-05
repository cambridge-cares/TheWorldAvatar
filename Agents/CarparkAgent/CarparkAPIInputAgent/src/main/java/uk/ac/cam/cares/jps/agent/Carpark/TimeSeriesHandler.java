package uk.ac.cam.cares.jps.agent.Carpark;
import org.json.JSONArray;
import org.json.JSONObject;
import org.jooq.exception.DataAccessException;
import uk.ac.cam.cares.jps.base.util.JSONKeyToIRIMapper;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;

import java.text.SimpleDateFormat;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.stream.Collectors;
import java.util.zip.GZIPInputStream;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;


public class TimeSeriesHandler
{
    public static final Logger Log = LogManager.getLogger(CarparkAgent.class);
    private TimeSeriesClient<OffsetDateTime> tsclient;
    private List<JSONKeyToIRIMapper> mappings;
    public static final String generatedIRIPrefix = TimeSeriesSparql.TIMESERIES_NAMESPACE + "Carpark";
    public static final String timeUnit = OffsetDateTime.class.getSimpleName();
    public static final String timestampKey = "time";
    //public static final String status = "qcstatus";
    public static final ZoneOffset ZONE_OFFSET = ZoneOffset.UTC;
 


    public TimeSeriesHandler(String propertiesFile) throws IOException
    {

        try(InputStream input = new FileInputStream(propertiesFile))
        {
            Properties prop = new Properties();
            prop.load(input);
            String mappingFolder;

            try
            {
                mappingFolder = System.getenv(prop.getProperty("Carpark.mappingfolder")); 
                //to substitute for values i need;
            }
            catch(NullPointerException e)
            {
                throw new IOException("The key Carpark.mappingfolder cannot be found in the file");

            }

            if(mappingFolder==null)
            {
                throw new InvalidPropertiesFormatException("The properties file does not contain the key Carpark.mappingfolder with a path to the folder containing the required JSON key to IRI Mappings");
            }

            readmappings(mappingFolder);
        }

    }

    public int getNumberofTimeSeries()
    {
        return mappings.size();
    }

    public void setTsClient(TimeSeriesClient<OffsetDateTime> tsclient)
    {
        this.tsclient = tsclient;
    }

    private void readmappings(String mappingfolder) throws IOException
    {
        mappings = new ArrayList<>();
        File folder = new File(mappingfolder);
        File[] mappingFiles = folder.listFiles();

        if(mappingFiles==null)
        {
            throw new IOException("Folder does not exist: " + mappingfolder);
        }
        if(mappingFiles.length==0)
        {
            throw new IOException("No files in folder");

        }
        else
        {
            for( File mappingFile: mappingFiles)
            {
                JSONKeyToIRIMapper mapper = new JSONKeyToIRIMapper(TimeSeriesHandler.generatedIRIPrefix, mappingFile.getAbsolutePath());
                mappings.add(mapper);
                mapper.saveToFile(mappingFile.getAbsolutePath());
            }
        }
    }

    public void initializeTimeSeriesIfNotExist()
    {
        for(JSONKeyToIRIMapper mapping:mappings)
        {
            List<String> iris = mapping.getAllIRIs();
            if(!timeSeriesExist(iris))
            {
                List<Class<?>> classes = iris.stream().map(this::getClassFromJSONKey).collect(Collectors.toList());
                // TO clarify later on Google.
                
                try
                {
                    tsclient.initTimeSeries(iris,classes,timeUnit);
                    Log.info(String.format("Initialized time series with the following IRIs: %s", String.join(", ", iris)));
                
                }
                catch(Exception e)
                {
                    throw new JPSRuntimeException("Could not instantiate TimeSeries");
                }
            }
        }
    }
    private boolean timeSeriesExist(List<String> iris)
    {
        for (String iri:iris)
        {
            try
            {
                if(!tsclient.checkDataHasTimeSeries(iri))
                {
                    return false;
                }
            }
            catch(DataAccessException e)
            {
                if (e.getMessage().contains("ERROR: relation \"dbTable\" does not exist")) 
                {
                    return false;
                }
                else 
                {
                    throw e;
                }
            }
        }
        return true;
    }

    public void updateData(JSONObject carparkReadings) throws IllegalArgumentException
    {
        Map <String, List<?>> carparkReadingsMap = new HashMap<>();
        try
        {
            carparkReadingsMap = jsonObjectToMap(carparkReadings);
        }
        catch (Exception e) 
        {
            throw new JPSRuntimeException (e.toString());
        }


        if(!carparkReadings.isEmpty())
        {
            List<TimeSeries<OffsetDateTime>> timeSeries;
            try
            {
                timeSeries = convertReadingsToTimeSeries(carparkReadingsMap);
            }
            catch (NoSuchElementException e)    
            {
                throw new IllegalArgumentException("Readings cannot be converted to ProperTimeSeries",e);
            }
            for (TimeSeries<OffsetDateTime> ts : timeSeries) 
            {
                // Retrieve current maximum time to avoid duplicate entries (can be null if no data is in the database yet)
                OffsetDateTime endDataTime;
                try 
                 {
                	endDataTime= tsclient.getMaxTime(ts.getDataIRIs().get(0));      
                 } 
                 catch (Exception e) 
                 {
                	throw new JPSRuntimeException("Could not get max time!");
                 }
                 
                OffsetDateTime startCurrentTime = ts.getTimes().get(0);
                // If there is already a maximum time
                if (endDataTime != null) 
                {
                    // If the new data overlaps with existing timestamps, prune the new ones
                    if (startCurrentTime.isBefore(endDataTime))
                        ts = pruneTimeSeries(ts, endDataTime);
                }
                // Only update if there actually is data
                if (!ts.getTimes().isEmpty()) 
                {
                	try 
                    {
                      tsclient.addTimeSeriesData(ts);
                      Log.debug(String.format("Time series updated for following IRIs: %s", String.join(", ", ts.getDataIRIs())));
                    }
                    catch (Exception e)
                    {
                	   throw new JPSRuntimeException("Could not add timeseries!");
                    } 
                }
            }
        }
        else 
        {
            throw new IllegalArgumentException("Readings can not be empty!");
        }
    }

    private Map<String, List<?>> jsonObjectToMap(JSONObject readings) {

        // First save the values as Object //
        
        Map<String, List<?>> readingsMap = new HashMap<>();
        
        Map<String, List<Object>> firstMap = new HashMap<>();
        
        JSONArray jsArr;
        try {
            
            jsArr = readings.getJSONArray("value");
            for(int i=0; i<jsArr.length();i++)
            {
                JSONObject currentEntry = jsArr.getJSONObject(i);
                
                String key1 = "AvailableLots";
                Object value = currentEntry.get(key1);
                String key = key1+"_"+currentEntry.get("CarParkID")+"_"+currentEntry.get("LotType");
                firstMap.put(key,new ArrayList<>());

                firstMap.get(key).add(value);

                List<Object> valuesUntyped = firstMap.get(key);
                List<?> valuesTyped = valuesUntyped.stream().map(x -> ((Number) x).intValue()).collect(Collectors.toList());

                readingsMap.put(key,valuesTyped);
            }
            long timestamp = System.currentTimeMillis();
            Date date = new java.util.Date(timestamp);
            SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
            sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
            
            Object ts = sdf.format(date);

            String k = "time";
            firstMap.put(k,new ArrayList<>());
            firstMap.get(k).add(ts);

            List<Object> valueUntyped = firstMap.get(k);
            List<?> valueTyped = valueUntyped.stream().map(Object::toString).collect(Collectors.toList());

            readingsMap.put(k,valueTyped);
            
        } catch (Exception e) {
            throw new JPSRuntimeException("Readings can not be empty!", e);
        }   
      
        return readingsMap;

    }


    private List<TimeSeries<OffsetDateTime>> convertReadingsToTimeSeries(Map<String, List<?>> carparkReadings)
    throws  NoSuchElementException 
    {
       // Extract the timestamps by mapping the private conversion method on the list items
       // that are supposed to be string (toString() is necessary as the map contains lists of different types)

       List<OffsetDateTime> carparkTimestamps = carparkReadings.get(TimeSeriesHandler.timestampKey).stream().map(timestamp -> (convertStringToOffsetDateTime(timestamp.toString()))).collect(Collectors.toList());
       try{
        // Construct a time series object for each mapping
        List<TimeSeries<OffsetDateTime>> timeSeries = new ArrayList<>();
        for (JSONKeyToIRIMapper mapping: mappings)
        {
            // Initialize the list of IRIs
            List<String> iris = new ArrayList<>();
            // Initialize the list of list of values
            List<List<?>> values = new ArrayList<>();
            List<Double> NaN = new ArrayList<>();
            NaN.add(Double.NaN);
            List<?> NaNTyped = NaN.stream().map(x -> ((Number) x).doubleValue()).collect(Collectors.toList());
            for(String key: mapping.getAllJSONKeys()) 
            {
                // Add IRI
                iris.add(mapping.getIRI(key));
                if (carparkReadings.containsKey(key)) 
                {
                    values.add(carparkReadings.get(key));
                }
                else 
                {
                    values.add(NaNTyped);
                }
            }  
          List<OffsetDateTime> times = carparkTimestamps;
          // Create the time series object and add it to the list
   
         TimeSeries<OffsetDateTime> currentTimeSeries = new TimeSeries<>(times, iris, values);
         timeSeries.add(currentTimeSeries);
        }

     return timeSeries;
    } catch (Exception e) {
        throw new JPSRuntimeException("Readings cannot be converted to Proper TimeSeries.");
      }
   }
   
   private OffsetDateTime convertStringToOffsetDateTime(String timestamp)  
   {
 // Convert first to a local time
 LocalDateTime localTime = LocalDateTime.parse(timestamp);
 // Then add the zone id
 return OffsetDateTime.of(localTime, ZoneOffset.UTC) ;
   }






   private TimeSeries<OffsetDateTime> pruneTimeSeries(TimeSeries<OffsetDateTime> timeSeries, OffsetDateTime timeThreshold) 
   {
     // Find the index from which to start
     List<OffsetDateTime> times = timeSeries.getTimes();
     int index = 0;
     while(index < times.size()) 
     {
        if (times.get(index).isAfter(timeThreshold)) 
        {
            break;
        }
        index++;
     }
     // Prune timestamps
     List<OffsetDateTime> newTimes = new ArrayList<>();
     // There are timestamps above the threshold
     if (index != times.size()) 
      {
         // Prune the times
         newTimes = new ArrayList<>(times.subList(index, times.size()));
      }
     // Prune data
     List<List<?>> newValues = new ArrayList<>();
     // Prune the values
     for (String iri: timeSeries.getDataIRIs())
       {
         // There are timestamps above the threshold
         if (index != times.size()) 
          {
             newValues.add(timeSeries.getValues(iri).subList(index, times.size()));
          }
         else 
          {
             newValues.add(new ArrayList<>());
          }
       }
     return new TimeSeries<>(newTimes, timeSeries.getDataIRIs(), newValues); 
    }





    private Class<?> getClassFromJSONKey(String jsonKey) 
    {
         if (jsonKey.contains("relative_humiditylow") || jsonKey.contains("relative_humidityhigh") || jsonKey.contains("temperaturelow") || jsonKey.contains("temperaturehigh")||jsonKey.contains("windspeedlow")||jsonKey.contains("windspeedhigh"))
         {
             return Double.class;
         }
        else
         {
            return String.class;
         }
    }


}
