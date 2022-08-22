package uk.ac.cam.cares.jps.agent.historicalpirmasens;


import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.*;
import java.util.Properties;
import java.util.Scanner;
/**
 * Class that retrieves data values frome csv files
 * @author  */
public class HistoricalPirmasensStationCSVConnector{
    private String numOfKeys;
	JSONArray dataJSONArray;
	JSONObject dataReadings;
	String csvFilePath;
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(HistoricalPirmasensStationAgentLauncher.class);
    private static final String DATA_ERROR_MSG = "Data readings could not be retrieved";

    /**
     * Standard constructor
     * @param filePath The file path for the CSV file
     * @param numOfKeysFilePath The file path for csvconnector.properties
     */
    public HistoricalPirmasensStationCSVConnector(String filePath, String numOfKeysFilePath) throws IOException {
        this.csvFilePath = filePath;
    	File file = new File(numOfKeysFilePath);
        if (!file.exists()) {
            throw new FileNotFoundException("No properties file found at specified filepath: " + numOfKeysFilePath);
        }
        try (InputStream input = new FileInputStream(file)) {

            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);

           
            if (prop.containsKey("numOfKeys")) {
                this.numOfKeys = prop.getProperty("numOfKeys");
            } else {
                throw new IOException("Properties file is missing \"numOfKeys=<numOfKeys>\"");
            }
        }
    }
    
    /**
     * Get readings from the CSV file
     */
    public JSONObject getReadings() {
        try {
            return retrieveReadings(csvFilePath, Integer.valueOf(numOfKeys));
        }
        catch (IOException | JSONException e) {
            LOGGER.error(DATA_ERROR_MSG, e);
            throw new JPSRuntimeException(DATA_ERROR_MSG, e);
        }
    }
    
    
    /**
     * Retrieve readings from CSV file and collate them in a JSONObject
     * @param filePath The file path for the CSV file
     * @param numOfKeys The number of keys in the CSV file
     */
    public JSONObject retrieveReadings(String filePath, int numOfKeys) throws IOException, JSONException {
    	dataJSONArray = new JSONArray();
    	JSONObject l = new JSONObject();
    	JSONArray m = new JSONArray();
    	JSONObject n = new JSONObject();
    	String data;
    	Double[] dataList = null;
		String[] dateList = null;
		String[] keys = null;
		String key;
		String date = "";
		Scanner sc;
		
		int totalNumOfRows = countNumberOfRows(filePath); //count total number of rows in the CSV file
	    dataList = new Double[totalNumOfRows * 24]; //define the size of the data List
		keys = new String[totalNumOfRows]; //define the size of the key list
		dateList = new String[totalNumOfRows]; //define the size of the date list
		LOGGER.info("The total number of rows in the csv file is " + totalNumOfRows);
		
		
		File file = new File(filePath);
        if (!file.exists()) {
            throw new FileNotFoundException("No properties file found at specified filepath: " + filePath);
        }
        sc = new Scanner(new File(filePath));
		
		//The first row is Station;Komponente;Datum;Wert01;Wert02;Wert03;Wert04;Wert05;Wert06;Wert07;Wert08;Wert09;Wert10;Wert11;Wert12;Wert13;Wert14;Wert15;Wert16;Wert17;Wert18;Wert19;Wert20;Wert21;Wert22;Wert23;Wert24;Nachweisgrenze
		data = sc.next(); // skip the first row
		
		LOGGER.info("Collecting all dates, keys and values from the CSV file...");
		totalNumOfRows = 0;
		while (sc.hasNext()) {
			
			//'PS-Innenstadt';'Gesamt_UV_Strahlung';'20220707';0.21651599;0.09434250;0.15619700;0.12448200;0.35326499;2.42124510;5.18091965;8.85325050;5.12361002;8.65175056;14.35159969;14.73234940;14.12985039;20.10925102;24.28499985;12.69910049;7.40499973;6.47114992;3.44150996;2.44336009;0.42795300;0.10956550;0.05604300;0.19735551;-999
			//<location name>;<key>;<date>; ... < 24 hours reading> ... ;<detection limit>
			data = sc.next();
			
		    //retrieve the date and remove ' from the front and back
			try {
				date = data.split(";")[2].substring(1, data.split(";")[2].length() - 1);
				LOGGER.info("The date is " + date);
				dateList[totalNumOfRows] = date;
				
				key = data.split(";")[1].substring(1, data.split(";")[1].length() - 1);
				keys[totalNumOfRows] = key;
				
				for (int h = 0; h < 24; h++) {
	                //collate all values as doubles in a single list
					dataList[(totalNumOfRows * 24) + h] = Double.parseDouble(data.split(";")[h + 3]);
					}
				} catch (IndexOutOfBoundsException e) {
					throw new JPSRuntimeException("There was an error while retrieving the data from the CSV file! The current row retrieved from the CSV file is " +  data.toString());
				}

				totalNumOfRows++;
				}
			sc.close();
			
		int numOfDates = dateList.length / numOfKeys; //given the structure of the raw data, this formula should give you the number of dates per key		
		LOGGER.info("The number of dates are " + numOfDates);
		
		dataJSONArray = processDataIntoJSONArray(numOfDates, numOfKeys, dateList, keys, dataList);
		l.put("data", dataJSONArray);
		LOGGER.info(l.toString());
		m.put(l);
			
		n.put("sensors", m);
		LOGGER.info("The JSONObject is" + n);

		return n;  
    	} 
    
    
    /**
     * Count number of rows in the CSV file
     * @param filePath The file path for the CSV file
     * @throws FileNotFoundException 
     */
    public Integer countNumberOfRows(String filePath)throws IOException {
    	Scanner sc;
    	Integer totalNumOfRows = 0;
    	
    	File file = new File(filePath);
        if (!file.exists()) {
            throw new FileNotFoundException("No properties file found at specified filepath: " + filePath);
        }
        sc = new Scanner(file);
		
		try {
			LOGGER.info("Counting the number of rows in the CSV file...");
			while (sc.hasNext()) {
				if (sc.next().isEmpty()) {
					break;
					}
				else {
					totalNumOfRows++;
					LOGGER.info(totalNumOfRows);
					}
				}
			sc.close();
			}catch (Exception e) {
				throw new JPSRuntimeException("An error was encountered while counting the total number of rows in the CSV file.");
				}
		return totalNumOfRows;
		}
    
    
    /**
     * Retrieve the dates, keys and data values from their respective lists and return them in a JSONArray
     * @param numOfDates The number of dates in the CSV file
     * @param numOfKeys The number of keys in the CSV file
     * @param dateList The list that contains the dates
     * @param keyList The list that contains the keys
     * @param dataList The list that contains the data values
     */
    private JSONArray processDataIntoJSONArray(int numOfDates, int numOfKeys, String[] dateList, String[] keyList, Double[] dataList) {
    	String dateInString;
    	String year;
    	String month;
    	String day;
    	for (int j = 0 ; j <numOfDates; j++) {
    		String dateReference = dateList[j];
    		year = dateReference.substring(0, 4);
    		month = dateReference.substring(4, 6);
    		day = dateReference.substring(6, dateReference.length());
    		
    		if (day.length() != 2) {
    				throw new JPSRuntimeException ("Ecountered an error while parsing the date " + dateReference);
    				}
    		for (int h = 00; h < 24; h++) {
    			if (h < 10) {
    				dateInString = year.concat("-" + month + "-" + day + "T0" + h + ":00:00");
    				} else {
					dateInString = year.concat("-" + month + "-" + day + "T" + h + ":00:00");
					}
					dataReadings = new JSONObject();
					dataReadings.put("ts", dateInString);
					for (int k = 0; k < numOfKeys; k++) {
						String keyReference = keyList[k * numOfDates];
						Double valueAsDouble = dataList[h + (k * numOfDates * 24) + (j * 24)];
						dataReadings.put(keyReference, valueAsDouble);
						LOGGER.info(dataReadings.toString());
						}
					dataJSONArray.put(dataReadings);
					}
    		}
    	return dataJSONArray;
    	}
    }
