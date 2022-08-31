package uk.ac.cam.cares.jps.agent.historicalaqmesh;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import org.apache.poi.openxml4j.opc.OPCPackage;
import org.apache.poi.ss.usermodel.Cell;  
import org.apache.poi.ss.usermodel.Row;  
import org.apache.poi.xssf.usermodel.XSSFSheet;  
import org.apache.poi.xssf.usermodel.XSSFWorkbook;  

import java.io.*;
import java.util.Properties;
import java.util.regex.Pattern;

public class HistoricalAQMeshAgentXLSXConnector {
	
	private String gasReadingsFilePath ;
	private String particleAndGeneralReadingsFilePath ;
	private String numOfGasKeys ;
	private String numOfParticleAndGeneralKeys ;
	JSONArray readings;
	JSONObject readingsPer5Minutes;

    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(HistoricalAQMeshAgentLauncher.class);
    /**
     * Error messages
     */
    private static final String GAS_ERROR_MSG ="Gas readings could not be retrieved";
    private static final String PARTICLE_ERROR_MSG = "Particle readings could not be retrieved";

    /**
     * Standard constructor
     *
     */
    public HistoricalAQMeshAgentXLSXConnector(String gasReadingsFilePath, String particleAndGeneralReadingsFilePath, String numOfKeysFilePath) throws IOException {
        this.gasReadingsFilePath = gasReadingsFilePath ;
        this.particleAndGeneralReadingsFilePath = particleAndGeneralReadingsFilePath ;
        
        File file = new File(numOfKeysFilePath);
        if (!file.exists()) {
            throw new FileNotFoundException("No properties file found at specified filepath: " + numOfKeysFilePath);
        }
        try (InputStream input = new FileInputStream(file)) {

            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);

            if (prop.containsKey("numOfGasKeys")) {
                this.numOfGasKeys = prop.getProperty("numOfGasKeys");
            } else {
                throw new IOException("Properties file is missing \"numOfGasKeys=<numOfGasKeys>\"");
            }
            if (prop.containsKey("numOfParticleAndGeneralKeys")) {
                this.numOfParticleAndGeneralKeys = prop.getProperty("numOfParticleAndGeneralKeys");
            } else {
                throw new IOException("Properties file is missing \"numOfParticleAndGeneralKeys=<numOfParticleAndGeneralKeys>\"");
            }
        }
    }

    /**
     * Retrieves the particle and general readings from the excel file
     * @return Readings in a JSON Array with a JSON object for each measurement time
     */
    public JSONArray getParticleReadings() {
        try {
            return retrieveReadings(particleAndGeneralReadingsFilePath, Integer.valueOf(numOfParticleAndGeneralKeys));
        }
        catch (IOException | JSONException e) {
            LOGGER.error(PARTICLE_ERROR_MSG, e);
            throw new JPSRuntimeException(PARTICLE_ERROR_MSG, e);
        }
    }

    /**
     * Retrieves the gas readings from the excel file
     * @return Readings in a JSON Array with a JSON object for each measurement time
     */
    public JSONArray getGasReadings() {
        try {
            return retrieveReadings(gasReadingsFilePath, Integer.valueOf(numOfGasKeys));
        }
        catch (IOException | JSONException e) {
            LOGGER.error(GAS_ERROR_MSG, e);
            throw new JPSRuntimeException(GAS_ERROR_MSG, e);
        }
    }

    /**
     * @param filePath file path of where the excel file is located
     * @param numOfKeys The number of keys/columns in the excel file
     * @return Readings in a JSON Array with a JSON object for each measurement time
     */
    public JSONArray retrieveReadings(String filePath, int numOfKeys) throws IOException, JSONException {
    	try  
    	{  
    	
    	OPCPackage pkg = OPCPackage.open(filePath); 
    	try (//creating Workbook instance that refers to .xlsx file  
		XSSFWorkbook wb = new XSSFWorkbook(pkg)) {
			XSSFSheet sheet = wb.getSheetAt(0);     //creating a Sheet object to retrieve object  
			int a = sheet.getLastRowNum();
			int b = numOfKeys;
			readings = new JSONArray();
			for (int i = 1 ; i <= a; i++) {
			Row row=sheet.getRow(i); //returns the logical row
			readingsPer5Minutes = new JSONObject();
			for (int j = 0; j < b; j++) {
			Cell cell=row.getCell(j); //getting the cell representing the given column
			Row keyRow = sheet.getRow(0);
			Cell keyCell = keyRow.getCell(j);
			
			switch (getClassFromJSONKey(keyCell.getStringCellValue()))               
			{  
			case "String":    //field that represents string cell type
			System.out.println(keyCell.getStringCellValue());
			try {
			String value = cell.getStringCellValue();
			readingsPer5Minutes.put(keyCell.getStringCellValue(), value);
			} catch (IllegalStateException | NullPointerException e) {
				Object value_1 =  JSONObject.NULL;
				readingsPer5Minutes.put(keyCell.getStringCellValue(), value_1);
			}
			break;  
			
			case "Double":    //field that represents number cell type
		    System.out.println(keyCell.getStringCellValue());
			Double value1 ;
			Object value1_2;
			try {
			value1 = cell.getNumericCellValue();
			readingsPer5Minutes.put(keyCell.getStringCellValue(), value1);
			} catch (IllegalStateException | NullPointerException e) {
				value1_2 =  JSONObject.NULL;
				readingsPer5Minutes.put(keyCell.getStringCellValue(), value1_2);
			}
			break; 
			
			case "Integer":    //field that represents number cell type 
			System.out.println(keyCell.getStringCellValue());
			double value2;
			Object value2_2;
			try {
				value2 = cell.getNumericCellValue(); 
				 Double value2_3 = value2 * 1.000;
                 int value=value2_3.intValue();
			readingsPer5Minutes.put(keyCell.getStringCellValue(), value);
			} catch (NullPointerException e) {
				value2_2 = JSONObject.NULL;
				readingsPer5Minutes.put(keyCell.getStringCellValue(), value2_2);
			} catch (IllegalStateException e) {
				value2_2 = cell.getStringCellValue();
				readingsPer5Minutes.put(keyCell.getStringCellValue(), value2_2);
			}
			break; 
			
			case "Boolean":    //field that represents number cell type 
			System.out.println(keyCell.getStringCellValue());
			try {
			Boolean value3 = cell.getBooleanCellValue();
			readingsPer5Minutes.put(keyCell.getStringCellValue(), value3);
			} catch (NullPointerException e) {
				Object value3_2 = JSONObject.NULL ;
				readingsPer5Minutes.put(keyCell.getStringCellValue(), value3_2);
			}
			break; 
			
			case "ts":    //field that represents number cell type  
			System.out.println(keyCell.getStringCellValue());
			String value4;
			try {
			value4 = cell.getLocalDateTimeCellValue().toString(); 
			readingsPer5Minutes.put(keyCell.getStringCellValue(), value4);
			} catch (NullPointerException e) {
				Object value4_2 = JSONObject.NULL;
				readingsPer5Minutes.put(keyCell.getStringCellValue(), value4_2);
			}
			
			break; 
			/*
			case "getStringConvertToInteger":
			System.out.println(keyCell.getStringCellValue());
			try {
		    String value5 = cell.getStringCellValue();
			readingsPer5Minutes.put(keyCell.getStringCellValue(), Integer.valueOf(value5));
			} catch (NullPointerException e) {
				Object value5_2 = JSONObject.NULL;
				readingsPer5Minutes.put(keyCell.getStringCellValue(), value5_2);
			}
			break;
			
			
			case "stringSerialNumber":    //field that represents sensor serial number
				System.out.println(keyCell.getStringCellValue());
				try {
			    String value6 = cell.getStringCellValue(); 
				readingsPer5Minutes.put(keyCell.getStringCellValue(), (value6));
				} catch (NullPointerException e) {
					Object value6_2 = JSONObject.NULL ;
					readingsPer5Minutes.put(keyCell.getStringCellValue(), value6_2);
				}
				break; 
				*/
			}  
			}
			System.out.println(readingsPer5Minutes.toString());
			readings.put(readingsPer5Minutes);
			}
		}
    	pkg.close();
    	System.out.println(readings.toString());
    	}
    	catch(Exception e)  
    	{  
    	e.printStackTrace();  
    	}
		return readings;  
    	}  
    	
    /**
     * Returns the class (datatype) corresponding to a JSON key. Note: rules for the mapping are hardcoded in the method.
     * @param jsonKey The JSON key as string.
     * @return The corresponding class as Class<?> object.
     */
    private String getClassFromJSONKey(String jsonKey) {
        // JSON keys for reading and sending intervals end in _p1, _p2 or _p3
        Pattern intervalPattern = Pattern.compile(".*_p[123]$");
        // Intervals are integers representing the seconds
        if (intervalPattern.matcher(jsonKey).matches() || jsonKey.contains("_reading_number")) {
            return "Integer";
        }
        // Battery voltage is a floating point
        else if (jsonKey.contains("_voltage")) {
            return "Double";
        }
        else if (jsonKey.contains("reading_datestamp")) {
            return "ts";
        }
        else if (jsonKey.contains("temperature") || jsonKey.contains("pressure") || jsonKey.contains("humidity")) {
            return "Double";
        }
        else if (jsonKey.contains("noise")) {
            return "Double";
        }
        else if (jsonKey.contains("pod_serial_number") || jsonKey.contains("owner_number") || jsonKey.contains("location_number") || jsonKey.contains("_serial_number")) {
            return "Integer";
        }
        /*
        else if (jsonKey.contains("aux2_sensor_serial_number") || jsonKey.contains("uart_sensor_serial_number") || jsonKey.contains("aux1_sensor_serial_number") || jsonKey.contains("h2s_sensor_serial_number")) {
            return "stringSerialNumber";
        }
        else if (jsonKey.contains("pod_serial_number") || jsonKey.contains("owner_number") || jsonKey.contains("location_number") || jsonKey.contains("_serial_number")) {
            return "getStringConvertToInteger";
        }
        */
        // Sensor readings and corresponding offset and slope are floating point numbers
        else if (jsonKey.contains("prescale") || jsonKey.contains("slope") || jsonKey.contains("offset")) {
            return "Double";
        }
        // Battery low warning and particle modem overlap are boolean
        else if (jsonKey.equals("battery_low") || jsonKey.equals("particle_modem_overlap")) {
            return "Boolean";
        }
        // The default datatype is string
        else {
            return "String";
        }
    }
    

}
