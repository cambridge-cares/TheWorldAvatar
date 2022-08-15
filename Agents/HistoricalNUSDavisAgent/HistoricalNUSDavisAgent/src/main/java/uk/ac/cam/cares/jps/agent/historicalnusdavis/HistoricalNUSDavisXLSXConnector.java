package uk.ac.cam.cares.jps.agent.historicalnusdavis;


import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.openxml4j.opc.OPCPackage;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.*;
import java.util.Properties;

/**
 * Class that retrieves data values frome excel files via the apache POI API
 * @author  */
class HistoricalNUSDavisXLSXConnector{
    private String dataFilePath ;
	private String numOfKeys ;
	JSONArray data;
	JSONObject dataReadings;

    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(HistoricalNUSDavisAgentLauncher.class);
    private static final String DATA_ERROR_MSG = "Data readings could not be retrieved";

    /**
     * Standard constructor
     *
     */
    public HistoricalNUSDavisXLSXConnector(String filePath, String numOfKeysFilePath) throws IOException {
        this.dataFilePath = filePath; 
        
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

   
    public JSONObject getReadings() {
        try {
            return retrieveReadings(dataFilePath, Integer.valueOf(numOfKeys));
        }
        catch (IOException | JSONException e) {
            LOGGER.error(DATA_ERROR_MSG, e);
            throw new JPSRuntimeException(DATA_ERROR_MSG, e);
        }
    }

   
    public JSONObject retrieveReadings(String filePath, int numOfKeys) throws IOException, JSONException {
    	JSONObject k = new JSONObject();
    	JSONArray l = new JSONArray();
    	JSONObject m = new JSONObject();
    	
    	OPCPackage pkg = null;
		try {
			pkg = OPCPackage.open(filePath);
		} catch (InvalidFormatException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} 
    	try (//creating Workbook instance that refers to .xlsx file  
		XSSFWorkbook wb = new XSSFWorkbook(pkg)) {
			XSSFSheet sheet = wb.getSheetAt(0);     //creating a Sheet object to retrieve object  
			int a = sheet.getLastRowNum();
			int b = numOfKeys;
			data = new JSONArray();
			for (int i = 1 ; i <= a; i++) {
				Row row=sheet.getRow(i); //returns the logical row
			dataReadings = new JSONObject();
			for (int j = 0; j < b; j++) {
			Cell cell=row.getCell(j); //getting the cell representing the given column
			Row keyRow = sheet.getRow(0);
			Cell keyCell = keyRow.getCell(j);
			
			switch (getClassFromJSONKey(keyCell.getStringCellValue()))               
			{  
			case "String":    //field that represents string cell type
			
			try {
			String value = cell.getStringCellValue();
			dataReadings.put(keyCell.getStringCellValue(), value);
			} catch (NullPointerException | IllegalStateException e) {
				Object value_1 =  JSONObject.NULL;
				dataReadings.put(keyCell.getStringCellValue(), value_1);
			} 
			break;  
			
			case "double":    //field that represents number cell type
		    
			Double value1 ;
			Object value1_2;
			try {
			value1 = cell.getNumericCellValue();
			dataReadings.put(keyCell.getStringCellValue(), value1);
			} catch (NullPointerException | IllegalStateException e) {
				value1_2 = JSONObject.NULL;
				dataReadings.put(keyCell.getStringCellValue(), value1_2);
			} 
			break; 
			
			case "integer":    //field that represents number cell type 
			
			double value2;
			Object value2_2;
			try {
				value2 = cell.getNumericCellValue(); 
			dataReadings.put(keyCell.getStringCellValue(), value2);
			} catch (NullPointerException | IllegalStateException e) {
				value2_2 = JSONObject.NULL;
				dataReadings.put(keyCell.getStringCellValue(), value2_2);
			} 
			break; 
			
			case "ts":    //field that represents number cell type  
			
			String value4;
			try {
			value4 = cell.getLocalDateTimeCellValue().toString(); 
			dataReadings.put(keyCell.getStringCellValue(), value4);
			} catch (IllegalStateException | NullPointerException e) {
				Object value4_2 = JSONObject.NULL;
				dataReadings.put(keyCell.getStringCellValue(), value4_2);
			}
			
			break; 
			}
			
			}
			
			data.put(dataReadings);
			}
			k.put("data", data);
			System.out.println(k.toString());
			LOGGER.info(k.toString());
			l.put(k);
			
			m.put("sensors", l);
			System.out.println("The JSONObject is" + m);

    	pkg.close();
    	
    	}
    	catch(Exception e)  
    	{  
    	e.printStackTrace();  
    	}
		return m;  
    	}  
    	
    /**
     * Returns the class (datatype) corresponding to a JSON key. Note: rules for the mapping are hardcoded in the method.
     * @param jsonKey The JSON key as string.
     * @return The corresponding class as Class<?> object.
     */
    private String getClassFromJSONKey(String jsonKey) {
        if (   jsonKey.contains("bar")  || jsonKey.contains("rain_day_mm") || jsonKey.contains("rain_month_mm")
            || jsonKey.contains("rain_year_mm") || jsonKey.contains("rain_rate_mm") || jsonKey.contains("rain_storm_mm")
            || jsonKey.contains("temp_in")  || jsonKey.contains("temp_out") || jsonKey.contains("dew_point") || jsonKey.contains("heat_index")
            || jsonKey.contains("wind_chill") || jsonKey.contains("hum_out") || jsonKey.contains("solar_rad") || jsonKey.contains("wind_speed")
            || jsonKey.contains("wind_dir") ) {
            return "double";
        }
        else if (jsonKey.contains("hum_in") || jsonKey.contains("uv")) {
        	return "integer";
        }
        else if (jsonKey.contains("ts")) {
        	return "ts";
        }
        else {
            return "string";
        }
    }
    
}
