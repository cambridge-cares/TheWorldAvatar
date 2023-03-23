package uk.ac.cam.cares.jps.agent.historicalntuenergy;

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

public class HistoricalNTUEnergyAgentXLSXConnector {

	private String energyReadingsFilePath;
	private String numOfEnergyKeys;
	JSONArray readings;
	JSONObject readingsPerHour;
	/**
	 * Logger for reporting info/errors.
	 */
	private static final Logger LOGGER = LogManager.getLogger(HistoricalNTUEnergyAgentLauncher.class);
	/**
	 * Error messages
	 */
	private static final String ENERGY_ERROR_MSG = "Energy readings could not be retrieved";
	/**
	 * Standard constructor
	 */
	public HistoricalNTUEnergyAgentXLSXConnector(String energyReadingsFilePath, String numOfKeysFilePath) throws IOException {
		this.energyReadingsFilePath = energyReadingsFilePath;

		File file = new File(numOfKeysFilePath);
		if (!file.exists()) {
			throw new FileNotFoundException("No properties file found at specified filepath: " + numOfKeysFilePath);
		}
		try (InputStream input = new FileInputStream(file)) {
			Properties prop = new Properties();
			prop.load(input);
			if (prop.containsKey("numOfEnergyKeys")) {
				this.numOfEnergyKeys = prop.getProperty("numOfEnergyKeys");
			} else {
				throw new IOException("Properties file is missing \"numOfEnergyKeys=<numOfEnergyKeys>\"");
			}
		}
	}

	/**
	 * Retrieves the energy readings from the Excel file
	 * @return Readings in a JSON Array with a JSON object for each measurement time
	 */
	public JSONArray getEnergyReadings() {
		try {
			return retrieveReadings(energyReadingsFilePath, Integer.valueOf(numOfEnergyKeys));
		} catch (IOException | JSONException e) {
			LOGGER.error(ENERGY_ERROR_MSG, e);
			throw new JPSRuntimeException(ENERGY_ERROR_MSG, e);
		}
	}
	/**
	 * @param filePath  file path of where the Excel file is located
	 * @param numOfKeys The number of keys/columns in the Excel file
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
					readingsPerHour = new JSONObject();
					for (int j = 0; j < b; j++) {
						Cell cell=row.getCell(j); //getting the cell representing the given column
						Row keyRow = sheet.getRow(0);
						Cell keyCell = keyRow.getCell(j);
						if (keyCell.getStringCellValue().equals("TIME")){
							String value=cell.getStringCellValue();
							readingsPerHour.put(keyCell.getStringCellValue(), value);
						}
						else{
							try {
								Double value = cell.getNumericCellValue();
								readingsPerHour.put(keyCell.getStringCellValue(), value);
							} catch (IllegalStateException | NullPointerException e) {
								Double value=null;
								readingsPerHour.put(keyCell.getStringCellValue(), value);
							}
						}
					}
					readings.put(readingsPerHour);
				}
			}
			pkg.close();
		}
		catch(Exception e)
		{
			e.printStackTrace();
		}
		return readings;
	}
}
