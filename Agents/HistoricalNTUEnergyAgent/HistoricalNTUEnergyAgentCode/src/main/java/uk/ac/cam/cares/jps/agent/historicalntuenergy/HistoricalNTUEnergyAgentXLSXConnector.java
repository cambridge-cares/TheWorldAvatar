package uk.ac.cam.cares.jps.agent.historicalntuenergy;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
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

public class HistoricalNTUEnergyAgentXLSXConnector {

	private String energyReadingsFilePath;
	//private String particleAndGeneralReadingsFilePath ;
	private String numOfEnergyKeys;
	//private String numOfParticleAndGeneralKeys ;
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
	//private static final String PARTICLE_ERROR_MSG = "Particle readings could not be retrieved";

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

			// Load properties file from specified path
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
	 * Retrieves the energy readings from the excel file
	 *
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
	 * @param filePath  file path of where the excel file is located
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
					readingsPerHour = new JSONObject();
					for (int j = 0; j < b; j++) {
						Cell cell=row.getCell(j); //getting the cell representing the given column
						Row keyRow = sheet.getRow(0);
						Cell keyCell = keyRow.getCell(j);
						LOGGER.info("----");
						LOGGER.info(keyCell.getStringCellValue());
						if (keyCell.getStringCellValue().equals("TIME")){
							LOGGER.info("HAS TIME");
							//String value=cell.getLocalDateTimeCellValue().toString();
							String value=cell.getStringCellValue();
							readingsPerHour.put(keyCell.getStringCellValue(), value);
							LOGGER.info(value);
						}
						else{
							try {
								Double value = cell.getNumericCellValue();
								readingsPerHour.put(keyCell.getStringCellValue(), value);
								LOGGER.info(value.toString());
							} catch (IllegalStateException | NullPointerException e) {
								Double value=null;
								readingsPerHour.put(keyCell.getStringCellValue(), value);
							}
						}
						//LOGGER.info(value.toString());
					}
					System.out.println(readingsPerHour.toString());
					readings.put(readingsPerHour);
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


}
