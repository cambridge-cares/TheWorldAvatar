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
    private String generatorSpecFilePath;
    private String busNodeSpecFilePath;
    private String branchSpecFilePath;
    private String pvFilePath;
    private String venueInfoFilePath;
    private String classScheduleFilePath;
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
    public HistoricalNTUEnergyAgentXLSXConnector(String energyReadingsFilePath, String numOfKeysFilePath, String generatorSpecFilePath, String busNodeSpecFilePath, String branchSpecFilePath, String pvFilePath, String venueInfoFilePath, String classScheduleFilePath) throws IOException {
        this.energyReadingsFilePath = energyReadingsFilePath;
        this.generatorSpecFilePath = generatorSpecFilePath;
        this.branchSpecFilePath = branchSpecFilePath;
        this.busNodeSpecFilePath = busNodeSpecFilePath;
        this.pvFilePath = pvFilePath;
        this.venueInfoFilePath = venueInfoFilePath;
        this.classScheduleFilePath = classScheduleFilePath;

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
    // Retreives the water reading from csv file
    public JSONArray getWaterReadings() {
        try {
            return retrieveWaterReadings(energyReadingsFilePath);
        } catch (IOException | JSONException e) {
            LOGGER.error(ENERGY_ERROR_MSG, e);
            throw new JPSRuntimeException(ENERGY_ERROR_MSG, e);
        }
    }
    public JSONArray getGeneratorSpecs() {
        try {
            return retrieveSpecs(generatorSpecFilePath);
        } catch (IOException | JSONException e) {
            LOGGER.error(ENERGY_ERROR_MSG, e);
            throw new JPSRuntimeException(ENERGY_ERROR_MSG, e);
        }
    }
    public JSONArray getBusNodeSpecs() {
        try {
            return retrieveSpecs(busNodeSpecFilePath);
        } catch (IOException | JSONException e) {
            LOGGER.error(ENERGY_ERROR_MSG, e);
            throw new JPSRuntimeException(ENERGY_ERROR_MSG, e);
        }
    }
    public JSONArray getBranchSpecs() {
        try {
            return retrieveSpecs(branchSpecFilePath);
        } catch (IOException | JSONException e) {
            LOGGER.error(ENERGY_ERROR_MSG, e);
            throw new JPSRuntimeException(ENERGY_ERROR_MSG, e);
        }
    }
    public JSONArray getPVSpecs() {
        try {
            return retrieveSpecs(pvFilePath);
        } catch (IOException | JSONException e) {
            LOGGER.error(ENERGY_ERROR_MSG, e);
            throw new JPSRuntimeException(ENERGY_ERROR_MSG, e);
        }
    }
    public JSONArray getVenueInfo() {
        try {
            return retrieveSpecs(venueInfoFilePath);
        } catch (IOException | JSONException e) {
            LOGGER.error(ENERGY_ERROR_MSG, e);
            throw new JPSRuntimeException(ENERGY_ERROR_MSG, e);
        }
    }

    public JSONArray getClassSchedule() {
        try {
            return retrieveSpecs(classScheduleFilePath);
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
        try {
            OPCPackage pkg = OPCPackage.open(filePath);
            try (//creating Workbook instance that refers to .xlsx file
                 XSSFWorkbook wb = new XSSFWorkbook(pkg)) {
                XSSFSheet sheet = wb.getSheetAt(0);     //creating a Sheet object to retrieve object
                int a = sheet.getLastRowNum();
                int b = numOfKeys;
                readings = new JSONArray();
                for (int i = 1; i <= a; i++) {
                    Row row = sheet.getRow(i); //returns the logical row
                    readingsPerHour = new JSONObject();
                    for (int j = 0; j < b; j++) {
                        Cell cell = row.getCell(j); //getting the cell representing the given column
                        Row keyRow = sheet.getRow(0);
                        Cell keyCell = keyRow.getCell(j);
                        if (keyCell.getStringCellValue().equals("TIME")) {
                            String value = cell.getStringCellValue();
                            readingsPerHour.put(keyCell.getStringCellValue(), value);
                        } else {
                            try {
                                Double value = cell.getNumericCellValue();
                                readingsPerHour.put(keyCell.getStringCellValue(), value);
                            } catch (IllegalStateException | NullPointerException e) {
                                Double value = null;
                                readingsPerHour.put(keyCell.getStringCellValue(), value);
                            }
                        }
                    }
                    readings.put(readingsPerHour);
                }
            }
            pkg.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return readings;
    }

    // This function reads a csv file to retrieve water readings
    public JSONArray retrieveWaterReadings(String filePath) throws IOException, JSONException {
        JSONArray readings = new JSONArray();
        String line;
        String cvsSplitBy = ","; // CSV files typically use a comma as the delimiter.

        try (BufferedReader br = new BufferedReader(new FileReader(filePath))) {
            String[] headers = br.readLine().split(cvsSplitBy); // Assuming the first line contains headers.

            while ((line = br.readLine()) != null) {
                String[] values = line.split(cvsSplitBy);
                JSONObject readingsPerHour = new JSONObject();

                // Assuming the "Date Time" and "Timezone" are in the same format for all stations and occur every third column.
                readingsPerHour.put("Date Time", values[0]); // Use the first instance of "Date Time"
                readingsPerHour.put("Timezone", values[1]); // Use the first instance of "Timezone"

                // Skip the repeated "Date Time" and "Timezone" values, start at index 2, and jump every third value.
                for (int i = 2; i < values.length; i += 3) {
                    String ntuKey = headers[i].trim(); // Get the station header
                    Double ntuValue = null;
                    try {
                        ntuValue = Double.parseDouble(values[i]);
                    } catch (NumberFormatException e) {
                        // Log or handle the parse error if necessary.
                    }
                    readingsPerHour.put(ntuKey, ntuValue);
                }

                readings.put(readingsPerHour);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return readings;
    }

    /*
    public static JSONArray retrieveSpecsFromDirectory(String directoryPath) throws IOException {
        File directory = new File(directoryPath);
        File[] files = directory.listFiles((dir, name) -> name.endsWith(".xlsx"));
        JSONArray allSpecs = new JSONArray();

        if (files != null) {
            for (File file : files) {
                LOGGER.info("printing file name: " + file.getName());
                JSONArray specs = retrieveSpecs(file.getAbsolutePath());
                for (int i = 0; i < specs.length(); i++) {
                    //LOGGER.info("printing schedules: " + specs.get(i));
                    allSpecs.put(specs.get(i));
                }
            }
        }
        return allSpecs;
    }
    */

    public static JSONArray retrieveSpecs(String filePath) throws IOException {
        FileInputStream fileInputStream = new FileInputStream(new File(filePath));
        XSSFWorkbook workbook = new XSSFWorkbook(fileInputStream);
        XSSFSheet sheet = workbook.getSheetAt(0);
        Row headerRow = sheet.getRow(0);
        int lastColumn = headerRow.getLastCellNum();
        JSONArray specs = new JSONArray();

        for (int i = 1; i <= sheet.getLastRowNum(); i++) {
            Row row = sheet.getRow(i);
            JSONObject entry = new JSONObject();
            for (int j = 0; j < lastColumn; j++) {
                Cell headerCell = headerRow.getCell(j);
                Cell cell = row.getCell(j);
                String cellValue = getCellValueAsString(cell);
                entry.put(headerCell.getStringCellValue(), cellValue);
            }
            specs.put(entry);
        }
        workbook.close();
        fileInputStream.close();
        return specs;
    }
    private static String getCellValueAsString(Cell cell) {
        if (cell == null) {
            return "";
        }
        switch (cell.getCellType()) {
            case STRING:
                return cell.getStringCellValue();
            case NUMERIC:
                if (org.apache.poi.ss.usermodel.DateUtil.isCellDateFormatted(cell)) {
                    return cell.getDateCellValue().toString();
                } else {
                    return String.valueOf(cell.getNumericCellValue());
                }
            case BOOLEAN:
                return String.valueOf(cell.getBooleanCellValue());
            case FORMULA:
                return cell.getCellFormula();
            default:
                return "";
        }
    }

}
