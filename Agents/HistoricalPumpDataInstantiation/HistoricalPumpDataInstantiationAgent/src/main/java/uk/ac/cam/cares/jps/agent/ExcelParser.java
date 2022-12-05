package uk.ac.cam.cares.jps.agent;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.ss.usermodel.*;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.*;
import java.time.Instant;
import java.time.LocalDate;
import java.time.Year;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Parses the readings from the Excel workbook into a Java collection that can be read by the time series client.
 *
 * @author qhouyee
 */
class ExcelParser {
    // Logger for reporting info/errors.
    private static final Logger LOGGER = LogManager.getLogger(HistoricalPumpDataInstantiationAgent.class);
    // Error messages
    private static final String FILE_NOT_FOUND_MSG = "No excel file found at specified filepath: %s";
    private static final String EXCEL_ERROR_MSG = "Data could not be retrieved from Excel";
    private static final String INVALID_SHEET_INDEX_MSG = "Sheet Index is invalid, please input a integer starting from 0.";

    private static final String INVALID_VALUE_STARTING_ROW_MSG = "valueStartingRow is invalid, please input an integer larger than 1.";
    private final int sheetIndex;
    private final String timeHeader;
    private final String EXCEL_FILE_PATH;
    private final String instantTimeZone = "T00:00:00Z"; // UTC Time zone
    private Sheet dataSheet;


    /**
     * Overloaded constructor with a default sheetIndex.
     * This will check for a valid File path as well.
     *
     * @param excel_filepath Specifies the file path to the Excel workbook.
     * @param timeHeader     Header name of the Time Column
     */
    protected ExcelParser(String excel_filepath, String timeHeader) throws IOException {
        this(excel_filepath, timeHeader, 0);
    }

    /**
     * Standard constructor which checks for a valid File path and modifies the sheetIndex value
     * By default, it has been set to the first sheet at index 0
     *
     * @param excel_filepath Specifies the file path to the Excel workbook.
     * @param timeHeader     Header name of the Time Column
     * @param sheetIndex     Specifies the sheet index to read in the Excel file.
     */
    protected ExcelParser(String excel_filepath, String timeHeader, int sheetIndex) throws IOException {
        File file = new File(excel_filepath);
        if (!file.exists()) {
            LOGGER.fatal(FILE_NOT_FOUND_MSG);
            throw new FileNotFoundException(FILE_NOT_FOUND_MSG);
        }
        this.EXCEL_FILE_PATH = excel_filepath;
        LOGGER.debug("Excel Workbook exists and is available...");
        this.timeHeader = timeHeader;

        if (sheetIndex < 0) {
            LOGGER.error(INVALID_SHEET_INDEX_MSG);
            throw new IOException(INVALID_SHEET_INDEX_MSG);
        } else {
            this.sheetIndex = sheetIndex;
        }
        LOGGER.debug("Sheet index has been set to: " + sheetIndex);
    }


    /**
     * Parses the values stored in Excel into a HashMap.
     *
     * @param valueStartingRow Starting row number that contains values, not headings. Indexing starts from 0, not 1
     * @return Excel values as a HashMap containing {dataIRI prefix: List of readings} key value pairs
     */
    protected Map<String, List<?>> parseToHashMap(int valueStartingRow) {
        Map<String, List<?>> excelValues;
        try (InputStream excelFile = new FileInputStream(this.EXCEL_FILE_PATH);
             Workbook workbook = WorkbookFactory.create(excelFile)) {
            LOGGER.debug("Opening Workbook...");
            this.dataSheet = workbook.getSheetAt(this.sheetIndex);
            LOGGER.debug("Processing Worksheet...");
            excelValues = parseSheet(valueStartingRow);
        } catch (Exception e) {
            LOGGER.error(EXCEL_ERROR_MSG, e);
            throw new JPSRuntimeException(EXCEL_ERROR_MSG, e);
        }
        return excelValues;
    }

    /**
     * Parses the Excel worksheet into a Hashmap
     *
     * @param valueStartingRow starting row number that contains values, not headings. Indexing starts from 0, not 1
     * @return Excel values as a HashMap object
     */
    private Map<String, List<?>> parseSheet(int valueStartingRow) {
        List<?> columnValues;
        Map<String, List<?>> excelValues = new HashMap<>();
        LOGGER.debug("Parsing the headings...");
        List<String> columnHeaders = parseHeadings(valueStartingRow - 1);
        LOGGER.debug("Headings parsed...");
        // Iterate over each row and store all column values into a list
        // Index starts from 0 while size start from 1
        for (int columnIndex = 0; columnIndex < columnHeaders.size(); columnIndex++) {
            if (columnIndex == columnHeaders.indexOf(timeHeader)) {
                LOGGER.debug("Parsing the values in time column " + columnIndex + "...");
                // For time column, parse column differently
                columnValues = parseColValuesToList(valueStartingRow, columnIndex, true);
            } else {
                LOGGER.debug("Parsing the values in column " + columnIndex + "...");
                columnValues = parseColValuesToList(valueStartingRow, columnIndex);
            }
            excelValues.put(columnHeaders.get(columnIndex), columnValues);
        }
        LOGGER.debug("All column values have been parsed...");
        return excelValues;
    }

    /**
     * Parse the column headings contain in the first or multiple rows into one string for prefixing dataIRI
     *
     * @param headerEndingRow Ending row number that contains the headings, usually 1 row before start of values
     * @return ArrayList containing all the headers in "firstrowcell_secondrowcell_thirdrowcell" format
     */
    private List<String> parseHeadings(int headerEndingRow) throws IllegalStateException, NullPointerException {
        List<String> columnHeaders = new ArrayList<>();
        String headerContent;
        for (int i = 0; i == headerEndingRow; i++) {
            LOGGER.debug("Parsing row " + i + "...");
            Row row = this.dataSheet.getRow(i);
            for (int j = 0; j < row.getLastCellNum(); j++) {
                LOGGER.debug("Parsing cell " + j + "...");
                Cell cell = row.getCell(j);

                // When cell is null, cell.getStringCellValue will throw an exception and break compilation
                headerContent = (cell != null) ? formatHeaderStringCell(cell) : "";
                LOGGER.debug("Header content is '" + headerContent + "'...");
                // When current row is first row, add a new (empty or not) String to Array List
                // Empty strings are vital as a placeholder to prevent skipping, which leads to inaccurate size
                if (i == 0) {
                    columnHeaders.add(headerContent);
                } else {
                    // When there is a header value in previous rows
                    if (cell != null && !columnHeaders.get(j).isEmpty() && !cell.getStringCellValue().isEmpty()) {
                        // Retrieve the existing String stored and concatenate the new cell content
                        String newHeaderContent = columnHeaders.get(j) + "_" + headerContent;
                        columnHeaders.set(j, newHeaderContent);
                        LOGGER.debug("Updated header content is '" + newHeaderContent + "'...");
                    } else if (columnHeaders.get(j).isEmpty()) {
                        // When there is no header values in previous rows, reset the string
                        columnHeaders.set(j, headerContent);
                    }
                }
            }
        }
        return columnHeaders;
    }

    /**
     * Retrieves the cell value as a formatted String for data IRI. Removes any characters after Brackets.
     * For eg, an input of "Time Series (Unit)" will return "TimeSeries"
     *
     * @param cell Text cell to retrieved String from in Excel
     * @return String in format "CapitalisedFirstWordAfterSpace"
     */
    private String formatHeaderStringCell(Cell cell) {
        LOGGER.debug("Formatting header...");
        // Retrieve cell and stores them in lowercase
        String phrase = cell.getStringCellValue().toLowerCase();
        // Remove description or information included as brackets () if they exist
        int startingIndexToRemove = phrase.indexOf("(");
        phrase = startingIndexToRemove == -1 ? phrase : phrase.substring(0, startingIndexToRemove);
        // Remove leading and trailing spaces before converting the string to an array
        char[] phraseChars = phrase.trim().toCharArray();
        // Capitalise the first character of the string and after a white space
        for (int i = 0; i < phraseChars.length - 1; i++) {
            if (i == 0) {
                phraseChars[i] = Character.toUpperCase(phraseChars[i]);
            } else if (phraseChars[i] == ' ') {
                phraseChars[i + 1] = Character.toUpperCase(phraseChars[i + 1]);
            }
        }
        // Remove white spaces
        return String.valueOf(phraseChars).replaceAll("\\s+", "");
    }

    /**
     * Overloaded method that calls standard method with a default false parameter.
     * Stores the values of one column into a list
     *
     * @param valueStartingRow Starting row number that contains values, not headings. Indexing starts from 0, not 1
     * @param columnIndex      Index of column to parse. Indexing starts from 0, not 1
     * @return A list object containing all the values in the cell
     */
    private List<?> parseColValuesToList(int valueStartingRow, int columnIndex) throws IllegalArgumentException {
        return parseColValuesToList(valueStartingRow, columnIndex, false);
    }

    /**
     * Stores the values of one column into a list
     *
     * @param valueStartingRow Starting row number that contains values, not headings. Indexing starts from 0, not 1
     * @param columnIndex      Index of column to parse. Indexing starts from 0, not 1
     * @param isTimeColumn     A boolean indicating if current column is time
     * @return A list object containing all the values in the cell
     */
    private List<?> parseColValuesToList(int valueStartingRow, int columnIndex, boolean isTimeColumn) throws IllegalArgumentException {
        if (valueStartingRow < 1) {
            LOGGER.error(INVALID_VALUE_STARTING_ROW_MSG);
            throw new IllegalArgumentException(INVALID_VALUE_STARTING_ROW_MSG);
        }

        List<Object> columnValues = new ArrayList<>();
        for (int i = valueStartingRow; i <= this.dataSheet.getLastRowNum(); i++) {
            LOGGER.debug("Parsing cell of row " + i + "...");
            Row currentRow = this.dataSheet.getRow(i);
            Cell currentCell = currentRow.getCell(columnIndex);
            // Add cell values into a List
            try {
                if (isTimeColumn) {
                    if (timeHeader.equalsIgnoreCase("year")) {
                        Double doubleValue = (Double) readCell(currentCell);
                        Year year = Year.of(doubleValue.intValue());
                        LocalDate date = year.atDay(year.length());
                        String instantValue = date + instantTimeZone;
                        columnValues.add(Instant.parse(instantValue));
                    }
                } else {
                    columnValues.add(readCell(currentCell));
                }
            } catch (IllegalStateException | NullPointerException e) {
                columnValues.add("");
            }
        }
        return columnValues;
    }

    /**
     * Read the cell content in the proper data type format
     *
     * @param cell Excel cell input
     * @return Cell content as the right data type
     */
    private Object readCell(Cell cell) throws IllegalStateException, NullPointerException {
        switch (cell.getCellType()) {
            case STRING:
                return cell.getStringCellValue();
            case NUMERIC:
                if (DateUtil.isCellDateFormatted(cell)) {
                    return cell.getLocalDateTimeCellValue();
                } else {
                    // in Double.class format
                    return cell.getNumericCellValue();
                }
            case BOOLEAN:
                return cell.getBooleanCellValue();
            default:
                return "";
        }
    }
}