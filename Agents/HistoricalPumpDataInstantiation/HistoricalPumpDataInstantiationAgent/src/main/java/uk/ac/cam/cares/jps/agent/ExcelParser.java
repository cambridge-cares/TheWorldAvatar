package uk.ac.cam.cares.jps.agent;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.ss.usermodel.*;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.*;
import java.time.Instant;
import java.time.LocalDate;
import java.time.Year;
import java.util.*;

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
    private Map<String, List<Integer>> groupingRowIndexMapping;


    /**
     * Overloaded constructor with a default sheetIndex.
     * This will check for a valid File path as well.
     *
     * @param excel_filepath Specifies the file path to the Excel workbook.
     * @param timeHeader     Header name of the Time Column.
     */
    protected ExcelParser(String excel_filepath, String timeHeader) throws IOException {
        this(excel_filepath, timeHeader, 0);
    }

    /**
     * Standard constructor which checks for a valid File path and modifies the sheetIndex value
     * By default, it has been set to the first sheet at index 0
     *
     * @param excel_filepath Specifies the file path to the Excel workbook.
     * @param timeHeader     Header name of the Time Column.
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
     * Parses the Excel worksheet into the required Hashmap of hashmap format of {group: {dataIRI prefix: List of readings}}.
     *
     * @param valueStartingRow Starting row number that contains values, not headings. Indexing starts from 0, not 1.
     * @param multiTSColIndex  Column index containing different time series grouping. Indexing starts from 0, not 1.
     *                         If no column index is present, pass -1.
     * @return Excel values as a HashMap containing {group: {dataIRI prefix: List of readings}} key value pairs.
     */
    protected Map<String, Map<String, List<?>>> parseToHashMap(int valueStartingRow, int multiTSColIndex) {
        Map<String, Map<String, List<?>>> groupedExcelValue;
        try (InputStream excelFile = new FileInputStream(this.EXCEL_FILE_PATH);
             Workbook workbook = WorkbookFactory.create(excelFile)) {
            LOGGER.debug("Opening Workbook...");
            this.dataSheet = workbook.getSheetAt(this.sheetIndex);
            LOGGER.debug("Processing Worksheet...");
            retrieveTimeSeriesGroups(valueStartingRow, multiTSColIndex);
            groupedExcelValue = parseSheet(valueStartingRow, multiTSColIndex);
        } catch (Exception e) {
            LOGGER.error(EXCEL_ERROR_MSG, e);
            throw new JPSRuntimeException(EXCEL_ERROR_MSG, e);
        }
        return groupedExcelValue;
    }

    /**
     * Retrieve the unique time series grouping names in the column specified by the POST parameters.
     * This will also extract and store each grouping and their row indices into a map.
     *
     * @param valueStartingRow Starting row number that contains values, not headings. Indexing starts from 0, not 1.
     * @param multiTSColIndex  Column index containing different time series grouping. Indexing starts from 0, not 1.
     */
    private void retrieveTimeSeriesGroups(int valueStartingRow, int multiTSColIndex) {
        groupingRowIndexMapping = new HashMap<>();
        if (multiTSColIndex != -1) {
            LOGGER.debug("Retrieving Time Series Groups...");
            for (int i = valueStartingRow; i <= this.dataSheet.getLastRowNum(); i++) {
                // Read the value of the grouping column
                Row currentRow = this.dataSheet.getRow(i);
                Cell currentCell = currentRow.getCell(multiTSColIndex);
                String grouping = ExcelParserHelper.formatHeaderStringCell(currentCell);
                // If the hashmap already contains this grouping key, add the row index to its List
                if (groupingRowIndexMapping.containsKey(grouping)) {
                    groupingRowIndexMapping.get(grouping).add(i);
                } else {
                    // Otherwise, create a new List and add the row index to this list
                    List<Integer> rowIndexList = new ArrayList<>();
                    rowIndexList.add(i);
                    // Next, put the group and its list as a new Key Value pair
                    groupingRowIndexMapping.put(grouping, rowIndexList);
                }
            }
        }
    }

    /**
     * Parses the Excel worksheet into the required Hashmap of hashmap format of {group: {dataIRI prefix: List of readings}}.
     *
     * @param valueStartingRow starting row number that contains values, not headings. Indexing starts from 0, not 1.
     * @param multiTSColIndex  Column index containing different time series grouping. Indexing starts from 0, not 1.
     * @return Excel values as a HashMap containing key value pairs.
     */
    private Map<String, Map<String, List<?>>> parseSheet(int valueStartingRow, int multiTSColIndex) {
        Map<String, Map<String, List<?>>> groupedExcelValues = new HashMap<>();
        LOGGER.debug("Parsing the headings...");
        List<String> colHeaders = parseHeadings(valueStartingRow - 1);
        LOGGER.debug("Headings parsed...");
        // Iterate over each row and store all column values into a list
        // Index starts from 0 while size start from 1
        for (int columnIndex = 0; columnIndex < colHeaders.size(); columnIndex++) {
            List<?> columnValues;
            // When it is not the Time Series Groupings column, run the script. Otherwise, ignore the column
            if (columnIndex != multiTSColIndex) {
                // Execute when there are only one time series
                if (multiTSColIndex == -1) {
                    LOGGER.debug("Parsing column " + columnIndex + " for single time series...");
                    columnValues = parseColValues(valueStartingRow, this.dataSheet.getLastRowNum(), columnIndex, colHeaders);
                    ExcelParserHelper.storeInParentMap(groupedExcelValues, ExcelParserHelper.BASE_KEY_FOR_SINGLE_TIMESERIES,
                            colHeaders.get(columnIndex), columnValues);
                } else {
                    LOGGER.debug("Parsing column " + columnIndex + " for multiple time series...");
                    // Execute when there are multiple time series
                    for (String group : groupingRowIndexMapping.keySet()) {
                        LOGGER.debug("Parsing rows for " + group + "...");
                        // Retrieve the list of row indices for each grouping
                        List<Integer> groupRowIndex = groupingRowIndexMapping.get(group);
                        // Parse only the rows related to the grouping
                        columnValues = parseColValues(groupRowIndex.get(0), groupRowIndex.get(groupRowIndex.size() - 1), columnIndex, colHeaders);
                        ExcelParserHelper.storeInParentMap(groupedExcelValues, group, colHeaders.get(columnIndex), columnValues);
                    }
                }
            }
        }
        LOGGER.debug("All column values have been parsed...");
        return groupedExcelValues;
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
                headerContent = (cell != null) ? ExcelParserHelper.formatHeaderStringCell(cell) : "";
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
     * Intermediate method to parse column values depending on if they are the time column or not.
     *
     * @param startingRow Starting row number to start parsing into List. Indexing starts from 0, not 1.
     * @param lastRow     Last row number to parse into List. Indexing starts from 0, not 1.
     * @param columnIndex Index of column to parse. Indexing starts from 0, not 1.
     * @param colHeaders  List of column headers to determine if they are time colum.
     * @return A list object containing all the values in the column.
     */
    private List<?> parseColValues(int startingRow, int lastRow, int columnIndex, List<String> colHeaders) throws IllegalArgumentException {
        List<?> colValues;
        if (columnIndex == colHeaders.indexOf(timeHeader)) {
            LOGGER.debug("Parsing the values in time column " + columnIndex + "...");
            // For time column, parse column differently
            colValues = parseColValuesToList(startingRow, lastRow, columnIndex, true);
        } else {
            LOGGER.debug("Parsing the values in column " + columnIndex + "...");
            colValues = parseColValuesToList(startingRow, lastRow, columnIndex);
        }
        return colValues;
    }

    /**
     * Overloaded method that calls standard method with a default false parameter.
     * Stores the values of one column into a list.
     *
     * @param startingRow Starting row number to start parsing into List. Indexing starts from 0, not 1.
     * @param lastRow     Last row number to parse into List. Indexing starts from 0, not 1.
     * @param columnIndex Index of column to parse. Indexing starts from 0, not 1.
     * @return A list object containing all the values in the column.
     */
    private List<?> parseColValuesToList(int startingRow, int lastRow, int columnIndex) throws IllegalArgumentException {
        return parseColValuesToList(startingRow, lastRow, columnIndex, false);
    }

    /**
     * Stores the values of one column into a list.
     *
     * @param startingRow  Starting row number to start parsing into List. Indexing starts from 0, not 1.
     * @param lastRow      Last row number to parse into List. Indexing starts from 0, not 1.
     * @param columnIndex  Index of column to parse. Indexing starts from 0, not 1.
     * @param isTimeColumn A boolean indicating if current column is time.
     * @return A list object containing all the values in the column.
     */
    private List<?> parseColValuesToList(int startingRow, int lastRow, int columnIndex, boolean isTimeColumn) throws IllegalArgumentException {
        if (startingRow < 1) {
            LOGGER.error(INVALID_VALUE_STARTING_ROW_MSG);
            throw new IllegalArgumentException(INVALID_VALUE_STARTING_ROW_MSG);
        }

        List<Object> columnValues = new ArrayList<>();
        for (int i = startingRow; i <= lastRow; i++) {
            LOGGER.debug("Parsing cell of row " + i + "...");
            Row currentRow = this.dataSheet.getRow(i);
            Cell currentCell = currentRow.getCell(columnIndex);
            // Add cell values into a List
            try {
                if (isTimeColumn) {
                    if (timeHeader.equalsIgnoreCase("year")) {
                        Double doubleValue = (Double) ExcelParserHelper.readCell(currentCell);
                        Year year = Year.of(doubleValue.intValue());
                        LocalDate date = year.atDay(year.length());
                        String instantValue = date + instantTimeZone;
                        columnValues.add(Instant.parse(instantValue));
                    }
                } else {
                    columnValues.add(ExcelParserHelper.readCell(currentCell));
                }
            } catch (IllegalStateException | NullPointerException e) {
                columnValues.add("");
            }
        }
        return columnValues;
    }
}