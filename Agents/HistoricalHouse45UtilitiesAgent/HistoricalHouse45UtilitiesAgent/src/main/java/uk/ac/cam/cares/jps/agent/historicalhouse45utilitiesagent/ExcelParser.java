package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.*;
import java.util.*;
import java.time.LocalDate;

import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.DateUtil;

/**
 * Parses the readings from the Excel workbook into a Java collection that can be read by the time series client.
 *
 * @author qhouyee
 */
class ExcelParser {
    // Logger for reporting info/errors.
    private static final Logger LOGGER = LogManager.getLogger(HistoricalHouse45UtilitiesAgent.class);
    // Error messages
    private static final String FILE_NOT_FOUND_MSG = "No excel file found at specified filepath: %s";
    private static final String EXCEL_ERROR_MSG = "Data could not be retrieved from Excel";
    private final String EXCEL_FILE_PATH;
    private int sheetIndex = 0;
    private Sheet dataSheet;
    private int[] parserDateArray;
    private final String dateKey;
    private Map<String, List<?>> excelValues;


    /**
     * Standard constructor which checks for a valid File path
     *
     * @param excel_filepath Specifies the file path to the Excel workbook.
     */
    protected ExcelParser(String excel_filepath, String dateKey) throws FileNotFoundException {
        File file = new File(excel_filepath);
        if (!file.exists()) {
            LOGGER.fatal(FILE_NOT_FOUND_MSG);
            throw new FileNotFoundException(FILE_NOT_FOUND_MSG + excel_filepath);
        }
        this.EXCEL_FILE_PATH = excel_filepath;
        this.dateKey = dateKey;
    }

    /**
     * Optional constructor which checks for a valid File path and modifies the sheetIndex value
     * By default, it has been set to the first sheet at index 0
     *
     * @param excel_filepath Specifies the file path to the Excel workbook.
     * @param sheetIndex     Specifies the sheet index to read in the Excel file.
     */
    protected ExcelParser(String excel_filepath, String dateKey, int sheetIndex) throws IOException {
        File file = new File(excel_filepath);
        if (!file.exists()) {
            LOGGER.fatal(FILE_NOT_FOUND_MSG);
            throw new FileNotFoundException(FILE_NOT_FOUND_MSG);
        }
        this.EXCEL_FILE_PATH = excel_filepath;
        this.dateKey = dateKey;
        if (sheetIndex < 0) {
            throw new IOException("Sheet Index is invalid, please input a integer starting from 0");
        }
        this.sheetIndex = sheetIndex;
    }

    /**
     * Use this overload method when Date data is available in one column.
     * Parses the values stored in Excel into a HashMap
     *
     * @param rowStart Starting row number that contains values, not headings. Start from index 0
     * @return Excel values as a HashMap containing {dataIRI prefix: List of readings} key value pairs
     */
    protected Map<String, List<?>> parseToHashMap(int rowStart) {
        try {
            return retrieveExcelValues(rowStart);
        } catch (Exception e) {
            LOGGER.error(EXCEL_ERROR_MSG, e);
            throw new JPSRuntimeException(EXCEL_ERROR_MSG, e);
        }
    }

    /**
     * Use this overload method for Date data that is split over multiple columns.
     * Parses the values stored in Excel into a HashMap.
     *
     * @param rowStart  Starting row number that contains values, not headings. Start from index 0
     * @param dateArray An array of size 3, containing the column index of day, month, and year data
     * @return Excel values as a HashMap containing {dataIRI prefix: List of readings} key value pairs
     */
    protected Map<String, List<?>> parseToHashMap(int rowStart, int[] dateArray) {
        try {
            setParserDateArray(dateArray);
            return retrieveExcelValues(rowStart);
        } catch (Exception e) {
            LOGGER.error(EXCEL_ERROR_MSG, e);
            throw new JPSRuntimeException(EXCEL_ERROR_MSG, e);
        }
    }

    /**
     * Retrieves the values stored in Excel
     *
     * @param rowStart Starting row number that contains values, not headings. Start from index 0
     * @return Excel values as a HashMap object
     */
    private Map<String, List<?>> retrieveExcelValues(int rowStart) {
        try (InputStream excelFile = new FileInputStream(this.EXCEL_FILE_PATH);
             Workbook workbook = WorkbookFactory.create(excelFile)) {
            this.dataSheet = workbook.getSheetAt(this.sheetIndex);
            this.excelValues = parseSheetToMap(rowStart);
        } catch (IOException e) {
            e.printStackTrace();
        }
        return this.excelValues;
    }

    /**
     * Parses the Excel worksheet into a Hashmap
     *
     * @param rowStart starting row number that contains values, not headings. Start from index 0
     * @return Excel values as a HashMap object
     */
    private Map<String, List<?>> parseSheetToMap(int rowStart) {
        List<?> columnValues;
        this.excelValues = new HashMap<>();
        List<String> columnHeaders = parseHeadings(rowStart);

        // If Date data is split across multiple column, search and combine those columns into one column
        if (this.parserDateArray != null) {
            this.excelValues.put(this.dateKey, parseMultiColumnDate(rowStart));
        }

        // Iterate over each row and store all column values into a list
        // Index starts from 0 while size start from 1
        for (int columnIndex = 0; columnIndex < columnHeaders.size(); columnIndex++) {
            // Run when Date data is not split
            if (this.parserDateArray == null) {
                columnValues = parseColValuesToList(rowStart, columnIndex);
                this.excelValues.put(columnHeaders.get(columnIndex), columnValues);
            } else if (this.parserDateArray != null) {
                // Skip the columns that contain Date data only if Date data is split across columns
                if (columnIndex != this.parserDateArray[0] && columnIndex != this.parserDateArray[1] && columnIndex != this.parserDateArray[2]) {
                    columnValues = parseColValuesToList(rowStart, columnIndex);
                    this.excelValues.put(columnHeaders.get(columnIndex), columnValues);
                }
            }
        }
        return this.excelValues;
    }

    /**
     * Parse the column headings contain in the first or multiple rows into one string for prefixing dataIRI
     *
     * @param rowStart starting row number that contains values, ignoring the headings
     * @return ArrayList containing all the headers in "firstrowcell_secondrowcell_thirdrowcell" format
     */
    private List<String> parseHeadings(int rowStart) throws IllegalStateException, NullPointerException {
        List<String> columnHeaders = new ArrayList<>();
        String headerContent;
        for (int i = 0; i < rowStart; i++) {
            Row row = this.dataSheet.getRow(i);
            for (int j = 0; j < row.getLastCellNum(); j++) {
                Cell cell = row.getCell(j);

                // When cell is null, cell.getStringCellValue will throw an exception and break compilation
                headerContent = (cell != null) ? formatHeaderStringCell(cell) : "";

                if (i == 0) {
                    // When current row is first row, add a new (empty or not) String to Array List
                    // Empty strings are vital as a placeholder to prevent skipping, which leads to inaccurate size
                    columnHeaders.add(headerContent);
                } else {
                    if (cell != null && !columnHeaders.get(j).isEmpty() && !cell.getStringCellValue().isEmpty()) {
                        // Retrieve the existing String stored and concatenate the new cell content
                        String newHeaderContent = columnHeaders.get(j) + "_" + headerContent;
                        columnHeaders.set(j, newHeaderContent);
                    } else if (columnHeaders.get(j).isEmpty()) {
                        columnHeaders.set(j, headerContent);
                    }
                }
            }
        }
        return columnHeaders;
    }

    /**
     * Retrieves the cell value as a formatted String for data IRI.
     *
     * @param cell Text cell to retrieved String from in Excel
     * @return String in format "CapitalisedFirstWordAfterSpace"
     */
    private String formatHeaderStringCell(Cell cell) {
        // Retrieve cell and stores them in lowercase
        String phrase = cell.getStringCellValue().toLowerCase();
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
     * Parses day, month, year data into ONE LocalDate object for time series instantiation
     * Used only when Date is stored in multiple columns
     *
     * @param rowStart Starting row number that contains values, not headings. Start from index 0
     * @return List of LocalDate timestamp objects
     */
    private List<LocalDate> parseMultiColumnDate(int rowStart) {
        List<?> day, month, year;
        List<LocalDate> timestamp = new ArrayList<>();

        day = parseColValuesToList(rowStart, this.parserDateArray[0]);
        month = parseColValuesToList(rowStart, this.parserDateArray[1]);
        year = parseColValuesToList(rowStart, this.parserDateArray[2]);

        for (int i = 0; i < day.size(); i++) {
            // X.get(i) returns a Double reference type
            // This should be cast to a double primitive type before it can be cast to an int primitive type
            double yearValue = (Double) year.get(i);
            double monthValue = (Double) month.get(i);
            double dayValue = (Double) day.get(i);
            timestamp.add(LocalDate.of((int) yearValue, (int) monthValue, (int) dayValue));
        }
        return timestamp;
    }

    /**
     * Stores the values of one column into a list
     *
     * @param rowStart Starting row number that contains values, not headings. Start from index 0
     * @return A list object containing all the values in the cell
     */
    private List<?> parseColValuesToList(int rowStart, int columnIndex) throws IllegalArgumentException {
        if (rowStart < 1) {
            throw new IllegalArgumentException("Input for rowStart is invalid, please input an integer from 1.");
        }

        List<Object> columnValues = new ArrayList<>();
        for (int i = rowStart; i <= this.dataSheet.getLastRowNum(); i++) {
            Row currentRow = this.dataSheet.getRow(i);
            Cell currentCell = currentRow.getCell(columnIndex);
            // Add cell values into a List
            try {
                columnValues.add(readCell(currentCell));
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

    /**
     * Get the field DateArray in this class
     */
    public int[] getParserDateArray() {
        return this.parserDateArray;
    }

    /**
     * Sets the field DateArray in this class
     *
     * @param parserDateArray Array object of size 3 containing the day, month, and year column index
     */
    public void setParserDateArray(int[] parserDateArray) throws IllegalArgumentException {
        if (parserDateArray.length < 3) {
            throw new IllegalArgumentException("dateArray contains less than 3 arguments and is invalid.");
        } else if (parserDateArray.length > 3) {
            throw new IllegalArgumentException("dateArray contains more than 3 arguments and is invalid.");
        }
        this.parserDateArray = parserDateArray;
    }
}