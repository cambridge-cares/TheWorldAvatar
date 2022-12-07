package uk.ac.cam.cares.jps.agent;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.DateUtil;

/**
 * Provide utility functions for the Excel parser.
 *
 * @author qhouyee
 */
class ExcelParserHelper {
    // Logger for reporting info/errors.
    private static final Logger LOGGER = LogManager.getLogger(HistoricalPumpDataInstantiationAgent.class);
    protected static final String BASE_KEY_FOR_SINGLE_TIMESERIES = "base";

    /**
     * Retrieves the cell value as a formatted String for data IRI. Removes any characters after Brackets.
     * For eg, an input of "Time Series (Unit)" will return "TimeSeries"
     *
     * @param cell Text cell to retrieved String from in Excel
     * @return String in format "CapitalisedFirstWordAfterSpace"
     */
    protected static String formatHeaderStringCell(Cell cell) {
        LOGGER.debug("Formatting header...");
        // Retrieve cell and stores them in lowercase
        String phrase = cell.getStringCellValue().toLowerCase();
        // Remove description or information included as brackets () if they exist
        int startingIndexToRemove = phrase.indexOf("(");
        phrase = startingIndexToRemove == -1 ? phrase : phrase.substring(0, startingIndexToRemove);
        // Remove all special characters
        phrase = phrase.replaceAll("[$&+,:;=?@#|'<>.^*()%!-]", "");
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
     * Read the cell content in the proper data type format
     *
     * @param cell Excel cell input
     * @return Cell content as the right data type
     */
    protected static Object readCell(Cell cell) throws IllegalStateException, NullPointerException {
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
