package uk.ac.cam.cares.jps.agent.ifc2ontobim.utils;

/**
 * Provides all the utility methods for handling strings.
 *
 * @author qhouyee
 */
public class StringUtils {
    public static final String UNDERSCORE = "_";
    public static final String SLASH = "/";

    /**
     * Retrieve the substring from the starting character to the just before the last appearance of a specified character.
     * Eg for "string utils last text" and character " ", this will retrieve "string utils last".
     *
     * @param line      A line of text.
     * @param character A specific character of interest to stop at.
     * @return The substring of interest.
     */
    public static String getStringBeforeLastCharacterOccurrence(String line, String character) {
        try {
            return line.substring(0, line.lastIndexOf(character));
        } catch (StringIndexOutOfBoundsException e) {
            return line;
        }
    }

    /**
     * Retrieve the substring from after the nth appearance of a specified character and onwards.
     * Eg for "string utils last text" and the second character " ", this will retrieve "last text".
     *
     * @param line      A line of text.
     * @param character A specific character of interest to stop at.
     * @param n         The nth character appearance, that starts from 1.
     * @return The substring of interest.
     */
    public static String getStringAfterNCharacterOccurrence(String line, String character, int n) {
        if (n == 1) {
            return getStringAfterFirstCharacterOccurrence(line, character);
        } else {
            String subLine = getStringAfterFirstCharacterOccurrence(line, character);
            return getStringAfterNCharacterOccurrence(subLine, character, n - 1);
        }
    }

    /**
     * Retrieve the substring from after the first appearance of a specified character and onwards.
     * Eg for "string utils" and character " ", this will retrieve "utils".
     *
     * @param line      A line of text.
     * @param character A specific character of interest to stop at.
     * @return The substring of interest.
     */
    public static String getStringAfterFirstCharacterOccurrence(String line, String character) {
        return line.substring(line.indexOf(character) + 1);
    }

    /**
     * Retrieve the substring from after the last appearance of a specified character and onwards.
     * Eg for "string utils last text" and character " ", this will retrieve "text".
     *
     * @param line      A line of text.
     * @param character A specific character of interest to stop at.
     * @return The substring of interest.
     */
    public static String getStringAfterLastCharacterOccurrence(String line, String character) {
        return line.substring(line.lastIndexOf(character) + 1);
    }
}
