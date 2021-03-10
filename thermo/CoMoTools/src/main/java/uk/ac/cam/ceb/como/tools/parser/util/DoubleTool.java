package uk.ac.cam.ceb.como.tools.parser.util;

import java.util.regex.Pattern;

/**
 *
 * @author pb556
 */
public abstract class DoubleTool {

    /**
     * A regular expression match a number pattern. See examples of number formats in test.
     */
    public static final String NUMBER_REGEX = "(?:[+-]?(?:(?:\\d*(?:\\.?\\d+)?)|(?:\\d+(?:\\.?\\d*)?))(?:[EeDdGgHh][+-]?\\d+[dDfF]?)?)";
    //  sign? |                     number                   |      exponent?      | precision?
    /**
     * A compiled Pattern object which matches a number pattern.
     */
    public static final Pattern NUMBER_PATTERN = Pattern.compile(NUMBER_REGEX);

    /**
     * Parse a string to double value, similar to Double.parseDouble function, but
     * also try to parse against FORTRAN number, e.g. 12.3D-05.
     *
     * <p>If the return value is Double.NaN, a RuntimeException is thrown.
     * This should not happen anyway.</p>
     *
     * @param db String of number value
     * @return double value of the given String
     * @throws NullPointerException if db is null.
     * @throws NumberFormatException if db is not a number.
     */
    public static double parseDouble(String db) {
        double d = Double.NaN;
        db = db.trim();
        // Try to parse string using java routine first. If it does not match
        // the string could be number in FORTRAN format, [DdGgHh] as exponential
        // notation. So we replace the first exponential notation by E and then
        // try to parse again with java routine. The two steps are necessary and
        // cannot be removed otherwise the number such as 12.0d will not be parsed.
        try {
            d = Double.parseDouble(db);
        } catch (NumberFormatException nfe) {
            d = Double.parseDouble(db.replaceFirst("[DdGgHh]", "E"));
        }
        if (d == Double.NaN) {
            throw new RuntimeException("Cannot parse {" + db + "} as double and cannot throw NumberFormatException. This is a program bug.");
        }
        return d;
    }
}
