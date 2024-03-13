package com.cmclinnovations.stack.clients.utils;

public class DateStringFormatter {

    private DateStringFormatter() {
        throw new IllegalStateException("Utility class");
    }

    private static int getCount(String string) {
        int max = 0;
        int count = 1;

        for (int i = 1; i < string.length(); i++) {
            if (string.charAt(i) == string.charAt(i - 1)) {
                count++;
            } else {
                max = Math.max(max, count);
                count = 1;
            }
        }
        return max;
    }

    /**
     * @param timeStringUnFormatted example: "yyyyddmm" or "yyyymm"
     * @return "yyyy/dd/mm" or "yyyy/mm"
     */
    public static String customDateStringFormatter(String unformatted) {

        String timeStringFormatted;

        // return the string without any delimiters if all characters are the same
        if (unformatted.chars().distinct().count() == 1) {
            timeStringFormatted = unformatted;
        } // add delimiters between different letters (assumed to be different time markers) based on length of input
        else {

            int firstPart = getCount(unformatted);
            int secondPart = getCount(unformatted.substring(firstPart));

            if (unformatted.length() > 8) {
                int thirdPart = getCount(unformatted.substring(firstPart + secondPart));

                timeStringFormatted = unformatted.substring(0, firstPart) + '/'
                        + unformatted.substring(firstPart, firstPart + secondPart) + '/'
                        + unformatted.substring(firstPart + secondPart, firstPart + secondPart + thirdPart)
                        + ':'
                        + unformatted.substring(firstPart + secondPart + thirdPart);
            } else if (unformatted.length() > 6) {
                timeStringFormatted = unformatted.substring(0, firstPart) + '/'
                        + unformatted.substring(firstPart, firstPart + secondPart) + '/'
                        + unformatted.substring(firstPart + secondPart);
            } else {
                timeStringFormatted = unformatted.substring(0, firstPart) + '/'
                        + unformatted.substring(firstPart);
            }
        }
        return timeStringFormatted;
    }
}
