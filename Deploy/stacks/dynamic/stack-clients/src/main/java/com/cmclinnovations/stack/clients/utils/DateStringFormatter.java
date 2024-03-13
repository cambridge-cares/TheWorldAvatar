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
     * @param template is the actual template in the config file of the data uploader
     * @param unormatted example: "yyyyddmm" or "yyyymm"
     * @return "yyyy/dd/mm" or "yyyy/mm"
     */
    public static String customDateStringFormatter(String unformatted, String template) {

        String timeStringFormatted;

        if (template.chars().distinct().count() == 1) {
            timeStringFormatted = unformatted;
        } else {

            int firstPart = getCount(template);
            int secondPart = getCount(template.substring(firstPart));
            
            if (unformatted.length() > 8) {
                int thirdPart = getCount(template.substring(firstPart + secondPart));

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
