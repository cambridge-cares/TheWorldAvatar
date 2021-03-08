/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.tools.util;

import java.util.ArrayList;

/**
 *
 * @author pb556
 */
public class StringUtil {

    private static char[] lowerCaseLetters = new char[]{'a', 'b', 'c', 'd', 'e', 'f',
        'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
        'u', 'v', 'w', 'x', 'y', 'z'};
    private static char[] upperCaseLetters = new char[]{'A', 'B', 'C', 'D', 'E', 'F',
        'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
        'U', 'V', 'W', 'X', 'Y', 'Z'};
    private static char[] numbers = new char[]{'0', '1', '2', '3', '4', '5', '6', '7',
        '8', '9', '.', ','};

    public static enum Category {

        LOWER,
        UPPER,
        NUMBER,
        SPECIAL,
        MIX_LOWER_UPPER,
        MIX_LOWER_NUMBER,
        MIX_UPPER_NUMBER,
        MIX_LOWER_SPECIAL,
        MIX_LOWER_UPPER_SPECIAL,
        MIX_LOWER_NUMPER_SPECIAL,
        MIX_UPPER_NUMBER_SPECIAL,
        MIX_NUMBER_SPECIAL,
        MIX_LOWER_UPPER_NUMBER,
        MIX_LOWER_UPPER_NUMBER_SPECIAL,
        NONE
    }

    public static boolean isLetter(char c) {
        return isLowerCase(c) || isUpperCase(c);
    }

    public static boolean areLetters(String s) {
        for (int i = 0; i < s.length(); i++) {
            if (!isLetter(s.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    public static boolean isNumber(char c) {
        if (c == ',' || c == '.') {
            return false;
        }
        return contains(c, numbers);
    }

    public static boolean isLowerCase(char c) {
        return contains(c, lowerCaseLetters);
    }

    public static boolean isUpperCase(char c) {
        return contains(c, upperCaseLetters);
    }

    public static boolean isSpecialCharacter(char c) {
        if (c == ',' || c == '.') {
            return true;
        }
        return !isLowerCase(c) && !isUpperCase(c) && !isNumber(c);
    }

    public static boolean isNumber(String s) {
        int ctrSeperator = 0;
        int start = 0;
        if (s.charAt(0) == '+' || s.charAt(0) == '-') {
            start = 1;
        }
        if (s.trim().equals(",") || s.trim().equals(".") || s.trim().equals("+") || s.trim().equals("-")) {
            return false;
        }
        for (int i = start; i < s.length(); i++) {
            if (!isNumber(s.charAt(i))) {
                if (s.charAt(i) == ',' || s.charAt(i) == '.') {
                    ctrSeperator++;
                    continue;
                }
                return false;
            } else if (s.charAt(i) == ',' || s.charAt(i) == '.') {
                ctrSeperator++;
            }
        }
        if (ctrSeperator > 1) {
            return false;
        }
        return true;
    }

    public static boolean isLowerCase(String s) {
        for (int i = 0; i < s.length(); i++) {
            if (!isLowerCase(s.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    public static boolean isUpperCase(String s) {
        for (int i = 0; i < s.length(); i++) {
            if (!isUpperCase(s.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    public static boolean isSpecialCharacter(String s) {
        for (int i = 0; i < s.length(); i++) {
            if (!isSpecialCharacter(s.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    public static boolean containsLowerCase(String s) {
        for (int i = 0; i < s.length(); i++) {
            if (isLowerCase(s.charAt(i))) {
                return true;
            }
        }
        return false;
    }

    public static boolean containsUpperCase(String s) {
        for (int i = 0; i < s.length(); i++) {
            if (isUpperCase(s.charAt(i))) {
                return true;
            }
        }
        return false;
    }

    public static boolean containsNumbers(String s) {
        for (int i = 0; i < s.length(); i++) {
            if (isNumber(s.charAt(i))) {
                return true;
            }
        }
        return false;
    }

    public static boolean containsSpecialCharacters(String s) {
        for (int i = 0; i < s.length(); i++) {
            if (isSpecialCharacter(s.charAt(i))) {
                return true;
            }
        }
        return false;
    }

    public static boolean containsLetters(String s) {
        for (int i = 0; i < s.length(); i++) {
            if (isLowerCase(s.charAt(i)) || isUpperCase(s.charAt(i))) {
                return true;
            }
        }
        return false;
    }

    public static boolean is(Category cat, char c) {
        switch (cat) {
            case LOWER:
                return isLowerCase(c);
            case UPPER:
                return isUpperCase(c);
            case SPECIAL:
                return isSpecialCharacter(c);
            case NUMBER:
                return isNumber(c);
            case MIX_LOWER_NUMBER:
                return false;
            case MIX_LOWER_NUMPER_SPECIAL:
                return false;
            case MIX_LOWER_SPECIAL:
                return false;
            case MIX_LOWER_UPPER:
                return false;
            case MIX_LOWER_UPPER_NUMBER:
                return false;
            case MIX_LOWER_UPPER_NUMBER_SPECIAL:
                return false;
            case MIX_LOWER_UPPER_SPECIAL:
                return false;
            case MIX_NUMBER_SPECIAL:
                return false;
            case MIX_UPPER_NUMBER:
                return false;
            case MIX_UPPER_NUMBER_SPECIAL:
                return false;
        }
        return false;
    }

    public static boolean is(Category cat, String s) {
        switch (cat) {
            case LOWER:
                return isLowerCase(s);
            case UPPER:
                return isUpperCase(s);
            case SPECIAL:
                return isSpecialCharacter(s);
            case NUMBER:
                return isNumber(s);
            case MIX_LOWER_NUMBER:
                return containsLowerCase(s) && !containsUpperCase(s) && containsNumbers(s) && !containsSpecialCharacters(s);
            case MIX_LOWER_NUMPER_SPECIAL:
                return containsLowerCase(s) && !containsUpperCase(s) && containsNumbers(s) && containsSpecialCharacters(s);
            case MIX_LOWER_SPECIAL:
                return containsLowerCase(s) && !containsUpperCase(s) && !containsNumbers(s) && containsSpecialCharacters(s);
            case MIX_LOWER_UPPER:
                return containsLowerCase(s) && containsUpperCase(s) && !containsNumbers(s) && !containsSpecialCharacters(s);
            case MIX_LOWER_UPPER_NUMBER:
                return containsLowerCase(s) && containsUpperCase(s) && containsNumbers(s) && !containsSpecialCharacters(s);
            case MIX_LOWER_UPPER_NUMBER_SPECIAL:
                return containsLowerCase(s) && containsUpperCase(s) && containsNumbers(s) && containsSpecialCharacters(s);
            case MIX_LOWER_UPPER_SPECIAL:
                return containsLowerCase(s) && containsUpperCase(s) && !containsNumbers(s) && containsSpecialCharacters(s);
            case MIX_NUMBER_SPECIAL:
                return !containsLowerCase(s) && !containsUpperCase(s) && containsNumbers(s) && containsSpecialCharacters(s);
            case MIX_UPPER_NUMBER:
                return !containsLowerCase(s) && containsUpperCase(s) && containsNumbers(s) && !containsSpecialCharacters(s);
            case MIX_UPPER_NUMBER_SPECIAL:
                return !containsLowerCase(s) && containsUpperCase(s) && containsNumbers(s) && containsSpecialCharacters(s);
        }
        return false;
    }

    public static Category getCategory(char c) {
        if (isLowerCase(c)) {
            return Category.LOWER;
        }
        if (isUpperCase(c)) {
            return Category.UPPER;
        }
        if (isNumber(c)) {
            return Category.NUMBER;
        }
        if (isSpecialCharacter(c)) {
            return Category.SPECIAL;
        }
        return Category.NONE;
    }

    public static Category getCategory(String s) {
        if (isLowerCase(s)) {
            return Category.LOWER;
        }
        if (isUpperCase(s)) {
            return Category.UPPER;
        }
        if (isNumber(s)) {
            return Category.NUMBER;
        }
        if (isSpecialCharacter(s)) {
            return Category.SPECIAL;
        }
        if (containsLowerCase(s) && containsUpperCase(s) && containsNumbers(s) && containsSpecialCharacters(s)) {
            return Category.MIX_LOWER_UPPER_NUMBER_SPECIAL;
        }
        if (containsLowerCase(s) && !containsUpperCase(s) && containsNumbers(s) && !containsSpecialCharacters(s)) {
            return Category.MIX_LOWER_NUMBER;
        }
        if (containsLowerCase(s) && !containsUpperCase(s) && containsNumbers(s) && containsSpecialCharacters(s)) {
            return Category.MIX_LOWER_NUMPER_SPECIAL;
        }
        if (containsLowerCase(s) && !containsUpperCase(s) && !containsNumbers(s) && containsSpecialCharacters(s)) {
            return Category.MIX_LOWER_SPECIAL;
        }
        if (containsLowerCase(s) && containsUpperCase(s) && !containsNumbers(s) && !containsSpecialCharacters(s)) {
            return Category.MIX_LOWER_UPPER;
        }
        if (containsLowerCase(s) && containsUpperCase(s) && containsNumbers(s) && !containsSpecialCharacters(s)) {
            return Category.MIX_LOWER_UPPER_NUMBER;
        }
        if (containsLowerCase(s) && containsUpperCase(s) && !containsNumbers(s) && containsSpecialCharacters(s)) {
            return Category.MIX_LOWER_UPPER_SPECIAL;
        }
        if (!containsLowerCase(s) && !containsUpperCase(s) && containsNumbers(s) && containsSpecialCharacters(s)) {
            return Category.MIX_NUMBER_SPECIAL;
        }
        if (!containsLowerCase(s) && containsUpperCase(s) && containsNumbers(s) && !containsSpecialCharacters(s)) {
            return Category.MIX_UPPER_NUMBER;
        }
        if (!containsLowerCase(s) && containsUpperCase(s) && containsNumbers(s) && containsSpecialCharacters(s)) {
            return Category.MIX_UPPER_NUMBER_SPECIAL;
        }

        return Category.NONE;
    }

    public static boolean contains(char c, char[] set) {
        for (int i = 0; i < set.length; i++) {
            if (c == set[i]) {
                return true;
            }
        }
        return false;
    }

    public static boolean contains(String s, char[] set) {
        for (int i = 0; i < s.length(); i++) {
            if (!contains(s.charAt(i), set)) {
                return false;
            }
        }
        return true;
    }

    public static String[] extractNumbers(String s) {
        ArrayList<String> extracted = new ArrayList<String>();
        String buffer = "";
        for (int i = 0; i < s.length(); i++) {
            if (isNumber(s.charAt(i))) {
                buffer += s.charAt(i);
            } else {
                if (!buffer.isEmpty()) {
                    extracted.add(buffer);
                    buffer = "";
                }
            }
        }
        if (!buffer.isEmpty()) {
            extracted.add(buffer);
        }
        String[] extractedNumbers = new String[extracted.size()];
        for (int i = 0; i < extracted.size(); i++) {
            extractedNumbers[i] = extracted.get(i);
        }
        return extractedNumbers;
    }
}
