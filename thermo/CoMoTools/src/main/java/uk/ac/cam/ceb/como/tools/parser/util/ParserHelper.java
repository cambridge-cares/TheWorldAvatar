/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.tools.parser.util;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 *
 * @author pb556
 */
public class ParserHelper {

    // gives only first match
    public static String parseLine(Pattern pattern, String str) {
        Matcher m = pattern.matcher(str);
        if (m.matches()) {
            return m.group(1);
        }
        return null;
    }

    public static List<String> tokenize(String str) {
        List<String> str_list = new ArrayList<String>();
        StringTokenizer tempStringTokenizer = new StringTokenizer(str);
        while (tempStringTokenizer.hasMoreTokens()) {
            str_list.add(((String) tempStringTokenizer.nextElement()).trim());
        }
        return str_list;
    }

    public static List<Double> parseDoubles(List<String> dbs) {
        List<Double> list_dbs = new ArrayList<Double>();
        for (String db : dbs) {
            list_dbs.add(Double.parseDouble(db));
        }
        return list_dbs;
    }

    public static List<Double> extractDoubles(List<String> dbs) {
        return parseDoubles(extractStrDoubles(dbs));
    }

    public static List<Integer> extractNumbers(List<String> dbs) {
        List<Integer> numbers = new ArrayList<Integer>();
        for (String line : dbs) {
            if (line == null) {
                continue;
            }
            Pattern p = Pattern.compile("\\d+");
            Matcher m = p.matcher(line);
            while (m.find()) {
                numbers.add(Integer.parseInt(m.group()));
            }
        }
        return numbers;
    }

    public static List<Integer> extractIntegers(List<String> dbs) {
        List<String> numbers = extractStrDoubles(dbs);
        ArrayList<String> newList = new ArrayList<String>();
        for (String line : dbs) {
            for (String v : numbers) {
                line = line.replace(v, "");
            }
            newList.add(line);
        }

        return extractNumbers(newList);
    }

    private static List<String> extractStrDoubles(List<String> dbs) {
        ArrayList<String> numbers = new ArrayList<String>();
        for (String line : dbs) {
            if (line == null) {
                continue;
            }
            Pattern p = Pattern.compile("(-?\\d+.\\d+)");
            Matcher m = p.matcher(line);
            while (m.find()) {
                numbers.add(m.group());
            }
        }
        return numbers;
    }
}
