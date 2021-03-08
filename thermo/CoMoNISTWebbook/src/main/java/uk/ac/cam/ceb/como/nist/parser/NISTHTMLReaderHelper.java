/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.nist.parser;

import uk.ac.cam.ceb.como.io.chem.file.parser.g09.util.StringList;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.io.IOUtils;

/**
 *
 * @author pb556
 */
public class NISTHTMLReaderHelper {

    public static StringList extractHTMLBody(File f) throws FileNotFoundException, IOException {
        return extractHTMLContent(f, "<body", "</body>");
    }

    public static StringList extractHTMLContent(File f, String startIdentifier, String endIdentifier) throws FileNotFoundException, IOException {
        BufferedReader r = null;
        try {
            r = new BufferedReader(new FileReader(f));
            List<String> section_lines = new ArrayList<String>();
            String l;
            boolean start = false;
            boolean end = false;
            while ((l = r.readLine()) != null) {
                if (l != null) {
                    if (!start) {
                        start = l.trim().contains(startIdentifier);
                    }
                    end = l.trim().contains(endIdentifier);
                }
                if (start) {
                    section_lines.add(l);
                }
                if (end || l == null) { // end of section
                    IOUtils.closeQuietly(r);
                    return new StringList(section_lines);
                }
            }
        } finally {
            IOUtils.closeQuietly(r);
        }
        return new StringList();
    }

    public static StringList extractHTMLContent(StringList content, String startIdentifier, String endIdentifier) {
        List<String> section_lines = new ArrayList<String>();
        String l;
        boolean start = false;
        boolean end = false;
        int i = 0;
        while ((l = content.get(i)) != null) {
            if (l != null) {
                if (!start) {
                    start = l.trim().contains(startIdentifier);
                }
                end = l.trim().contains(endIdentifier);
            }
            if (start) {
                section_lines.add(l);
            }
            if ((end && start) || l == null) { // end of section
                return new StringList(section_lines);
            }
            i++;
            if (i >= content.size()) {
                break;
            }
        }
        return new StringList(section_lines);
    }

    public static StringList extractTable(int indexFrom, StringList content) {
        String l;
        List<String> section_lines = new ArrayList<String>();
        boolean start = false;
        boolean end = false;
        for (int i = indexFrom; i < content.size(); i++) {
            l = content.get(i);
            if (l != null) {
                if (!start) {
                    start = l.trim().contains("<table");
                }
                end = l.trim().contains("</table>");
            }
            if (start) {
                section_lines.add(l);
            }
            if (end || l == null) { // end of section
                return new StringList(section_lines);
            }
        }
        return new StringList();
    }

    public static ArrayList<ArrayList<String>> extractTableCellEntries(StringList table) {
        ArrayList<String> rows = extractTableRows(table);
        ArrayList<ArrayList<String>> data = new ArrayList<ArrayList<String>>();
        for (int i = 0; i < rows.size(); i++) {
            data.add(extractEntries(rows.get(i), "<td"));
        }
        return data;
    }

    public static ArrayList<String> extractEntries(StringList row, String tag) {
        ArrayList<String> cols = new ArrayList<String>();
        String buffer = "";
        boolean start = false;
        for (int i = 0; i < row.size(); i++) {
            String line = row.get(i);
            for (int j = 0; j < line.length(); j++) {
                char c = line.charAt(j);
                if (!start) {
                    if (c == tag.charAt(0)) {
                        if ((j + tag.length() - 1) < line.length()) {
                            start = true;
                            for (int k = 1; k < tag.length(); k++) {
                                if (line.charAt(j + k) != tag.charAt(k)) {
                                    start = false;
                                    break;
                                }
                            }
                            if (start) {
                                buffer = "";
                            }
                        }
                    }
                } else if (c == tag.charAt(0)) {
                    if ((j + tag.length()) < line.length()) {
                        start = true;
                        if (line.charAt(j + 1) == '/') {
                            boolean valid = true;
                            String temp = "/";
                            for (int k = 1; k < tag.length(); k++) {
                                if (line.charAt(j + k + 1) != tag.charAt(k)) {
                                    valid = false;
                                    temp = "";
                                    break;
                                } else {
                                    temp += line.charAt(j + k + 1);
                                }
                            }
                            if (valid) {
                                if (!buffer.isEmpty()) {
                                    buffer += c + temp + ">";
                                    cols.add(buffer);
                                    buffer = "";
                                    start = false;
                                }
                            }
                        }
                    }

                }
                buffer += c;
            }
        }

        if (!buffer.isEmpty() && start) {
            cols.add(buffer);
        }
        return cols;
    }

    public static ArrayList<String> extractEntries(String data, String tag) {
        StringList s = new StringList();
        s.add(data);
        return extractEntries(s, tag);
    }

    public static ArrayList<String> extractTableRows(StringList table) {
        return extractEntries(table, "<tr");
    }

    public static String removeTags(String s) {
        // removes tags and returns the modified string
        String newStr = "";
        ArrayList<String> list = extractContent(s);
        for (String item : list) {
            newStr += item;
        }
        return newStr;
    }

    public static ArrayList<String> extractContent(String s) {
        // removes tags and returns the modified string
        ArrayList<String> content = new ArrayList<String>();
        String buffer = "";
        for (int i = 0; i < s.length(); i++) {
            //if (remove) {
            if (s.charAt(i) == '>') {
                buffer = "";
                continue;
            }
            //} else 
            if (s.charAt(i) == '<') {
                if (buffer.length() != 0) {
                    content.add(buffer);
                    buffer = "";
                }
                continue;
            } else {
                buffer += s.charAt(i);
            }
        }
        if (buffer.length() != 0) {
            content.add(buffer);
        }
        return content;
    }

    public static ArrayList<String> extractTagValue(String tag, String label) {
        // removes tags and returns the modified string
        ArrayList<String> content = new ArrayList<String>();

        Pattern p = Pattern.compile(".*" + label + "\\s*=\\s*\\\"(.*)\\\".*");
        Matcher m = p.matcher(tag);
        if (m.find()) {
            for (int i = 0; i < m.groupCount(); i++) {
                content.add(m.group(i + 1));
            }
        }
        return content;
    }
}
