package uk.ac.cam.cares.jps.agent.ifc2ontobim.ttlparser;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Provide methods to sort and groups the triples in readable TTL format.
 *
 * @author qhouyee
 */
class FileContentSorter {
    private static Integer index;
    private static Map<String, Integer> subjectIndexMap;

    /**
     * Group the triples in the file specified. Each subject will be linked to all their properties using multiline syntax.
     *
     * @param contents     The file contents on a per-line basis.
     * @param classMapping A mapping to relate data IRI of generic elements to their ontoBIM class for renaming.
     * @return The contents of the entire file in TTL format as a string builder.
     */
    protected static StringBuilder groupTriples(List<String> contents, Map<String, String> classMapping) {
        // Sort the contents to ensure the groups are grouped together sequentially
        List<String> parsedContents = parseLines(contents, classMapping);
        // Sorting the string with our own string comparator
        Collections.sort(parsedContents, new Comparator<String>() {
            public int compare(String o1, String o2) {
                return extractIndex(o1) - extractIndex(o2);
            }

            int extractIndex(String s) {
                Matcher matcher = Pattern.compile("^(\\d+)").matcher(s); // Retrieve the appended index at the start
                String num = matcher.find() ? matcher.group(1) : ""; // Return match values if matched, else, return empty string.
                return num.isEmpty() ? 0 : Integer.parseInt(num); // return 0 if no digits found
            }
        });
        return parseGroupStatements(parsedContents);
    }

    /**
     * Parses each line to get the right format.
     *
     * @param contents     The file contents on a per-line basis.
     * @param classMapping A mapping to relate data IRI of generic elements to their ontoBIM class for renaming.
     * @return The sorted file contents.
     */
    private static List<String> parseLines(List<String> contents, Map<String, String> classMapping) {
        index = 1;
        subjectIndexMap = new HashMap<>();    // Create mappings between each subject and their index
        List<String> parsedContents = new ArrayList<>(); // Create an empty list to store parsed lines

        for (String line : contents) {
            line = (classMapping.size() > 0) ? renameInstances(line, classMapping) : line;
            appendSortingIndex(line, parsedContents);
        }
        return parsedContents;
    }

    /**
     * Rename the instances to their ontoBIM class stated in the classMapping variable if it exists.
     * Otherwise, keep the same names.
     *
     * @param line         Current line to rename instances, usually in the form "inst:Class_1234 rdf:predicate object:ObjectName."
     * @param classMapping Maps the instances to their correct ontoBIM class for renaming.
     * @return The sorted file contents.
     */
    private static String renameInstances(String line, Map<String, String> classMapping) {
        // Retrieves the subject and object components that we need
        String subjectPrefix = StringUtils.getStringBeforeFirstCharacterOccurrence(line, ":");
        String subjectName = StringUtils.getStringAfterFirstCharacterOccurrence(line, ":");
        subjectName = StringUtils.getStringBeforeFirstCharacterOccurrence(subjectName, " ");
        String object = StringUtils.getStringAfterNCharacterOccurrence(line, " ",2);
        String objectName = StringUtils.getStringAfterFirstCharacterOccurrence(object, ":");

        // If neither the subject or object is found in the class mapping, return the line, and stop processing
        if (classMapping.get(subjectName) == null && classMapping.get(objectName) == null) {
            return line;
        }

        // Retrieve the remaining nodes parts we require
        String predicate = StringUtils.getStringAfterFirstCharacterOccurrence(line, " ");
        predicate = StringUtils.getStringBeforeFirstCharacterOccurrence(predicate, " ");
        String objectPrefix = StringUtils.getStringBeforeFirstCharacterOccurrence(object, ":") + ":";

        // When subject name should be renamed
        if (classMapping.get(subjectName) != null) {
            String identifier = StringUtils.getStringAfterFirstCharacterOccurrence(subjectName, "_");
            String bimClassName = classMapping.get(subjectName);
            subjectName = bimClassName + "_" + identifier;
            // Check that instance type is correct
            if (predicate.equals("rdf:type")) {
                if (objectName.startsWith("Ifc")) {
                    object = objectPrefix + bimClassName;
                }
            }
        }
        // When object name should be renamed
        if (classMapping.get(objectName) != null) {
            String identifier = StringUtils.getStringAfterFirstCharacterOccurrence(objectName, "_");
            object = objectPrefix + classMapping.get(objectName) + "_" + identifier;
        }
        // Create the parsedLine
        String parsedLine = subjectPrefix + ":" + subjectName + " " + predicate + " " + object;
        return parsedLine;
    }

    /**
     * Appends a sorting index to each line based on their subject. This will make it easier to sort and group the same
     * subjects in the same order.
     *
     * @param line           Current line to append the index.
     * @param parsedContents List to add the parsed contents.
     * @return The sorted file contents.
     */
    private static List<String> appendSortingIndex(String line, List<String> parsedContents) {
        String subject = StringUtils.getStringBeforeFirstCharacterOccurrence(line, " ");
        String parsedLine = "";
        // When the subject has only appeared once, add the subject and the current index to the hashmap
        if (subjectIndexMap.get(subject) == null) {
            String indexString = index.toString();
            parsedLine += indexString + " " + line;
            subjectIndexMap.put(subject, index);
            index++;
        } else {
            // When the subject has appeared at least twice, retrieve their index and append it to the line
            parsedLine += subjectIndexMap.get(subject) + " " + line;
        }
        parsedContents.add(parsedLine);
        return parsedContents;
    }

    /**
     * Parses the statement into a block TTL format that is readable and valid.
     * Removes the appended index, and formats accordingly to the multi-line or one-line statements.
     *
     * @param contents The sorted file contents on a per-line basis.
     * @return The contents of the entire file in TTL format as a string builder.
     */
    private static StringBuilder parseGroupStatements(List<String> contents) {
        int prevIndex = -1;
        int currentIndex;
        StringBuilder sbuilder = new StringBuilder();

        for (String line : contents) {
            // Set the current index
            currentIndex = Integer.parseInt(StringUtils.getStringBeforeFirstCharacterOccurrence(line, " "));
            // Run code when this is the first line
            if (prevIndex == -1) {
                sbuilder.append("\n\n") // Add a line break to discern from previous copy
                        .append(StringUtils.getStringAfterFirstCharacterOccurrence(line, " ")); // Starts the triple for a new subject
                prevIndex = currentIndex;
            } // Run code when moving to a new subject that differs from the previous subject node
            else if (currentIndex != prevIndex && prevIndex != -1) {
                sbuilder.append(".\n\n") // Add a "." Turtle syntax to denote the triples for the previous subject has ended
                        .append(StringUtils.getStringAfterFirstCharacterOccurrence(line, " ")); // Starts the triple for a new subject
                prevIndex = currentIndex;
            } else { // Run code when current statement has the same subject node as the previous statement
                sbuilder.append(";\n") // Add a ";" Turtle syntax to denote the same subject has other properties as a multiline code
                        .append("\t")
                        .append(StringUtils.getStringAfterNCharacterOccurrence(line, " ", 2)); // Append only the properties (predicate + object)
            }
        }
        // Add the "." Turtle syntax after the last line
        sbuilder.append(".");
        return sbuilder;
    }
}
