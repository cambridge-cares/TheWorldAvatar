package com.cmclinnovations.agent.utils;

import java.util.List;

public class ShaclResource {
  // JSON LD keys
  public static final String CONTEXT_KEY = "@context";
  public static final String ID_KEY = "@id";
  public static final String TYPE_KEY = "@type";
  public static final String VAL_KEY = "@value";
  public static final String REPLACE_KEY = "@replace";
  public static final String REVERSE_KEY = "@reverse";
  public static final String UNIT_KEY = "unit";
  public static final String VARIABLE_KEY = "variable";
  public static final String OUTPUT_KEY = "output";
  // Namespaces
  public static final String BASE_PREFIX = "https://www.theworldavatar.com/kg/";
  public static final String RDFS_PREFIX = "http://www.w3.org/2000/01/rdf-schema#";
  public static final String SHACL_PREFIX = "http://www.w3.org/ns/shacl#";
  public static final String XSD_PREFIX = "http://www.w3.org/2001/XMLSchema#";
  // SHACL classes
  public static final String PROPERTY_GROUP = "PropertyGroup";
  public static final String PROPERTY_SHAPE = "PropertyShape";
  // SHACL properties
  public static final String COMMENT_PROPERTY = "comment";
  public static final String LABEL_PROPERTY = "label";
  public static final String NAME_PROPERTY = "name";
  public static final String DESCRIPTION_PROPERTY = "description";
  public static final String ORDER_PROPERTY = "order";
  public static final String GROUP_PROPERTY = "group";
  public static final String PROPERTY_PROPERTY = "property";
  public static final String DEFAULT_VAL_PROPERTY = "defaultValue";
  public static final String CLASS_PROPERTY = "class";
  public static final String DATA_TYPE_PROPERTY = "datatype";
  public static final String IN_PROPERTY = "in";
  // Query string elements
  public static final String RDFS_LABEL_PREDICATE = "rdfs:label";
  public static final String FULL_STOP = ".";
  public static final String VARIABLE_MARK = "?";
  public static final String REPLACEMENT_ENDPOINT = "[endpoint]";
  public static final String WHITE_SPACE = " ";
  public static final String UNION_OPERATOR = "} UNION {";

  private ShaclResource() {
  }

  /**
   * Method to support the comparison of two lists containing integers.
   * 
   * @param list1 First list of integers.
   * @param list2 Second list of integers.
   */
  public static int compareLists(List<Integer> list1, List<Integer> list2) {
    int size1 = list1.size();
    int size2 = list2.size();
    int minSize = Math.min(size1, size2);

    // Compare element by element
    for (int i = 0; i < minSize; i++) {
      int difference = Integer.compare(list1.get(i), list2.get(i));
      if (difference != 0) {
        return difference; // Return at the first difference
      }
    }

    // If all compared elements are equal, the shorter list is smaller
    return Integer.compare(size1, size2);
  }
}
