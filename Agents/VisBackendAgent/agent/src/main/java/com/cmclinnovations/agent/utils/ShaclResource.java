package com.cmclinnovations.agent.utils;

public class ShaclResource {
  // JSON LD keys
  public static final String CONTEXT_KEY = "@context";
  public static final String ID_KEY = "@id";
  public static final String TYPE_KEY = "@type";
  public static final String VAL_KEY = "@value";
  public static final String REPLACE_KEY = "@replace";
  public static final String REVERSE_KEY = "@reverse";
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
  public static final String QUALIFIED_VAL_SHAPE_PROPERTY = "qualifiedValueShape";
  // Query string elements
  public static final String RDFS_LABEL_PREDICATE = "rdfs:label";
  public static final String FULL_STOP = ".";
  public static final String VARIABLE_MARK = "?";
  public static final String REPLACEMENT_ENDPOINT = "[endpoint]";
  public static final String WHITE_SPACE = " ";
  public static final String UNION_OPERATOR = "} UNION {";

  private ShaclResource(){
  }
}
