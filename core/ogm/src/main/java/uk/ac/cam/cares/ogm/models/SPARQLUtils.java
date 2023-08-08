package uk.ac.cam.cares.ogm.models;

import org.apache.jena.arq.querybuilder.AbstractQueryBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.query.sparql.PrefixToUrlMap;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A utility class for looking up prefixes in the JPS_BASE_LIB {@link PrefixToUrlMap} and specifications in the
 * config file of the form {@code uri.prefix.[prefix]=[full IRI]}, and for decoding AccessAgent responses.
 * @author <a href="mailto:jec226@cam.ac.uk">Jefferson Chua</a>
 * @version $Id$
 */
public class SPARQLUtils {

  private static Map<String, String> prefixMap = new HashMap<>();
  private static final Pattern qualifiedNamePattern = Pattern.compile("([A-Za-z0-9]+):([A-Za-z0-9]+)");

  /**
   * Identifies all qualified names in the sample string provided, looks the prefixes up in {@link PrefixToUrlMap}
   * and the <code>config.properties</code> file, and adds any identified prefix-IRI pairs to the provided builder.
   * @param sample the string in which to search for qualified names.
   * @param builder the builder to which to add prefixes.
   * @return the same builder that was provided.
   */
  public static <T extends AbstractQueryBuilder<T>> AbstractQueryBuilder<T> addPrefix(String sample, AbstractQueryBuilder<T> builder) {
    // Find all qualified names in the query
    Matcher matcher = qualifiedNamePattern.matcher(sample);
    Set<String> prefixes = new HashSet<>();
    while(matcher.find()) prefixes.add(matcher.group(1));
    // If sample is itself a prefix, add that as well
    prefixes.add(sample);
    // Compile prefix statements
    for (String prefix: prefixes) {
      String prefixUrl = getPrefixUrl(prefix);
      if(prefixUrl != null) builder.addPrefix(prefix, prefixUrl);
    }
    return builder;
  }

  /**
   * Identifies all qualified names in the sample string provided, looks the prefixes up in {@link PrefixToUrlMap}
   * and the <code>config.properties</code> file, and adds any identified prefix-IRI pairs to the provided builder.
   * @param sample the string in which to search for qualified names.
   * @param builder the builder to which to add prefixes.
   * @return the same builder that was provided.
   */
  public static UpdateBuilder addPrefix(String sample, UpdateBuilder builder) {
    // Find all prefixes referenced in the query
    Matcher matcher = qualifiedNamePattern.matcher(sample);
    Set<String> prefixes = new HashSet<>();
    while(matcher.find()) prefixes.add(matcher.group(1));
    // If sample is itself a prefix, add that as well
    prefixes.add(sample);
    // Compile prefix statements
    for (String prefix: prefixes) {
      String prefixUrl = getPrefixUrl(prefix);
      if(prefixUrl != null) builder.addPrefix(prefix, prefixUrl);
    }
    return builder;
  }

  /**
   * Expands a qualified name to its full IRI by looking up the prefix in {@link PrefixToUrlMap} and the
   * <code>config.properties</code> specifications.
   * @param expression the expression to expand.
   * @return the expanded IRI.
   */
  public static String expandQualifiedName(String expression) {
    String[] parts = expression.split(":", 2);
    if(parts.length > 1) {
      String prefixUrl = getPrefixUrl(parts[0]);
      if(prefixUrl != null) return prefixUrl + parts[1];
    }
    return expression;
  }

  /**
   * Looks up a prefix in {@link PrefixToUrlMap} and the <code>config.properties</code> file specifications.
   * @param prefix the prefix to look up.
   * @return the full IRI if identified, otherwise <code>null</code>.
   */
  public static String getPrefixUrl(String prefix) {
    if(prefixMap.containsKey(prefix)) {
    } else if(PrefixToUrlMap.getPrefixUrl(prefix) != null){
      prefixMap.put(prefix, PrefixToUrlMap.getPrefixUrl(prefix));
    } else if(ResourceBundle.getBundle("config").containsKey("uri.prefix." + prefix)) {
      String uri = ResourceBundle.getBundle("config").getString("uri.prefix." + prefix);
      prefixMap.put(prefix, uri);
    } else {
      return null;
    }
    return prefixMap.get(prefix);
  }

  /**
   * Extracts the data from an AccessAgent query response of the form {@code {"result": "[JSON array]"}}.
   * @param responseString
   * @return the deserialised JSON array stored within the "result" property.
   */
  public static JSONArray unpackQueryResponse(String responseString) {
    return new JSONArray(new JSONObject(responseString).getString("result"));
  }

}
