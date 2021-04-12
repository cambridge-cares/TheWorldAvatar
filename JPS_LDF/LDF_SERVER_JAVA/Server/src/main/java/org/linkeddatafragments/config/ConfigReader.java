package org.linkeddatafragments.config;

import java.io.Reader;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.linkeddatafragments.datasource.IDataSourceType;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

/**
 * Reads the configuration of a Linked Data Fragments server.
 *
 * @author Ruben Verborgh
 * @author <a href="http://olafhartig.de">Olaf Hartig</a>
 */
public class ConfigReader {
    private final Map<String, IDataSourceType> dataSourceTypes = new HashMap<>();
    private final Map<String, JsonObject> dataSources = new HashMap<>();
    private final Map<String, String> prefixes = new HashMap<>();
    private final String baseURL;

    /**
     * Creates a new configuration reader.
     *
     * @param configReader the configuration
     */
    public ConfigReader(Reader configReader) {
        JsonObject root = new JsonParser().parse(configReader).getAsJsonObject();
        
        this.baseURL = root.has("baseURL") ? root.getAsJsonPrimitive("baseURL").getAsString() : null;
        
    
        for (Entry<String, JsonElement> entry : root.getAsJsonObject("datasourcetypes").entrySet()) {
            final String className = entry.getValue().getAsString();
            this.dataSourceTypes.put(entry.getKey(), initDataSouceType(className) );
        }
        
         
        for (Entry<String, JsonElement> entry : root.getAsJsonObject("datasources").entrySet()) {
            JsonObject dataSource = entry.getValue().getAsJsonObject();
            this.dataSources.put(entry.getKey(), dataSource);
        }
        
 
        System.out.println("root before extracting prefixes");
        System.out.println(root);
        JsonObject _prefix = root.getAsJsonObject("prefixes");
        System.out.println(_prefix);
        for (Entry<String, JsonElement> entry : _prefix.entrySet()) {
            this.prefixes.put(entry.getKey(), entry.getValue().getAsString());
        }
    }

    /**
     * Gets the data source types.
     *
     * @return a mapping of names of data source types to these types  
     */
    public Map<String, IDataSourceType> getDataSourceTypes() {
        return dataSourceTypes;
    }

    /**
     * Gets the data sources.
     *
     * @return the data sources
     */
    public Map<String, JsonObject> getDataSources() {
        return this.dataSources;
    }

    /**
     * Gets the prefixes.
     *
     * @return the prefixes
     */
    public Map<String, String> getPrefixes() {
        return this.prefixes;
    }

    /**
     * Gets the base URL
     * 
     * @return the base URL
     */
    public String getBaseURL() {
        return this.baseURL;
    }

    /**
     * Loads a certain {@link IDataSourceType} class at runtime
     * 
     * @param className IDataSourceType class
     * @return the created IDataSourceType object
     */
    protected IDataSourceType initDataSouceType( final String className )
    {
        final Class<?> c;
        try {
            c = Class.forName( className ); 
        }
        catch ( ClassNotFoundException e ) {
            throw new IllegalArgumentException( "Class not found: " + className,
                                                e );
        }

        final Object o;
        try {
            o = c.newInstance();
        }
        catch ( Exception e ) {
            throw new IllegalArgumentException(
                        "Creating an instance of class '" + className + "' " +
                        "caused a " + e.getClass().getSimpleName() + ": " +
                        e.getMessage(), e );
        }

        if ( ! (o instanceof IDataSourceType) )
            throw new IllegalArgumentException(
                        "Class '" + className + "' is not an implementation " +
                        "of IDataSourceType." );

        return (IDataSourceType) o;
    }

}
