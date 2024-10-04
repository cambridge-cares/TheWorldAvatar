package org.linkeddatafragments.datasource;

import java.util.HashMap;
import java.util.Map;

/**
 * A registry of {@link IDataSourceType}s.
 *
 * @author <a href="http://olafhartig.de">Olaf Hartig</a>
 */
public class DataSourceTypesRegistry
{
    private static Map<String, IDataSourceType> registry =
                                        new HashMap<String, IDataSourceType>();

    /**
     *
     * @param typeName
     * @return
     */
    public static synchronized IDataSourceType getType( final String typeName )
    {
        return registry.get( typeName );
    }

    /**
     *
     * @param typeName
     * @return
     */
    public static synchronized boolean isRegistered( final String typeName )
    {
        return registry.containsKey( typeName );
    }

    /**
     *
     * @param typeName
     * @param type
     */
    public static synchronized void register( final String typeName,
                                              final IDataSourceType type )
    {
        if ( registry.containsKey(typeName) ) {
            throw new IllegalArgumentException( "The registry already " +
                       "contains a type with the name '" + typeName + "'." );
        }
        else {
        registry.put( typeName, type );}
    }

}
