package org.linkeddatafragments.datasource;

import com.google.gson.JsonObject;
import org.linkeddatafragments.exceptions.DataSourceCreationException;
import org.linkeddatafragments.exceptions.UnknownDataSourceTypeException;

/**
 *
 * @author Miel Vander Sande
 * @author Bart Hanssens
 * @author <a href="http://olafhartig.de">Olaf Hartig</a>
 */
public class DataSourceFactory {
    /**
     * Create a datasource using a JSON config
     * 
     * @param config
     * @return datasource interface
     * @throws DataSourceCreationException 
     */
    public static IDataSource create(JsonObject config) throws DataSourceCreationException {
    	 
    	System.out.println("================== config file in create ===========================");
    	System.out.println(config);
    	System.out.println("we should be expecting a title attribute");
    	
        String title = config.getAsJsonPrimitive("title").getAsString();
        String description = config.getAsJsonPrimitive("description").getAsString();
        String typeName = config.getAsJsonPrimitive("type").getAsString();
        
        JsonObject settings = config.getAsJsonObject("settings");

        final IDataSourceType type = DataSourceTypesRegistry.getType(typeName);
        if ( type == null )
            throw new UnknownDataSourceTypeException(typeName);

        return type.createDataSource( title, description, settings );
    }

}
