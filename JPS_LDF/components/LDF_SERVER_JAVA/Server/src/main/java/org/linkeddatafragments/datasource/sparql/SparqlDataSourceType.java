package org.linkeddatafragments.datasource.sparql;

import java.net.URI;
import java.net.URISyntaxException;

import org.apache.http.client.utils.URIBuilder;
import org.linkeddatafragments.datasource.IDataSource;
import org.linkeddatafragments.datasource.IDataSourceType;
import org.linkeddatafragments.exceptions.DataSourceCreationException;

import com.google.gson.JsonObject;

/**
 * The type of Triple Pattern Fragment data sources that are backed by
 * a SPARQL endpoint.
 *
 * @author <a href="http://olafhartig.de">Olaf Hartig</a>
 * @author <a href="https://awoods.io">Andrew Woods</a>
 */
public class SparqlDataSourceType implements IDataSourceType
{
    @Override
    public IDataSource createDataSource( final String title,
                                         final String description,
                                         final JsonObject settings )
                                                     throws DataSourceCreationException
    {
        // Required: Set the SPARQL endpoint
        if ( ! settings.has("endpoint")) {
            throw new DataSourceCreationException("SparqlDataSource", "Missing required configuration element: 'endpoint'");
        }

        URI endpoint;
        try {
            endpoint = new URIBuilder(settings.getAsJsonPrimitive("endpoint").getAsString()).build();
        } catch (URISyntaxException e) {
            throw new DataSourceCreationException("SparqlDataSource", "Configuration element, 'endpoint', must be a valid URI");
        }

        // Set SPARQL endpoint credentials, if provided
        String username = null;
        if (settings.has("username")) {
            username = settings.getAsJsonPrimitive("username").getAsString();
        }

        String password = null;
        if (settings.has("password")) {
            password = settings.getAsJsonPrimitive("password").getAsString();
        }

        try {
            return new SparqlDataSource(title, description, endpoint, username, password);
        } catch (Exception ex) {
            throw new DataSourceCreationException(ex);
        }
    }

}
