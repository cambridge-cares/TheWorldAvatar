package org.linkeddatafragments.exceptions;

/**
 *
 * @author Miel Vander Sande
 */
public class DataSourceCreationException extends DataSourceException {

    /**
     *
     * @param cause
     */
    public DataSourceCreationException(Throwable cause) {
        super(cause);
    }

    /**
     *
     * @param datasourceName
     * @param message
     */
    public DataSourceCreationException(String datasourceName, String message) {
        super(datasourceName, "Could not create DataSource - " + message);
    }  
}
