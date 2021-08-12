package org.linkeddatafragments.exceptions;

import org.linkeddatafragments.datasource.IDataSource;

/**
 *
 * @author Miel Vander Sande
 */
abstract public class DataSourceException extends Exception {

    /**
     *
     * @param cause
     */
    public DataSourceException(Throwable cause) {
        super(cause);
    }

    /**
     *
     * @param datasourceName
     * @param message
     */
    public DataSourceException(String datasourceName, String message) {
        super("Error for datasource '" + datasourceName + "': " + message);
    }
    
    /**
     *
     * @param datasource
     * @param message
     */
    public DataSourceException(IDataSource datasource, String message) {
        this(datasource.getTitle(), message);
    }
    
}
