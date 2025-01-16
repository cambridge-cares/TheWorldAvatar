package org.linkeddatafragments.exceptions;

/**
 *
 * @author Miel Vander Sande
 */
public class UnknownDataSourceTypeException extends DataSourceCreationException {
    
    /**
     *
     * @param type
     */
    public UnknownDataSourceTypeException(String type) {
        super("", "Type " + type + " does not exist.");
    } 
}
