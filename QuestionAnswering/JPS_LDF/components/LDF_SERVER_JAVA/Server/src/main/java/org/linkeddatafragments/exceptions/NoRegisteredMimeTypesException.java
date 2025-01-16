package org.linkeddatafragments.exceptions;

/**
 * Exception thrown when no mimeTypes are known to the system
 * 
 * @author Miel Vander Sande
 */
public class NoRegisteredMimeTypesException extends Exception {

    /**
     * Constructs the exception
     */
    public NoRegisteredMimeTypesException() {
        super("List of supported mimeTypes is empty.");
    }
    
}
