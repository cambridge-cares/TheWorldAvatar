package com.cmclinnovations.stack.exceptions;

import java.io.IOException;

public class InvalidTemplateException extends RuntimeException {

    public InvalidTemplateException(String templateType, String filename, Exception cause) {
        super("The " + templateType + " template file '" + filename + "' is invalid.", cause);
    }

    public InvalidTemplateException(String templateType, String filename, IOException cause) {
        super("The " + templateType + " template file '" + filename + "' could not be read.", cause);
    }

}
