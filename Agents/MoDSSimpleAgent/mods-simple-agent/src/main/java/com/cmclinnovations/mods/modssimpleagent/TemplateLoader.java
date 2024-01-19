package com.cmclinnovations.mods.modssimpleagent;

import java.io.InputStream;
import java.util.NoSuchElementException;

import javax.xml.bind.JAXBException;

public final class TemplateLoader {

    private static final String TEMPLATES_DIR = "MoDS_inputs_templates/";
    private static final String TEMPLATE_FILE_EXTENSION = ".xml";

    private TemplateLoader() {
    }

    public static BackendInputFile load(String templateName) throws JAXBException {
        if (templateName.isBlank()) {
            templateName = "HDMR";
        }
        return new BackendInputFile(getTemplate(templateName));
    }

    private static InputStream getTemplate(String templateName) throws NoSuchElementException {
        InputStream resourceAsStream = TemplateLoader.class.getClassLoader().getResourceAsStream(
                TEMPLATES_DIR + templateName + TEMPLATE_FILE_EXTENSION);
        if (null != resourceAsStream) {
            return resourceAsStream;
        } else {
            throw new NoSuchElementException("No template with the name '" + templateName + "' exists.");
        }
    }

}
