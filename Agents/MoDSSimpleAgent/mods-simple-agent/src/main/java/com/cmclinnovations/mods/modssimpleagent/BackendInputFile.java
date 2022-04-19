/*
 * Copyright (c) 2011-2017  CMCL Innovations - All Rights Reserved
 *
 * This application and all inherent data, source files, information and graphics are
 * the copyright and sole property of Computational Modelling Cambridge Ltd (CMCL Innovations).
 *
 * Any unauthorised redistribution or reproduction of part, or all, of the contents of this
 * application in any form is prohibited under UK Copyright Law. You may not, except with the
 * express written permission of CMCL Innovations, distribute or commercially exploit this
 * application or its content. All other rights reserved.
 *
 * For more information please contact support(@)cmclinnovations.com
 */
package com.cmclinnovations.mods.modssimpleagent;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import com.cmclinnovations.mods.backendinputfile.Mods;
import com.cmclinnovations.mods.backendinputfile.ObjectFactory;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Wraps the class auto-generated from MoDS_inputs.xsd by
 * jaxb2-maven-plugin (xjc) and populates all nodes using Simulation data
 *
 * @author Owen Parry {@literal <oparry@cmclinnovations.com>}
 */
public final class BackendInputFile {

    private static final Logger LOGGER = LoggerFactory.getLogger(BackendInputFile.class);

    private static final String CASE_DETAIL_SEPARATOR = ";";
    private static final String DETAIL_SEPARATOR = ",";
    public static final String FILENAME = "MoDS_inputs.xml";
    private static final String SCHEMA_LOCATION = "http://como.cheng.cam.ac.uk/MoDS MoDS_inputs.xsd";

    private static final ObjectFactory objectFactory = new ObjectFactory();
    // Cached fields
    private static JAXBContext context;
    private static Unmarshaller unmarshaller;
    private static Marshaller marshaller;

    private Mods modsInputFileObject;

    public BackendInputFile() {
        this(objectFactory.createMods());
    }

    /**
     * Unmarshal from path/FILENAME
     * 
     * @throws IOException
     * @throws JAXBException
     */
    public BackendInputFile(InputStream stream) throws JAXBException {
        this(unmarshal(stream));
    }

    private BackendInputFile(Mods modsInputFileObject) {
        this.modsInputFileObject = modsInputFileObject;
    }

    private static JAXBContext getContext() throws JAXBException {
        if (null == context) {
            context = JAXBContext.newInstance(Mods.class);
        }
        return context;
    }

    private static Unmarshaller getUnmarshaller() throws JAXBException {
        if (null == unmarshaller) {
            unmarshaller = getContext().createUnmarshaller();
        }
        return unmarshaller;
    }

    private static Marshaller getMarshaller() throws JAXBException {
        if (null == marshaller) {
            marshaller = getContext().createMarshaller();
            marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
            marshaller.setProperty(Marshaller.JAXB_SCHEMA_LOCATION, SCHEMA_LOCATION);
        }
        return marshaller;
    }

    /**
     * Unmarshal from stream
     * 
     * @throws IOException
     * @throws JAXBException
     */
    private static Mods unmarshal(InputStream stream) throws JAXBException {
        try {
            return (Mods) getUnmarshaller().unmarshal(stream);
        } catch (JAXBException ex) {
            throw new JAXBException("Error whilst reading MoDS inputs file.", ex);
        }
    }

    /**
     * Marshal to path/FILENAME
     * 
     * @throws IOException
     * @throws JAXBException
     */
    public final void marshal(Path path) throws IOException, JAXBException {
        try (BufferedWriter writer = java.nio.file.Files.newBufferedWriter(path, StandardCharsets.UTF_8,
                StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)) {
            getMarshaller().marshal(modsInputFileObject, writer);
        } catch (IOException ex) {
            throw new IOException("IO problem writing MoDS inputs file to " + path, ex);
        } catch (JAXBException ex) {
            throw new JAXBException("JAXB exception whilst writing MoDS inputs file ", ex);
        }
    }
}
