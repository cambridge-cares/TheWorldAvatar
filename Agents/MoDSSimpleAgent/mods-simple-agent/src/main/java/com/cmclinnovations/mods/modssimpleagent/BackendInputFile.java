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
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import com.cmclinnovations.mods.backendinputfile.Details;
import com.cmclinnovations.mods.backendinputfile.Details.Detail;
import com.cmclinnovations.mods.backendinputfile.Mods;
import com.cmclinnovations.mods.backendinputfile.Mods.Cases.Case;
import com.cmclinnovations.mods.backendinputfile.Mods.Cases;
import com.cmclinnovations.mods.backendinputfile.Mods.Files;
import com.cmclinnovations.mods.backendinputfile.Mods.Models;
import com.cmclinnovations.mods.backendinputfile.Mods.Models.Model;
import com.cmclinnovations.mods.backendinputfile.Mods.Parameters;
import com.cmclinnovations.mods.backendinputfile.Mods.Algorithms.Algorithm;
import com.cmclinnovations.mods.backendinputfile.Mods.Parameters.Parameter;
import com.cmclinnovations.mods.backendinputfile.ObjectFactory;
import com.cmclinnovations.mods.modssimpleagent.simulations.Simulation;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Wraps the class auto-generated from MoDS_inputs.xsd by
 * jaxb2-maven-plugin (xjc) and populates all nodes using Simulation data
 *
 * @author Owen Parry {@literal <oparry@cmclinnovations.com>}
 */
public final class BackendInputFile implements FileGenerator {

    private static final Logger LOGGER = LoggerFactory.getLogger(BackendInputFile.class);

    private static final String DETAIL_SEPARATOR = " ";
    private static final String PARAMETER_DETAIL_SEPARATOR = ",";
    private static final String PARAMETER_CASE_DETAIL_SEPARATOR = ";";
    public static final String FILENAME = "MoDS_inputs.xml";
    private static final String SCHEMA_LOCATION = "http://como.cheng.cam.ac.uk/MoDS MoDS_inputs.xsd";

    private static final ObjectFactory objectFactory = new ObjectFactory();
    // Cached fields
    private static JAXBContext context;
    private static Unmarshaller unmarshaller;
    private static Marshaller marshaller;

    private final Mods modsObject;

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

    public BackendInputFile(Path path) throws JAXBException, IOException {
        this(java.nio.file.Files.newInputStream(path));
    }

    private BackendInputFile(Mods modsObject) {
        this.modsObject = modsObject;
    }

    public Mods getModsObject() {
        return modsObject;
    }

    public void configureAlgorithmParams(List<String> inputVarSubtypes, List<String> outputVarSubtypes) {
        for (Algorithm alg : modsObject.getAlgorithms().getAlgorithm()) {
            addDetail(alg.getDetails(), "optimisable_param_subtypes", inputVarSubtypes);
            addDetail(alg.getDetails(), "response_param_subtypes", outputVarSubtypes);
        }
    }

    public void configureMOOAlgorithm(String algorithmName, String displayName,
            Map<String, List<String>> partitionedSubtypes) {
        modsObject.getAlgorithms().getAlgorithm().stream()
                .filter(alg -> alg.getName().equals(algorithmName))
                .forEach(alg -> {
                    alg.setDisplayName(displayName);
                    Details details = alg.getDetails();
                    partitionedSubtypes.entrySet().forEach(
                            varEntry -> varEntry.getValue().forEach(
                                    subtype -> addDetail(
                                            details, subtype + ":objective_function", varEntry.getKey())));
                });

    }

    public void addCase(String name, List<String> modelNames) {
        Case case_ = objectFactory.createModsCasesCase();

        case_.setName(name);
        addCaseModels(case_, modelNames);

        addCase(case_);
    }

    private void addCaseModels(Case case_, List<String> modelNames) {
        Case.Models caseModels = objectFactory.createModsCasesCaseModels();
        caseModels.getModel().addAll(modelNames);
        case_.setModels(caseModels);
    }

    private void addCase(Case case_) {
        Cases cases = modsObject.getCases();
        if (null == cases) {
            cases = objectFactory.createModsCases();
            modsObject.setCases(cases);
        }
        cases.getCase().add(case_);
    }

    public void addModel(String name, String type) {
        Model model = objectFactory.createModsModelsModel();

        model.setName(name);
        Details details = objectFactory.createDetails();
        addDetail(details, "model_type", type);
        model.setDetails(details);

        addModel(model);
    }

    private void addModel(Model model) {
        Models models = modsObject.getModels();
        if (null == models) {
            models = objectFactory.createModsModels();
            modsObject.setModels(models);
        }
        models.getModel().add(model);
    }

    public void addFile(String name, String type, String delimiter) {
        Files.File file = objectFactory.createModsFilesFile();

        file.setFileName(name);
        Details details = objectFactory.createDetails();
        addDetail(details, "file_type", type);
        addDetail(details, "delimiter", delimiter);
        file.setDetails(details);

        addFile(file);
    }

    private void addFile(Files.File file) {
        Files files = modsObject.getFiles();
        if (null == files) {
            files = objectFactory.createModsFiles();
            modsObject.setFiles(files);
        }
        files.getFile().add(file);
    }

    public void addParameter(String name, String subtype, String type,
            List<String> caseNames, List<String> modelNames,
            double lowerBound, double upperBound) {
        Parameter param = objectFactory.createModsParametersParameter();

        param.setName(name);
        param.setType("active_" + type);
        param.setSubtype(subtype);
        param.setScaling("linear");

        addParameterCases(param, caseNames);
        addParameterModels(param, modelNames);

        Parameter.Files.InitialRead initialRead = createInitialRead(name, lowerBound, upperBound);
        Parameter.Files files = objectFactory.createModsParametersParameterFiles();
        files.setInitialRead(initialRead);
        param.setFiles(files);

        addParameter(param);
    }

    private void addParameterCases(Parameter param, List<String> caseNames) {
        Parameter.Cases paramCases = objectFactory.createModsParametersParameterCases();
        paramCases.getCase().addAll(caseNames);
        param.setCases(paramCases);
    }

    private void addParameterModels(Parameter param, List<String> modelNames) {
        Parameter.Models paramModels = objectFactory.createModsParametersParameterModels();
        paramModels.getModel().addAll(modelNames);
        param.setModels(paramModels);
    }

    private Parameter.Files.InitialRead createInitialRead(String name, double lowerBound, double upperBound) {
        Parameter.Files.InitialRead initialRead = objectFactory
                .createModsParametersParameterFilesInitialRead();
        initialRead.setFileName(Simulation.INITIAL_FILE_NAME);
        Details initialDetails = objectFactory.createDetails();
        addDetail(initialDetails, "column", name);
        addDetail(initialDetails, "row", 0);
        addDetail(initialDetails, "file_name", Simulation.INITIAL_FILE_NAME);
        addDetail(initialDetails, "read_function", "Get_DSV_double");
        addDetail(initialDetails, "lb_abs", lowerBound);
        addDetail(initialDetails, "ub_abs", upperBound);
        initialRead.setDetails(initialDetails);
        return initialRead;
    }

    private void addParameter(Parameter param) {
        Parameters parameters = modsObject.getParameters();
        if (null == parameters) {
            parameters = objectFactory.createModsParameters();
            modsObject.setParameters(parameters);
        }
        parameters.getParameter().add(param);
    }

    private <T> void addDetail(Details details, String name, T value) {
        Detail detail = objectFactory.createDetailsDetail();
        String strValue = (value instanceof Collection)
                ? ((Collection<?>) value).stream().map(Object::toString)
                        .collect(Collectors.joining(DETAIL_SEPARATOR))
                : value.toString();
        detail.setName(name);
        detail.setValue(strValue);
        details.getDetail().add(detail);
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
    public final void marshal(Path filePath) throws FileGenerationException {
        try (BufferedWriter writer = java.nio.file.Files.newBufferedWriter(filePath, StandardCharsets.UTF_8,
                StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)) {
            getMarshaller().marshal(modsObject, writer);
        } catch (IOException ex) {
            throw new FileGenerationException("IO problem writing MoDS inputs file to " + filePath, ex);
        } catch (JAXBException ex) {
            throw new FileGenerationException("JAXB exception whilst writing MoDS inputs file ", ex);
        }
    }

}
