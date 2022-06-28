package com.cmclinnovations.stack.clients.ontop;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.cmclinnovations.stack.clients.utils.FileUtils;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;

import it.unibz.inf.ontop.exception.MappingException;
import it.unibz.inf.ontop.injection.OntopMappingConfiguration;
import it.unibz.inf.ontop.injection.OntopSQLOWLAPIConfiguration;
import it.unibz.inf.ontop.spec.mapping.PrefixManager;
import it.unibz.inf.ontop.spec.mapping.pp.PreProcessedMapping;
import it.unibz.inf.ontop.spec.mapping.pp.SQLPPMapping;
import it.unibz.inf.ontop.spec.mapping.pp.SQLPPTriplesMap;
import it.unibz.inf.ontop.spec.mapping.serializer.impl.OntopNativeMappingSerializer;

final class SQLPPMappingImplementation implements SQLPPMapping {

    private final Map<String, String> prefixMap = new HashMap<>();
    private final List<SQLPPTriplesMap> triplesMap = new ArrayList<>();

    public void addMappings(Path ontopMappingFilePath) {
        try {
            Path tempFilePath = reformatMappingFile(ontopMappingFilePath);

            PreProcessedMapping<SQLPPTriplesMap> extraMapings = generateConfiguration(tempFilePath)
                    .loadProvidedPPMapping();

            prefixMap.putAll(extraMapings.getPrefixManager().getPrefixMap());
            triplesMap.addAll(extraMapings.getTripleMaps());
        } catch (MappingException ex) {
            // TODO Auto-generated catch block
            ex.printStackTrace();
        } catch (IOException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        }
    }

    private static Path reformatMappingFile(Path ontopMappingFilePath) throws IOException {
        Path tempFilePath = createTempOBDAFile(ontopMappingFilePath);
        try (BufferedReader bufferedReader = Files.newBufferedReader(ontopMappingFilePath);
                BufferedWriter bufferedWriter = Files.newBufferedWriter(tempFilePath)) {
            bufferedReader.lines()
                    .map(line -> line.replaceFirst("^ *#.*$", ""))
                    .map(line -> line.replaceFirst("(, *|; *|\\. *)", "\1\n"))
                    .forEachOrdered(string -> {
                        try {
                            bufferedWriter.write(string, 0, string.length());
                        } catch (IOException e) {
                            // TODO Auto-generated catch block
                            e.printStackTrace();
                        }
                    });
        }
        return tempFilePath;
    }

    public static Path createTempOBDAFile(Path ontopMappingFilePath) throws IOException {
        return File.createTempFile(FileUtils.getFileNameWithoutExtension(ontopMappingFilePath), ".obda").toPath();
    }

    private static OntopSQLOWLAPIConfiguration generateConfiguration(Path ontopMappingFilePath) {
        return OntopSQLOWLAPIConfiguration
                .defaultBuilder()
                .nativeOntopMappingFile(ontopMappingFilePath.toFile())
                .jdbcDriver("dummy")
                .jdbcUrl("dummy")
                .jdbcUser("")
                .jdbcPassword("")
                .build();
    }

    @Override
    public PrefixManager getPrefixManager() {
        return OntopMappingConfiguration.defaultBuilder().build()
                .getSpecificationFactory()
                .createPrefixManager(ImmutableMap.copyOf(prefixMap));
    }

    @Override
    public ImmutableList<SQLPPTriplesMap> getTripleMaps() {
        return ImmutableList.copyOf(triplesMap);
    }

    public void serialize(Path ontopMappingFilePath) throws IOException {

        OntopNativeMappingSerializer writer = new OntopNativeMappingSerializer();

        writer.write(ontopMappingFilePath.toFile(), this);

    }
}