package com.cmclinnovations.stack.clients.ontop;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import com.cmclinnovations.stack.clients.utils.FileUtils;
import com.cmclinnovations.stack.clients.utils.TempFile;
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

    // Matches comments that are on their own line
    private static final Pattern WHOLE_LINE_COMMENT_PATTERN = Pattern.compile("^[\\t ]*#.*\\r?\\n",
            Pattern.MULTILINE);
    // Matches comments that are at the end of a line, after some actual content
    private static final Pattern END_OF_LINE_COMMENT_PATTERN = Pattern.compile("[\\t ]*#[\\t ]+.+(\\r?\\n)");
    // Matches newline characters that follow the standard turtle seperators ",",
    // ";" and "."
    private static final Pattern TARGET_LINES_PATTERN = Pattern.compile("([,;\\.])[\\t ]*\\r?\\n+[\\t ]+");
    // Matches two newline characters seperated whitespace
    private static final Pattern WHITESPACE_BETWEEN_MAPPINGS_PATTERN = Pattern.compile("(\\r?\\n)+\\s+\\r?\\n");
    
    private final Map<String, String> prefixMap = new HashMap<>();
    private final Map<String, SQLPPTriplesMap> triplesMap = new HashMap<>();

    public void addMappings(Path ontopMappingFilePath) {
        try (TempFile tempFilePath = reformatMappingFile(ontopMappingFilePath)) {

            PreProcessedMapping<SQLPPTriplesMap> extraMapings = generateConfiguration(tempFilePath.getPath())
                    .loadProvidedPPMapping();

            prefixMap.putAll(extraMapings.getPrefixManager().getPrefixMap());
            // Add mappings, replacing existing ones if the new ones have the same IDs.
            extraMapings.getTripleMaps().stream()
                    .collect(Collectors.toMap(SQLPPTriplesMap::getId, Function.identity(),
                            (oldEntry, newEntry) -> newEntry, () -> triplesMap));
        } catch (MappingException ex) {
            throw new RuntimeException(ex);
        } catch (IOException ex2) {
            throw new RuntimeException(ex2);
        }
    }

    private static TempFile reformatMappingFile(Path ontopMappingFilePath) throws IOException {
        TempFile tempFilePath = createTempOBDAFile(ontopMappingFilePath);
        String transformedMappings = Files.readString(ontopMappingFilePath);
        // Remove all comments (any text following a # and a space or tab, e.g.,
        // "This is not a comment # This is a comment", "None of this #is a comment")
        // Replace whole line comments, remove the newline character(s)
        transformedMappings = WHOLE_LINE_COMMENT_PATTERN.matcher(transformedMappings).replaceAll("");
        // Replace comments at the end of a line, keep the newline character(s)
        transformedMappings = END_OF_LINE_COMMENT_PATTERN.matcher(transformedMappings).replaceAll("$1");
        // Ontop requires that the "target" (triple template) section is all on one line
        // so remove newline characters that follow the standard turtle seperators ",",
        // ";" and ".".
        transformedMappings = TARGET_LINES_PATTERN.matcher(transformedMappings).replaceAll("$1 ");
        // Problems result if there are tabs between mappings so this replaces them with
        // newline characters.
        transformedMappings = WHITESPACE_BETWEEN_MAPPINGS_PATTERN.matcher(transformedMappings).replaceAll("$1$1");
        Files.writeString(tempFilePath.getPath(), transformedMappings);
        return tempFilePath;
    }

    public static TempFile createTempOBDAFile(Path ontopMappingFilePath) throws IOException {
        return new TempFile(
                File.createTempFile(FileUtils.getFileNameWithoutExtension(ontopMappingFilePath), ".obda").toPath());
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
        return ImmutableList.copyOf(triplesMap.values());
    }

    public void serialize(Path ontopMappingFilePath) throws IOException {

        OntopNativeMappingSerializer writer = new OntopNativeMappingSerializer();

        writer.write(ontopMappingFilePath.toFile(), this);

    }
}