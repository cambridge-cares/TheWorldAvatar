package com.cmclinnovations.stack.clients.utils;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import com.moandjiezana.toml.Toml;
import com.moandjiezana.toml.TomlWriter;

public final class SparqlRulesFile {

    private final List<String> rules;

    public SparqlRulesFile() {
        rules = new ArrayList<>();
    }

    public SparqlRulesFile(Path file) {
        Toml tomlRules = new Toml().read(file.toFile());
        rules = tomlRules.to(SparqlRulesFile.class).rules;
    }

    public SparqlRulesFile(Collection<Path> files) {
        this();
        this.addAllRules(files);
    }

    public List<String> getRules() {
        return rules;
    }

    public void addAllRules(Collection<Path> files) {
        files.forEach(this::addRules);
    }

    public void addRules(Path file) {
        addRules(new SparqlRulesFile(file));
    }

    public void addRules(SparqlRulesFile other) {
        rules.addAll(other.rules);
    }

    public void write(OutputStream outputStream) throws IOException {
        TomlWriter tomlWriter = new TomlWriter();
        tomlWriter.write(this, outputStream);
    }
}
