package com.cmclinnovations.stack.clients.ontop;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import com.moandjiezana.toml.Toml;
import com.moandjiezana.toml.TomlWriter;

final class SparqlRulesFile {

    private final List<String> rules;

    SparqlRulesFile() {
        rules = new ArrayList<>();
    }

    SparqlRulesFile(Path file) {
        Toml tomlRules = new Toml().read(file.toFile());
        rules = tomlRules.to(SparqlRulesFile.class).rules;
    }

    void addRules(Path file) {
        addRules(new SparqlRulesFile(file));
    }

    void addRules(SparqlRulesFile other) {
        rules.addAll(other.rules);
    }

    void write(OutputStream outputStream) throws IOException {
        TomlWriter tomlWriter = new TomlWriter();
        tomlWriter.write(this, outputStream);
    }
}
