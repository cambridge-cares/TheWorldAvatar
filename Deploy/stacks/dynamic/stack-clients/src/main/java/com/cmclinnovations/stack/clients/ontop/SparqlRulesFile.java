package com.cmclinnovations.stack.clients.ontop;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

class SparqlRulesFile {

    private List<String> rules;

    public SparqlRulesFile() {
        this(List.of());
    }

    public SparqlRulesFile(List<String> rules) {
        this.rules = rules;
    }

    public List<String> getRules() {
        return rules;
    }

    public void setRules(List<String> rules) {
        this.rules = rules;
    }

    public void addRules(SparqlRulesFile rules) {
        addRules(rules.getRules());
    }

    public void addRules(List<String> rules) {
        this.rules = Stream.concat(this.rules.stream(), rules.stream()).collect(Collectors.toList());
    }

}
