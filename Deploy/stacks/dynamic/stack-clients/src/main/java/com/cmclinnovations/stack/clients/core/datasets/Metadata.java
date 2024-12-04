package com.cmclinnovations.stack.clients.core.datasets;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Predicate;

import javax.annotation.Nonnull;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.eclipse.rdf4j.sparqlbuilder.rdf.RdfPredicateObjectList;

import com.cmclinnovations.stack.clients.utils.JsonHelper;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

class Metadata {

    static final Metadata EMPTY_METADATA = new Metadata(Map.of(), Optional.empty());

    private final Map<String, String> prefixes;
    private final Optional<String> triplePatterns;

    @JsonCreator
    Metadata(@JsonProperty("prefixes") Map<String, String> prefixes,
            @JsonProperty("triplePatterns") Optional<String> triplePatterns) {
        this.prefixes = prefixes;
        this.triplePatterns = triplePatterns.map(JsonHelper::handleFileValues);
    }

    public List<Prefix> getPrefixes() {
        List<Prefix> prefixList = new ArrayList<>(prefixes.size());
        prefixes.forEach((alias, iri) -> prefixList.add(SparqlBuilder.prefix(alias, Rdf.iri(iri))));
        return prefixList;
    }

    Optional<@Nonnull TriplePattern> getTriplePatterns() {

        return triplePatterns
                // Remove blank entry
                .filter(Predicate.not(String::isBlank))
                // Ensure that the last triple pattern ends with a "."
                .map(tp -> tp.replaceFirst("([^.\\s])\\s*\\.?\\s*$", "$1 ."))
                // Wrap the triple patterns for RDF4J
                .map(tp -> new TriplePattern() {
                    @Override
                    public String getQueryString() {
                        return tp;
                    }

                    @Override
                    public TriplePattern andHas(RdfPredicateObjectList... lists) {
                        throw new UnsupportedOperationException("Unsupported methodandHas'");
                    }
                });
    }

}
