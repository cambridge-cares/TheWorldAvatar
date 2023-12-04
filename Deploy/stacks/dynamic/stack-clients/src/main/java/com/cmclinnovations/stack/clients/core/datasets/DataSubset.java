package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonSubTypes.Type;
import com.fasterxml.jackson.annotation.JsonTypeInfo;

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type")
@JsonSubTypes({
        @Type(value = Tabular.class, names = { "Tabular", "tabular" }),
        @Type(value = Vector.class, names = { "Vector", "vector" }),
        @Type(value = OSMRouting.class, names = { "OSMRouting", "osmRouting", "OsmRouting", "osmrouting" }),
        @Type(value = Raster.class, names = { "Raster", "raster" }),
        @Type(value = RDF.class, names = { "Triples", "triples", "RDF", "rdf", "Quads", "quads" }),
        @Type(value = TBoxCSV.class, names = { "TBoxCSV", "TboxCSV", "tboxcsv" }),
        @Type(value = CityDB.class, names = { "CityDB", "citydb" }),
        @Type(value = XtoCityDB.class, names = { "XtoCityDB", "xtocitydb" }) })
public abstract class DataSubset {

    private String name;
    @JsonProperty
    private Path subdirectory;

    @JsonProperty
    private boolean skip;

    public String getName() {
        return name;
    }

    public Path getSubdirectory() {
        return subdirectory; 
    }

    public boolean isSkip() {
        return skip;
    }

    public boolean usesPostGIS() {
        return false;
    }

    public boolean usesBlazegraph() {
        return false;
    }

    public boolean usesGeoServer() {
        return false;
    }

    public void load(Dataset dataset) {
        if (!skip) {
            loadInternal(dataset);
        }
    }

    abstract void loadInternal(Dataset dataset);
}
