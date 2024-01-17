package com.cmclinnovations.mods.modssimpleagent.datamodels;

import java.util.List;
import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

import lombok.Builder;

@JsonInclude(Include.NON_NULL)
@Builder(toBuilder = true)
public record Request(String jobID, @JsonProperty("SimulationType") String simulationType,
        @JsonProperty("Algorithms") List<Algorithm> algorithms, @JsonProperty("Inputs") Data inputs,
        @JsonProperty("Outputs") Data outputs, @JsonProperty("Sensitivities") List<SensitivityResult> sensitivities) {

    public Request(String jobID, String simulationType) {
        this(jobID, simulationType, null, null, null, null);
    }

    @JsonIgnore
    public Algorithm getAlgorithmOfType(String algType) {
        return algorithms.stream().filter(alg -> alg.type().equals(algType)).findFirst().orElseThrow();
    }

    @JsonIgnore
    public String getSurrogateToLoad() {
        return algorithms.stream().map(Algorithm::surrogateToLoad).filter(Objects::nonNull).findFirst().orElse(null);
    }
}
