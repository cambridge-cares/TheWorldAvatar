package com.cmclinnovations.stack.clients.core;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonCreator;

public class Option {
    private List<String> optionList;

    @JsonCreator
    public Option() {
        this.optionList = List.of();
    }

    @JsonCreator
    public Option(String option) {
        this.optionList = option.isEmpty() ? List.of() : List.of(option);
    }

    @JsonCreator
    public Option(List<String> option) {
        this.optionList = option;
    }

    public List<String> getOptionList() {
        return optionList;
    }
}
