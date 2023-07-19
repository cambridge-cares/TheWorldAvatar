package com.cmclinnovations.stack.clients.core;

import java.util.Arrays;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonCreator;

public class Option {
    private List<String> optionList;

    @JsonCreator
    public Option(String option) {
        this.optionList = Arrays.asList(option);
    }

    @JsonCreator
    public Option(List<String> option) {
        this.optionList = option;
    }

    public List<String> getOptionList() {
        return optionList;
    }
}
