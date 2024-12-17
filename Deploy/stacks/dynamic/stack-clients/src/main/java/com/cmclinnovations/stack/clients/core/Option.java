package com.cmclinnovations.stack.clients.core;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonCreator;

public class Option {
    private List<String> optionList;

    /**
     * De-serialises empty objects <code>{}</code>
     */
    @JsonCreator
    public Option() {
        this.optionList = List.of();
    }

    /**
     * De-serialises empty strings <code>""</code> and single arguments
     * <code>"A"</code>.
     */
    @JsonCreator
    public Option(String option) {
        this.optionList = option.isEmpty() ? List.of() : List.of(option);
    }

    /**
     * De-serialises empty and populated arrays <code>[]</code>, <code>["A"]</code>
     * and <code>["A", "B"]</code>.
     */
    @JsonCreator
    public Option(List<String> option) {
        this.optionList = option;
    }

    /**
     * For programatic construction only.
     */
    public Option(String... option) {
        this.optionList = List.of(option);
    }

    public List<String> getOptionList() {
        return optionList;
    }
}
