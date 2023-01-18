package com.github.dockerjava.core.command;

import static com.google.common.base.Preconditions.checkNotNull;

import java.util.List;
import java.util.Map;

import com.cmclinnovations.swagger.podman.model.ListPodsReport;
import com.github.dockerjava.api.command.ListPodsCmd;
import com.github.dockerjava.core.util.FiltersBuilder;


public class ListPodsCmdImpl extends AbstrDockerCmd<ListPodsCmd, List<ListPodsReport>> implements
        ListPodsCmd {

    private FiltersBuilder filters = new FiltersBuilder();

    public ListPodsCmdImpl(Exec exec) {
        super(exec);
    }

    @Override
    public Map<String, List<String>> getFilters() {
        return filters.build();
    }

    @Override
    public ListPodsCmd withNameFilter(List<String> names) {
        checkNotNull(names, "names was not specified");
        this.filters.withFilter("name", names);
        return this;
    }

}
