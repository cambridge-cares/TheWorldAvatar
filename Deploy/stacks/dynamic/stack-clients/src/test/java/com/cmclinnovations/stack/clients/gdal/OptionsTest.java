package com.cmclinnovations.stack.clients.gdal;

import java.util.List;

import javax.annotation.Nonnull;

import org.junit.jupiter.api.Assertions;

public abstract class OptionsTest<T extends CommonOptions<T>> {

    protected void checkCommand(@Nonnull T options, List<String> expected) {
        List<String> actual = options.appendCommonToArgs();
        Assertions.assertEquals(expected, actual);
    }
}
