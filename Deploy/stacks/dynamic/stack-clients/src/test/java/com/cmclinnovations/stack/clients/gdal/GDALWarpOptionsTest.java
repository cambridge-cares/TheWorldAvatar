package com.cmclinnovations.stack.clients.gdal;

import java.util.List;

import javax.annotation.Nonnull;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

class GDALWarpOptionsTest {
    private static final @Nonnull GDALWarpOptionsTestHelper TEST_HELPER = new GDALWarpOptionsTestHelper();

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "trivial", "trivialFile" })
    void testTrivial(ArgsEnum ArgsEnum) {
        GDALWarpOptions options = TEST_HELPER.createOptions(ArgsEnum);

        List<String> expected = TEST_HELPER.getExpectedCommand();
        TEST_HELPER.checkCommand(options, expected);
    }
}
