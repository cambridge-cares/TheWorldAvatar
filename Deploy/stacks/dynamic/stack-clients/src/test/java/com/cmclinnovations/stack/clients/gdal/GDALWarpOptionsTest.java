package com.cmclinnovations.stack.clients.gdal;

import java.util.List;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

class GDALWarpOptionsTest extends AbstractOptionsTest<GDALWarpOptions, GDALWarpOptionsFactory> {

    protected GDALWarpOptionsTest() {
        super("gdalwarp", new GDALWarpOptionsFactory());
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "trivial", "trivialFile" })
    void testTrivial(ArgsEnum ArgsEnum) {
        GDALWarpOptions options = getOptions(ArgsEnum);

        List<String> expected = getExpectedCommand();
        checkCommand(options, expected);
    }

}
