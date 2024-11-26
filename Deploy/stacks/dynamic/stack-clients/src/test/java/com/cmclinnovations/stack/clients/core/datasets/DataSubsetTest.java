package com.cmclinnovations.stack.clients.core.datasets;

import java.util.stream.Stream;

import javax.annotation.Nonnull;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.cmclinnovations.stack.clients.gdal.GDALOptions;
import com.cmclinnovations.stack.clients.gdal.GDALTranslateOptions;
import com.cmclinnovations.stack.clients.gdal.GDALWarpOptions;
import com.cmclinnovations.stack.clients.utils.JsonHelper;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

class DataSubsetTest {

    private static String formatJson(String json) {
        return json.replace("'", "\"");
    }

    private static Stream<Arguments> getArgsForRasterTests() {
        return Stream.of(
                Arguments.of("{'type':'Raster','name':'elevation'}", new GDALTranslateOptions()),
                Arguments.of("{'type':'Raster','name':'elevation', 'gdalTranslateOptions':{}}",
                        new GDALTranslateOptions()),
                Arguments.of("{'type':'Raster','name':'elevation', 'gdalWarpOptions':{}}", new GDALWarpOptions()));
    }

    @ParameterizedTest
    @MethodSource("getArgsForRasterTests")
    <T extends GDALOptions<T>> void testRasterParse(String json, @Nonnull T expectedOptions)
            throws JsonProcessingException {
        ObjectMapper mapper = JsonHelper.getMapper();
        Raster dataSubset = mapper.readValue(formatJson(json), Raster.class);

        Assertions.assertEquals(expectedOptions.getClass(), dataSubset.gdalOptions.getClass());

        Assertions.assertEquals(mapper.writeValueAsString(expectedOptions),
                mapper.writeValueAsString(dataSubset.gdalOptions));
    }

}
