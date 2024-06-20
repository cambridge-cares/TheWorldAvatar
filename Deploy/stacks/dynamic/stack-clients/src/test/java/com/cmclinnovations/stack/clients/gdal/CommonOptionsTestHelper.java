package com.cmclinnovations.stack.clients.gdal;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.annotation.Nonnull;

import org.junit.jupiter.api.Assertions;

public class CommonOptionsTestHelper<T extends CommonOptions<T>> extends BaseOptionsTestHelper<T> {

    protected static final String SRID_IN = "EPSG:3264";
    protected static final String SRID_OUT = "EPSG:4326";
    protected static final String X_POSSIBLE_NAMES_VALUE = "x";
    protected static final String Y_POSSIBLE_NAMES_VALUE = "y";
    protected static final String OTHER_OPTIONS_SELECT_VALUE = "a,b,c";

    protected static final String TEST_SOURCE = "testSource";
    protected static final String TEST_DESTINATION = "testDestination";

    protected <F extends CommonOptionsTestHelper<T>> CommonOptionsTestHelper(String command, Class<T> classBeingTested) {
        super(command, classBeingTested);
    }

    @Override
    protected void configureOptions(ArgsEnum optionEnum, @Nonnull T options) {
        switch (optionEnum) {
            case sridIn:
                options.setSridIn(CommonOptionsTestHelper.SRID_IN);
                break;
            case sridBoth:
                options.setSridIn(CommonOptionsTestHelper.SRID_IN);
            case sridOut:
                options.setSridOut(CommonOptionsTestHelper.SRID_OUT);
                break;
            case inputDatasetOpenOptions:
                options.addInputDatasetOpenOption("X_POSSIBLE_NAMES", X_POSSIBLE_NAMES_VALUE);
                options.addInputDatasetOpenOption("Y_POSSIBLE_NAMES", Y_POSSIBLE_NAMES_VALUE);
                break;
            case otherOptions:
                options.addOtherOption("-select", OTHER_OPTIONS_SELECT_VALUE);
                break;
            case otherOptionsArray:
                options.addOtherOption("-spat", OTHER_OPTIONS_SELECT_VALUE.split(","));
                break;
            default:
                break;
        }
    }

    protected List<String> getExpectedCommand(String... explicitArgs) {
        return Stream.of(Stream.of(
                getCommand(),
                "-oo", "AUTODETECT_TYPE=YES",
                "-oo", "EMPTY_STRING_AS_NULL=YES"),
                Stream.of(explicitArgs),
                Stream.of(TEST_SOURCE, TEST_DESTINATION))
                .flatMap(Function.identity())
                .collect(Collectors.toList());
    }

    protected void checkCommand(@Nonnull T options, List<String> expected) {
        List<String> actual = List.of(options.generateCommandInternal(new ArrayList<>(),
                CommonOptionsTestHelper.TEST_SOURCE,
                CommonOptionsTestHelper.TEST_DESTINATION));
        Assertions.assertEquals(expected, actual);
    }
}
