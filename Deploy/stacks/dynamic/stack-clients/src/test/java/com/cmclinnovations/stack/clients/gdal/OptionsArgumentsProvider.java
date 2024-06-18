package com.cmclinnovations.stack.clients.gdal;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.util.stream.Stream;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.ArgumentsProvider;

import com.cmclinnovations.stack.clients.utils.JsonHelper;

abstract class OptionsArgumentsProvider<T extends CommonOptions<T>> implements ArgumentsProvider {
    protected static final String SRID_IN = "EPSG:3264";
    protected static final String SRID_OUT = "EPSG:4326";

    protected final String command;
    protected final Class<T> classBeingTested;

    protected OptionsArgumentsProvider(String command, Class<T> classBeingTested) {
        this.command = command;
        this.classBeingTested = classBeingTested;
    }

    @Override
    public Stream<? extends Arguments> provideArguments(ExtensionContext context) {
        String testName = context.getRequiredTestMethod().getName();
        return Stream.of(Arguments.of(getMockedOptions(testName), command),
                Arguments.of(getOptionsFromFile(testName), command));
    }

    private T getOptionsFromFile(String testName) {
        try (InputStream is = CommonOptionsTest.class
                .getResourceAsStream(classBeingTested.getSimpleName() + "/" + testName + ".json")) {
            if (null == is) {
                return JsonHelper.getMapper().readValue("{}", classBeingTested);
            } else {
                T options = JsonHelper.getMapper().readValue(is, classBeingTested);
                Assertions.assertNotNull(options,
                        () -> "Ensure the file '" + testName + ".json' is a valid JSON file.");
                return options;
            }
        } catch (IOException ex) {
            Assertions.fail(ex);
        }
        return null;
    }

    private T getMockedOptions(String testName) {
        try {
            T options = classBeingTested.getConstructor().newInstance();

            configureMockedOptions(testName, options);
            return options;
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
                | InvocationTargetException | NoSuchMethodException | SecurityException ex) {
            Assertions.fail(ex);
            return null;
        }
    }

    protected abstract void configureMockedOptions(String testName, T options);
}