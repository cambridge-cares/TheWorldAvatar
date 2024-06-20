package com.cmclinnovations.stack.clients.gdal;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;

import javax.annotation.Nonnull;

import org.junit.jupiter.api.Assertions;

import com.cmclinnovations.stack.clients.utils.JsonHelper;

public class CommonOptionsFactory<T extends CommonOptions<T>> {

    protected static final String SRID_IN = "EPSG:3264";
    protected static final String SRID_OUT = "EPSG:4326";
    protected static final String X_POSSIBLE_NAMES_VALUE = "x";
    protected static final String Y_POSSIBLE_NAMES_VALUE = "y";
    protected static final String OTHER_OPTIONS_SELECT_VALUE = "a,b,c";

    protected final Class<T> classBeingTested;

    protected <F extends CommonOptionsFactory<T>> CommonOptionsFactory(Class<T> classBeingTested) {
        this.classBeingTested = classBeingTested;
    }

    protected void configureOptions(ArgsEnum optionEnum, @Nonnull T options) {
        switch (optionEnum) {
            case sridIn:
                options.setSridIn(CommonOptionsFactory.SRID_IN);
                break;
            case sridBoth:
                options.setSridIn(CommonOptionsFactory.SRID_IN);
            case sridOut:
                options.setSridOut(CommonOptionsFactory.SRID_OUT);
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

    protected final T createOptions(@Nonnull ArgsEnum argsEnum) {
        T options;
        String name = argsEnum.name();
        int index = name.lastIndexOf("File");
        if (index > 0) {
            options = getOptionsFromFile(name.substring(0, index), classBeingTested);
        } else {
            try {
                options = classBeingTested.getConstructor().newInstance();

                configureOptions(argsEnum, options);
            } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
                    | InvocationTargetException | NoSuchMethodException | SecurityException ex) {
                Assertions.fail(ex);
                return null;
            }
        }
        return options;
    }

    private static final <T> T getOptionsFromFile(String name, Class<T> classBeingTested) {
        T options;
        String filename = name + ".json";
        try (InputStream is = CommonOptionsTest.class.getResourceAsStream(filename)) {

            Assertions.assertNotNull(is, () -> "Ensure the file '"
                    + CommonOptionsTest.class.getResource("./") + filename
                    + "' exists and is a valid JSON file.");

            options = JsonHelper.getMapper().readValue(is, classBeingTested);

            Assertions.assertNotNull(options, () -> "Ensure the file '"
                    + CommonOptionsTest.class.getResource("./") + filename
                    + "' is a valid JSON file.");

        } catch (IOException ex) {
            Assertions.fail(ex);
            options = (T) null; // appeasing the compiler: this line will never be executed.
        }
        return options;
    }

}
