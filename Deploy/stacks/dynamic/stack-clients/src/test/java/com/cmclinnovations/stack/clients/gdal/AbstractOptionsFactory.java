package com.cmclinnovations.stack.clients.gdal;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;

import javax.annotation.Nonnull;

import org.junit.jupiter.api.Assertions;

import com.cmclinnovations.stack.clients.utils.JsonHelper;

public abstract class AbstractOptionsFactory<T extends CommonOptions<T>> {

    interface IArgs<T extends CommonOptions<T>> {

        public AbstractOptionsFactory<T> getFactory();

        default T getOptions() {
            return getFactory().createOptions(this);
        }

        String getCommand();

        String name();
    }

    protected static final String SRID_IN = "EPSG:3264";
    protected static final String SRID_OUT = "EPSG:4326";
    protected static final String X_POSSIBLE_NAMES_VALUE = "x";
    protected static final String Y_POSSIBLE_NAMES_VALUE = "y";
    protected static final String OTHER_OPTIONS_SELECT_VALUE = "a,b,c";

    private static final <T> T getOptionsFromFile(String name, Class<T> classBeingTested) {
        T options;
        String relativePath = classBeingTested.getSimpleName() + "/" + name + ".json";
        try (InputStream is = CommonOptionsTest.class.getResourceAsStream(relativePath)) {

            Assertions.assertNotNull(is, () -> "Ensure the file '"
                    + CommonOptionsTest.class.getResource(relativePath)
                    + "' exists and is a valid JSON file.");

            options = JsonHelper.getMapper().readValue(is, classBeingTested);

            Assertions.assertNotNull(options, () -> "Ensure the file '"
                    + CommonOptionsTest.class.getResource(relativePath)
                    + "' is a valid JSON file.");

        } catch (IOException ex) {
            Assertions.fail(ex);
            options = (T) null; // appeasing the compiler: this line will never be executed.
        }
        return options;
    }

    protected final Class<T> classBeingTested;

    protected AbstractOptionsFactory(Class<T> classBeingTested) {
        this.classBeingTested = classBeingTested;
    }

    protected abstract void configureOptions(IArgs<T> optionEnum, T options);

    private <O extends IArgs<T>> T createOptions(@Nonnull O argsEnum) {
        T options;
        String name = argsEnum.name();
        int index = name.lastIndexOf("File");
        if (index > 0) {
            options = getOptionsFromFile(name.substring(0, index), classBeingTested);
        } else {
            try {
                options = classBeingTested.getConstructor().newInstance();

                configureOptions(argsEnum, options);
                return options;
            } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
                    | InvocationTargetException | NoSuchMethodException | SecurityException ex) {
                Assertions.fail(ex);
                return null;
            }
        }
        return options;
    }

}
