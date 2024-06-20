package com.cmclinnovations.stack.clients.gdal;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;

import javax.annotation.Nonnull;

import org.junit.jupiter.api.Assertions;

import com.cmclinnovations.stack.clients.utils.JsonHelper;

public abstract class BaseOptionsTestHelper<T extends CommonOptions<T>> {

    private final String command;
    protected final Class<T> classBeingTested;

    protected <F extends BaseOptionsTestHelper<T>> BaseOptionsTestHelper(String command, Class<T> classBeingTested) {
        this.command = command;
        this.classBeingTested = classBeingTested;
    }

    public String getCommand() {
        return command;
    }

    protected abstract void configureOptions(ArgsEnum optionEnum, @Nonnull T options);

    protected final T createOptions(@Nonnull ArgsEnum argsEnum) {
        @Nonnull
        T options;
        String name = argsEnum.name();
        int index = name.lastIndexOf("File");
        if (index > 0) {
            options = getOptionsFromFile(name.substring(0, index), classBeingTested);
        } else {
            try {
                options = Assertions.assertInstanceOf(classBeingTested,
                        classBeingTested.getConstructor().newInstance());

                configureOptions(argsEnum, options);
            } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
                    | InvocationTargetException | NoSuchMethodException | SecurityException ex) {
                Assertions.fail(ex);
                throw new RuntimeException("Appeasing the compiler: this exception should never be thrown.");
            }
        }
        return options;
    }

    private static final <T> @Nonnull T getOptionsFromFile(String name, Class<T> classBeingTested) {
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
            throw new RuntimeException("Appeasing the compiler: this exception should never be thrown.");
        }
        return options;
    }
}
