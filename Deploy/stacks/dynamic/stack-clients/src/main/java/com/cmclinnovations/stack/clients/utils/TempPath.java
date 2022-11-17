package com.cmclinnovations.stack.clients.utils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.UserPrincipal;

public abstract class TempPath implements AutoCloseable {

    /**
     *
     */
    private static final String COMMON_USER = "1000";

    private final Path path;

    private final UserPrincipal commonUser;

    protected Path changeOwner(Path path) {
        try {
            return Files.setOwner(path, commonUser);
        } catch (IOException ex) {
            throw new RuntimeException(
                    "Failed to change the owner of the file/directory '" + path + "' to '" + commonUser.getName()
                            + "'.",
                    ex);
        }
    }

    protected TempPath(Path path) {

        try {
            this.commonUser = path.getFileSystem().getUserPrincipalLookupService().lookupPrincipalByName(COMMON_USER);
        } catch (IOException e) {
            throw new RuntimeException("Failed to lookup user '" + COMMON_USER + "'.");
        }
        try {
            Files.setOwner(path, commonUser);
        } catch (IOException e) {
            throw new RuntimeException(
                    "Failed to set owner of temporary file/directory '" + path + "' to user '" + COMMON_USER + "'.");
        }

        this.path = path;
    }

    public Path getPath() {
        return path;
    }

    @Override
    public String toString() {
        return path.toString();
    }

    @Override
    public abstract void close() throws RuntimeException;

}
