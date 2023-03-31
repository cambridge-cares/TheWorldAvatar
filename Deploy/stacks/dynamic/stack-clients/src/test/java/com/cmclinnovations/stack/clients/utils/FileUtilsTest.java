package com.cmclinnovations.stack.clients.utils;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.Assert;
import org.junit.Test;

public class FileUtilsTest {
    @Test
    public void testEnsureScriptsExecutable() {

    }

    @Test
    public void testSanitiseFilename() {
        Path path = Paths.get("abc", "efg", "+Hello()World-123.csv");
        Assert.assertEquals("_Hello__World_123.csv", FileUtils.sanitiseFilename(path));
    }

    @Test
    public void testFilterOnExtension() {
        Assert.assertEquals(List.of(
                Paths.get("abc", "efg", "123.csv"),
                Paths.get("abc", "efg", "345.csv")),
                Stream.of(
                        Paths.get("abc", "efg", "123.csv"),
                        Paths.get("abc", "efg", "345.csv"),
                        Paths.get("abc", "efg", "678.json"))
                        .filter(path -> FileUtils.filterOnExtension(path, ".csv"))
                        .collect(Collectors.toList()));
    }

    @Test
    public void testGetFileNameWithoutExtension() {
        Assert.assertEquals(List.of(
                "123", "345", "678"),
                Stream.of(
                        Paths.get("abc", "efg", "123.csv"),
                        Paths.get("abc", "efg", "345.csv"),
                        Paths.get("abc", "efg", "678.json"))
                        .map(path -> FileUtils.getFileNameWithoutExtension(path))
                        .collect(Collectors.toList()));
    }

}
