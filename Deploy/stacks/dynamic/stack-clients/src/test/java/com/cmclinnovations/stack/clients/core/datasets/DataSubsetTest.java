package com.cmclinnovations.stack.clients.core.datasets;

import java.util.Arrays;
import java.util.Collection;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;

@RunWith(Parameterized.class)
public class DataSubsetTest {

    private static String formatJson(String json) {
        return json.replace("'", "\"");
    }

    @Parameters(name = "{index}: {2}")
    public static Collection<Object[]> data() {
        return Arrays.asList(new Object[][] {
                { "{'type':'Raster','name':'elevation'}", Raster.class, "Raster" }
        });
    }

    private String json;

    private Class<? extends DataSubset> clazz;

    public DataSubsetTest(String json, Class<? extends DataSubset> clazz, String className) {
        this.json = formatJson(json);
        this.clazz = clazz;
    }

    @Test
    public void testClass() throws JsonMappingException, JsonProcessingException {

        DataSubset readValue = new ObjectMapper().readValue(json, DataSubset.class);
        Assert.assertEquals(clazz, readValue.getClass());
    }

}
