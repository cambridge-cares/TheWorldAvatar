package uk.ac.cam.cares.jps.agent.ifc2ontobim.ttlparser;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class StringUtilsTest {
    private final String testStr = "sample#test#string#one#two";
    private final String testChar = "#";

    @Test
    void testGetStringBeforeFirstCharacterOccurrence() {
        assertEquals("sample", StringUtils.getStringBeforeFirstCharacterOccurrence(testStr, testChar));
    }

    @Test
    void testGetStringBeforeFirstCharacterOccurrenceFail() {
        assertEquals(testStr, StringUtils.getStringBeforeFirstCharacterOccurrence(testStr, "+"));
    }

    @Test
    void testGetStringBeforeLastCharacterOccurrence() {
        assertEquals("sample#test#string#one", StringUtils.getStringBeforeLastCharacterOccurrence(testStr, testChar));
    }

    @Test
    void testGetStringBeforeLastCharacterOccurrenceFail() {
        assertEquals(testStr, StringUtils.getStringBeforeLastCharacterOccurrence(testStr, "+"));
    }

    @Test
    void testGetStringAfterFirstCharacterOccurrence() {
        assertEquals("test#string#one#two", StringUtils.getStringAfterFirstCharacterOccurrence(testStr, testChar));
    }

    @Test
    void testGetStringAfterFirstCharacterOccurrenceFail() {
        assertEquals(testStr, StringUtils.getStringAfterFirstCharacterOccurrence(testStr, "+"));
    }

    @Test
    void testGetStringAfterNCharacterOccurrence() {
        assertEquals("string#one#two", StringUtils.getStringAfterNCharacterOccurrence(testStr, testChar, 2));
        assertEquals("one#two", StringUtils.getStringAfterNCharacterOccurrence(testStr, testChar, 3));
    }

    @Test
    void testGetStringAfterNCharacterOccurrenceFail() {
        assertEquals(testStr, StringUtils.getStringAfterNCharacterOccurrence(testStr, "+", 2));
    }

    @Test
    void testGetStringAfterLastCharacterOccurrence() {
        assertEquals("two", StringUtils.getStringAfterLastCharacterOccurrence(testStr, testChar));
    }

    @Test
    void testGetStringAfterLastCharacterOccurrenceFail() {
        assertEquals(testStr, StringUtils.getStringAfterLastCharacterOccurrence(testStr, "+"));
    }
}