package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils;

import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.util.LinkedHashSet;

import static org.junit.jupiter.api.Assertions.*;

class StatementHandlerTest {
    private static final String testSubject = "http://www.example.org/Wall_12";
    private static final String testProperty = "http://www.example.org/has";
    private static final String testObject = "http://www.example.org/Door_12";
    private static final String testStringLiteral = "Name is wall";
    private static final Double testDoubleLiteral = 1.02;
    private static final Integer testIntegerLiteral = 5;


    @Test
    void testAddStatementForThreeNodes() {
        // Set up
        LinkedHashSet<Statement> testSet = new LinkedHashSet<>();
        // Execute method
        StatementHandler.addStatement(testSet, testSubject, testProperty, testObject);
        // Test that only one statement is generated
        assertTrue(testSet.size() == 1);
        // Get the first statement
        Statement result = testSet.iterator().next();
        // Test that the statement values are correct
        assertEquals(testSubject, result.getSubject().toString());
        assertEquals(testProperty, result.getPredicate().toString());
        assertEquals(testObject, result.getObject().toString());
    }

    @Test
    void testAddStatementForStringLiteral() {
        // Set up
        LinkedHashSet<Statement> testSet = new LinkedHashSet<>();
        // Execute method
        StatementHandler.addStatement(testSet, testSubject, testProperty, testStringLiteral, false);
        // Test that only one statement is generated
        assertTrue(testSet.size() == 1);
        // Get the first statement
        Statement result = testSet.iterator().next();
        // Test that the statement values are correct
        assertEquals(testSubject, result.getSubject().toString());
        assertEquals(testProperty, result.getPredicate().toString());
        assertEquals(testStringLiteral, result.getString());
    }

    @Test
    void testAddStatementWithNumberLiteralForDouble() {
        // Set up
        LinkedHashSet<Statement> testSet = new LinkedHashSet<>();
        // Execute method
        StatementHandler.addStatementWithNumberLiteral(testSet, testSubject, testProperty, testDoubleLiteral);
        // Test that only one statement is generated
        assertTrue(testSet.size() == 1);
        // Get the first statement
        Statement result = testSet.iterator().next();
        // Test that the statement values are correct
        assertEquals(testSubject, result.getSubject().toString());
        assertEquals(testProperty, result.getPredicate().toString());
        assertEquals(testDoubleLiteral, result.getDouble());
    }

    @Test
    void testAddStatementWithNumberLiteralForInteger() {
        // Set up
        LinkedHashSet<Statement> testSet = new LinkedHashSet<>();
        // Execute method
        StatementHandler.addStatementWithNumberLiteral(testSet, testSubject, testProperty, testIntegerLiteral);
        // Test that only one statement is generated
        assertTrue(testSet.size() == 1);
        // Get the first statement
        Statement result = testSet.iterator().next();
        // Test that the statement values are correct
        assertEquals(testSubject, result.getSubject().toString());
        assertEquals(testProperty, result.getPredicate().toString());
        assertEquals(testIntegerLiteral, result.getInt());
    }

    @Test
    void testAddStatementInvalidNode() {
        // Set up
        LinkedHashSet<Statement> testSet = new LinkedHashSet<>();
        // Execute method
        JPSRuntimeException thrownError = assertThrows(JPSRuntimeException.class, () ->
                StatementHandler.addStatement(testSet, testStringLiteral, testProperty, testObject));
        assertEquals("Invalid node format for " + testStringLiteral + ". Valid formats should start with http:// or https://", thrownError.getMessage());
    }

    @Test
    void testAddStatementWithNumberLiteralInvalidNode() {
        // Set up
        LinkedHashSet<Statement> testSet = new LinkedHashSet<>();
        // Execute method
        JPSRuntimeException thrownError = assertThrows(JPSRuntimeException.class, () ->
                StatementHandler.addStatementWithNumberLiteral(testSet, testStringLiteral, testProperty, testDoubleLiteral));
        assertEquals("Invalid node format for " + testStringLiteral + ". Valid formats should start with http:// or https://", thrownError.getMessage());
    }
}