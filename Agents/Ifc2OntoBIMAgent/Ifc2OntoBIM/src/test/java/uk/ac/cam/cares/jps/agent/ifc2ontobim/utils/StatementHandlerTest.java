package uk.ac.cam.cares.jps.agent.ifc2ontobim.utils;

import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.util.LinkedHashSet;

import static org.junit.jupiter.api.Assertions.*;

class StatementHandlerTest {
    private static final String baseURI = "http://www.example.org/";
    private static final String testSubject = baseURI + "IfcWall_12";
    private static final String testProperty = baseURI + "has";
    private static final String testObject = baseURI + "Door_12";
    private static final String testBIMClass = "Wall";
    private static final String testRenamedInstance = baseURI + "Wall_12";

    private static final String testStringLiteral = "Name is wall";
    private static final Double testDoubleLiteral = 1.02;
    private static final Integer testIntegerLiteral = 5;


    @Test
    void testCreateInstanceFromIRI() {
        NamespaceMapper.setBaseNameSpace(baseURI);
        String result = StatementHandler.createInstanceFromIRI(testSubject, testBIMClass);
        // Test that the generated instance is correct
        assertEquals(testRenamedInstance, result);
    }

    @Test
    void testCreateInstanceFromOptionalIRI() {
        NamespaceMapper.setBaseNameSpace(baseURI);
        String result = StatementHandler.createInstanceFromOptionalIRI(testSubject, testBIMClass);
        // Test that the generated instance is correct
        assertEquals(testRenamedInstance, result);
        // Test that when IRI is optional ie null, it returns null
        assertNull(StatementHandler.createInstanceFromOptionalIRI(null, testBIMClass));
    }

    @Test
    void testAddStatementForThreeNodes() {
        // Set up
        LinkedHashSet<Statement> testSet = new LinkedHashSet<>();
        // Execute method
        StatementHandler.addStatement(testSet, testSubject, testProperty, testObject);
        // Test that only one statement is generated
        assertEquals(1, testSet.size());
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
        assertEquals(1, testSet.size());
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
        assertEquals(1, testSet.size());
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
        assertEquals(testSet.size(), 1);
        // Get the first statement
        Statement result = testSet.iterator().next();
        // Test that the statement values are correct
        assertEquals(testSubject, result.getSubject().toString());
        assertEquals(testProperty, result.getPredicate().toString());
        assertEquals(testIntegerLiteral, result.getInt());
    }

    @Test
    void testAddOptionalStatementForThreeNodes() {
        // Set up
        LinkedHashSet<Statement> testSet = new LinkedHashSet<>();
        // Execute method for null literal
        StatementHandler.addOptionalStatement(testSet, testSubject, testProperty, (String) null);
        // Test that no statement is generated when it is null
        assertEquals(0, testSet.size());

        // Execute method for node with value
        StatementHandler.addOptionalStatement(testSet, testSubject, testProperty, testObject);
        // Test that only one statement is generated
        assertEquals(1, testSet.size());
        // Get the first statement
        Statement result = testSet.iterator().next();
        // Test that the statement values are correct
        assertEquals(testSubject, result.getSubject().toString());
        assertEquals(testProperty, result.getPredicate().toString());
        assertEquals(testObject, result.getObject().toString());
    }

    @Test
    void testAddOptionalStatementForStringLiteral() {
        // Set up
        LinkedHashSet<Statement> testSet = new LinkedHashSet<>();
        // Execute method for null literal
        StatementHandler.addOptionalStatement(testSet, testSubject, testProperty, null, false);
        // Test that no statement is generated when it is null
        assertEquals(0, testSet.size());

        // Execute method for literal with value
        StatementHandler.addOptionalStatement(testSet, testSubject, testProperty, testStringLiteral, false);
        // Test that only one statement is generated
        assertEquals(1, testSet.size());
        // Get the first statement
        Statement result = testSet.iterator().next();
        // Test that the statement values are correct
        assertEquals(testSubject, result.getSubject().toString());
        assertEquals(testProperty, result.getPredicate().toString());
        assertEquals(testStringLiteral, result.getString());
    }

    @Test
    void testAddOptionalStatementForNumberLiteral() {
        // Set up
        LinkedHashSet<Statement> testSet = new LinkedHashSet<>();
        // Execute method for null literal
        StatementHandler.addOptionalStatement(testSet, testSubject, testProperty, (Number) null);
        // Test that no statement is generated when it is null
        assertEquals(0, testSet.size());

        // Execute method for literal with value
        StatementHandler.addOptionalStatement(testSet, testSubject, testProperty, testIntegerLiteral);
        // Test that only one statement is generated
        assertEquals(1, testSet.size());
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