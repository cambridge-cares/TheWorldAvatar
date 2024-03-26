package uk.ac.cam.cares.jps.base.derivation;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.eclipse.rdf4j.sparqlbuilder.rdf.RdfObject;
import org.eclipse.rdf4j.sparqlbuilder.rdf.RdfLiteral.BooleanLiteral;
import org.eclipse.rdf4j.sparqlbuilder.rdf.RdfLiteral.NumericLiteral;
import org.eclipse.rdf4j.sparqlbuilder.rdf.RdfLiteral.StringLiteral;
import org.junit.Assert;
import org.junit.jupiter.api.Test;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class ValuesPatternTest extends TestCase {
    static ValuesPattern valuesPattern;
    // string literals
    static Variable var1 = SparqlBuilder.var("var1");
    static StringLiteral var1Value1 = Rdf.literalOf("http://var1Value1");
    static StringLiteral var1Value2 = Rdf.literalOf("var1Value2");

    // iris
    static Variable var2 = SparqlBuilder.var("var2");
    static Iri var2Value1 = Rdf.iri("http://var2Value1");
    static Iri var2Value2 = Rdf.iri("http://var2Value2");

    // integer literals
    static Variable var3 = SparqlBuilder.var("var3");
    static NumericLiteral var3Value1 = Rdf.literalOf(1);
    static NumericLiteral var3Value2 = Rdf.literalOf(2);

    // float numbers literals
    static Variable var4 = SparqlBuilder.var("var4");
    static NumericLiteral var4Value1 = Rdf.literalOf(1.1);
    static NumericLiteral var4Value2 = Rdf.literalOf(2.2);

    // boolean literals
    static Variable var5 = SparqlBuilder.var("var5");
    static BooleanLiteral var5Value1 = Rdf.literalOf(true);
    static BooleanLiteral var5Value2 = Rdf.literalOf(false);

    // Infinity/-Infinity/NaN as number literals
    static Variable var6 = SparqlBuilder.var("var6");
    static NumericLiteral var6Value1 = Rdf.literalOf(Double.POSITIVE_INFINITY);
    static NumericLiteral var6Value2 = Rdf.literalOf(Double.NEGATIVE_INFINITY);
    static NumericLiteral var6Value3 = Rdf.literalOf(Double.NaN);

    // Infinity/-Infinity/NaN as string literals
    static Variable var7 = SparqlBuilder.var("var7");
    static StringLiteral var7Value1 = Rdf.literalOf("Infinity");
    static StringLiteral var7Value2 = Rdf.literalOf("-Infinity");
    static StringLiteral var7Value3 = Rdf.literalOf("NaN");

    static List<Variable> varList = Arrays.asList(var1, var2, var3, var4, var5);
    static List<RdfObject[]> valuePairs = new ArrayList<>(
        Arrays.asList(
            new RdfObject[] {var1Value1, var2Value1, var3Value1, var4Value1, var5Value1},
            new RdfObject[] {var1Value2, var2Value2, var3Value2, var4Value2, var5Value2}
        )
    );

    @Test
	public void testConstructor1() {
        // constructor ValuesPattern(Variable, RdfObject...)
        valuesPattern = new ValuesPattern(var1, var1Value1, var1Value2);
        Assert.assertEquals(
            "VALUES ( ?var1 )   { (\"http://var1Value1\") (\"var1Value2\") }",
            valuesPattern.getQueryString().trim());
    }

    @Test
    public void testConstructor2() {
        // constructor ValuesPattern(Variable, List<RdfObject>)
        valuesPattern = new ValuesPattern(var1, new ArrayList<RdfObject>(Arrays.asList(var1Value1, var1Value2)));
        Assert.assertEquals(
            "VALUES ( ?var1 )   { (\"http://var1Value1\") (\"var1Value2\") }",
            valuesPattern.getQueryString().trim());
    }

    @Test
    public void testConstructor3() {
        // constructor ValuesPattern(List<Variable>, List<RdfObject[]>)
        valuesPattern = new ValuesPattern(varList, valuePairs);
        Assert.assertEquals(
            "VALUES ( ?var1 ?var2 ?var3 ?var4 ?var5 )   { (\"http://var1Value1\" <http://var2Value1> 1 1.1 true) (\"var1Value2\" <http://var2Value2> 2 2.2 false) }",
            valuesPattern.getQueryString().trim());
    }

    @Test
    public void testConstructor4() {
        // constructor ValuesPattern(Variable...)
        valuesPattern = new ValuesPattern(var1, var2, var3, var4, var5);
        Assert.assertEquals(
            "VALUES ( ?var1 ?var2 ?var3 ?var4 ?var5 )   { }",
            valuesPattern.getQueryString().trim());
    }

    @Test
    public void testAddValuePairForMultipleVariables() {
        valuesPattern = new ValuesPattern(var1, var2, var3, var4, var5);
        Assert.assertEquals(
            "VALUES ( ?var1 ?var2 ?var3 ?var4 ?var5 )   { }",
            valuesPattern.getQueryString().trim());
        valuesPattern.addValuePairForMultipleVariables(var1Value1, var2Value1, var3Value1, var4Value1, var5Value1);
        Assert.assertEquals(
            "VALUES ( ?var1 ?var2 ?var3 ?var4 ?var5 )   { (\"http://var1Value1\" <http://var2Value1> 1 1.1 true) }",
            valuesPattern.getQueryString().trim());
        valuesPattern.addValuePairForMultipleVariables(var1Value2, var2Value2, var3Value2, var4Value2, var5Value2);
        Assert.assertEquals(
            "VALUES ( ?var1 ?var2 ?var3 ?var4 ?var5 )   { (\"http://var1Value1\" <http://var2Value1> 1 1.1 true) (\"var1Value2\" <http://var2Value2> 2 2.2 false) }",
            valuesPattern.getQueryString().trim());
    }

    @Test
    public void testSpecialNumberEscape() {
        valuesPattern = new ValuesPattern(var6, var6Value1, var6Value2, var6Value3);
        Assert.assertEquals(
            "VALUES ( ?var6 )   { (\"Infinity\"^^<http://www.w3.org/2001/XMLSchema#double>) (\"-Infinity\"^^<http://www.w3.org/2001/XMLSchema#double>) (\"NaN\"^^<http://www.w3.org/2001/XMLSchema#double>) }",
            valuesPattern.getQueryString().trim());

        // Infinity/-Infinity/NaN as string literals will not be escaped, i.e. they will be treated as string literals
        valuesPattern = new ValuesPattern(var7, var7Value1, var7Value2, var7Value3);
        Assert.assertEquals(
            "VALUES ( ?var7 )   { (\"Infinity\") (\"-Infinity\") (\"NaN\") }",
            valuesPattern.getQueryString().trim());
    }

    @Test
    public void testExceptions() {
        // test exception - ILLEGAL_INIT_ARG_ERROR_MSG
        List<RdfObject[]> valuePairsIncorrectLength = new ArrayList<>();
        valuePairsIncorrectLength.add(new RdfObject[] {var1Value1, var2Value1, var3Value1, var4Value1});
        JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class,
				() -> new ValuesPattern(varList, valuePairsIncorrectLength));
		Assert.assertTrue(e.getMessage().contains(ValuesPattern.ILLEGAL_INIT_ARG_ERROR_MSG));

        // test exception - ILLEGAL_VALUEPAIR_ERROR_MSG
        valuesPattern = new ValuesPattern(var1, var2, var3, var4, var5);
        JPSRuntimeException e2 = Assert.assertThrows(JPSRuntimeException.class,
                () -> valuesPattern.addValuePairForMultipleVariables(var1Value1, var2Value1, var3Value1, var4Value1));
        Assert.assertTrue(e2.getMessage().contains(ValuesPattern.ILLEGAL_VALUEPAIR_ERROR_MSG));
    }

    @Test
    public void testGraphPatternsUnion() {
        GraphPattern graphPattern = Rdf.iri("http://s").has(Rdf.iri("http://p"), Rdf.iri("http://o"));
        valuesPattern = new ValuesPattern(var1, var1Value1, var1Value2);
        Assert.assertEquals(
            "VALUES ( ?var1 )   { (\"http://var1Value1\") (\"var1Value2\") }",
            valuesPattern.getQueryString().trim());
        Assert.assertEquals(
            "{ <http://s> <http://p> <http://o> . } UNION {  VALUES ( ?var1 )   { (\"http://var1Value1\") (\"var1Value2\") }  }",
            GraphPatterns.union(graphPattern, valuesPattern).getQueryString().trim());
    }
}
