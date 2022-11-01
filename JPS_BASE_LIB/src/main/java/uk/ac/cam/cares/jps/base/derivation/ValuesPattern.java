package uk.ac.cam.cares.jps.base.derivation;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.eclipse.rdf4j.sparqlbuilder.rdf.RdfObject;
import org.eclipse.rdf4j.sparqlbuilder.rdf.RdfLiteral.NumericLiteral;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/**
 * This class is used to create a VALUES pattern for SPARQL queries/updates.
 * It should be noted that although the official SPARQL 1.1 specification allows undefined values in the VALUES clause,
 * it is not implemented here. An exception will be thrown if the number of values for a variable is not equal to the
 * number of values for all other variables.
 * For official syntax in SPARQL 1.1, see https://www.w3.org/TR/sparql11-query/#inline-data
 *
 * @author Jiaru Bai
 * @author Kok Foong Lee
 */
class ValuesPattern implements GraphPattern {
	public static final String ILLEGAL_INIT_ARG_ERROR_MSG = "Illegal init argument: Each value pair MUST contain all variables, received: ";
	public static final String ILLEGAL_VALUEPAIR_ERROR_MSG = "Illegal value pair to add: The added value pair MUST contain the same number of values as existing value pairs, existing: ";

	List<Variable> variables;
	List<RdfObject[]> valuePairs;

	/**
	 * Constructor for a VALUES clause with a single variable and multiple possible values.
	 *
	 * @param variable
	 * @param valuesForSingleVariable
	 */
	public ValuesPattern(Variable variable, RdfObject... valuesForSingleVariable) {
		this.variables = Arrays.asList(variable);
		this.valuePairs = new ArrayList<>();
		for (RdfObject value : valuesForSingleVariable) {
			this.valuePairs.add(new RdfObject[] {value});
		}
	}

	/**
	 * Constructor for a VALUES clause with a single variable and a list of possible values.
	 *
	 * @param variable
	 * @param valuesListForSingleVariable
	 */
	public ValuesPattern(Variable variable, List<RdfObject> valuesListForSingleVariable) {
		this.variables = Arrays.asList(variable);
		this.valuePairs = new ArrayList<>();
		for (RdfObject value : valuesListForSingleVariable) {
			this.valuePairs.add(new RdfObject[] {value});
		}
	}

	/**
	 * Constructor for a VALUES clause with multiple variables and multiple set of possible value pairs.
	 * Note that each value pair MUST contain all variables. Otherwise, an exception will be thrown.
	 * The UNDEF keyword shown in the SPARQL 1.1 documentation https://www.w3.org/TR/sparql11-query/#inline-data
	 * is NOT implemented yet.
	 *
	 * @param variables
	 * @param valuePairs
	 */
	public ValuesPattern(List<Variable> variables, List<RdfObject[]> valuePairsForMultipleVariables) {
		this.variables = variables;
		if (!valuePairsForMultipleVariables.stream().allMatch(arr -> arr.length == variables.size())) {
			throw new JPSRuntimeException(ILLEGAL_INIT_ARG_ERROR_MSG +
				formulateStringForVariables(variables) + formulateStringForValuePairs(valuePairsForMultipleVariables));
		}
		this.valuePairs = new ArrayList<>(valuePairsForMultipleVariables);
	}

	/**
	 * Constructor for a VALUES clause with (multiple) variables. Concrete value (pairs) needs to be added later.
	 *
	 * @param variables
	 */
	public ValuesPattern(Variable... variables) {
		this.variables = Arrays.asList(variables);
		this.valuePairs = new ArrayList<>();
	}

	/**
	 * Add a value pair to the VALUES clause.
	 * Note that the added value pair MUST contain the same number of values as existing value pairs.
	 * Otherwise, an exception will be thrown.
	 * The UNDEF keyword shown in the SPARQL 1.1 documentation https://www.w3.org/TR/sparql11-query/#inline-data
	 * is NOT implemented yet.
	 *
	 * @param valuePairForMultipleVariables
	 */
	public void addValuePairForMultipleVariables(RdfObject... valuePairForMultipleVariables) {
		if (valuePairForMultipleVariables.length != variables.size()) {
			throw new JPSRuntimeException(ILLEGAL_VALUEPAIR_ERROR_MSG +
				this.getQueryString() + ", received: " + formulateStringForValuePairs(valuePairForMultipleVariables));
		}
		this.valuePairs.add(valuePairForMultipleVariables);
	}

	@Override
	public String getQueryString() {
		return formulateStringForVariables(this.variables) + " " + formulateStringForValuePairs(this.valuePairs);
	}

	//////////////////////////////////////////////////////////////////////////////////////////////////
	// Helper method to formulate the string for the variables and value pairs in the VALUES clause //
	//////////////////////////////////////////////////////////////////////////////////////////////////
	/**
	 * Helper method to formulate the string for the variables in the VALUES clause.
	 *
	 * @param variables
	 * @return
	 */
	private String formulateStringForVariables(List<Variable> variables) {
		StringBuilder sb = new StringBuilder();
		sb.append(" VALUES ( ");
		sb.append(variables.stream().map(v -> v.getQueryString()).collect(Collectors.joining(" ")));
		sb.append(" ) ");
		return sb.toString();
	}

	/**
	 * Helper method to formulate the string for the value pairs in the VALUES clause.
	 *
	 * @param valuePairs
	 * @return
	 */
	private String formulateStringForValuePairs(List<RdfObject[]> valuePairs) {
		StringBuilder sb = new StringBuilder();
		sb.append(" { ");
		for (RdfObject[] valuePair : valuePairs) {
			sb.append("(");
			sb.append(Arrays.stream(valuePair).map(v -> getQueryStringEscapeInfinityAndNaN(v)).collect(Collectors.joining(" ")));
			sb.append(") ");
		}
		sb.append("} ");
		return sb.toString();
	}

	/**
	 * Helper method to formulate the string for ONE value pair in the VALUES clause.
	 *
	 * @param valuePair
	 * @return
	 */
	private String formulateStringForValuePairs(RdfObject... values) {
		List<RdfObject[]> pair = new ArrayList<>();
		pair.add(values);
		return formulateStringForValuePairs(pair);
	}

	/**
	 * Helper method to escape the infinity and NaN values in the VALUES clause.
	 *
	 * @param value
	 * @return
	 */
	private String getQueryStringEscapeInfinityAndNaN(RdfObject v) {
		if (v instanceof NumericLiteral) {
			if (v.equals(Rdf.literalOf(Double.POSITIVE_INFINITY))) {
				return "\"Infinity\"^^<http://www.w3.org/2001/XMLSchema#double>";
			} else if (v.equals(Rdf.literalOf(Double.NEGATIVE_INFINITY))) {
				return "\"-Infinity\"^^<http://www.w3.org/2001/XMLSchema#double>";
			} else if (v.equals(Rdf.literalOf(Double.NaN))) {
				return "\"NaN\"^^<http://www.w3.org/2001/XMLSchema#double>";
			}
		}
		return v.getQueryString();
	}

}
