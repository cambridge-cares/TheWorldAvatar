package uk.ac.cam.cares.jps.base.derivation;

import org.eclipse.rdf4j.sparqlbuilder.core.Assignable;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.util.SparqlBuilderUtils;

/**
 * TODO remove this class once we moved to Java 11 and upgradeed to rdf4j-sparqlbuilder>=4.0.0
 * This is a workaround for the SparqlBuilder library.
 * The BIND clause is added since their 4.0.0 release, which requires minimum Java 11.
 * Given the jps-base-lib is still Java 8, the clause is implemented here.
 * For more information, see:
 * https://github.com/eclipse/rdf4j/blob/main/core/sparqlbuilder/src/main/java/org/eclipse/rdf4j/sparqlbuilder/constraint/Bind.java
 *
 */
public class Bind implements GraphPattern {
	private static final String AS = " AS ";
	private final Assignable expression;
	private final Variable var;

	Bind(Assignable exp, Variable var) {
		this.expression = exp;
		this.var = var;
	}

	@Override
	public String getQueryString() {
		return "BIND"
				+ SparqlBuilderUtils.getParenthesizedString(
						expression.getQueryString() + AS + var.getQueryString());
	}
}
