package org.linkeddatafragments.fragments.tpf;

/**
 * Represents an element of a triple pattern (i.e., subject, predicate, object). 
 *
 * @param <ConstantTermType> type for representing constants in triple patterns
 *                           (i.e., URIs and literals)
 * @param <NamedVarType> type for representing named variables in triple patterns
 * @param <AnonVarType> type for representing anonymous variables in triple
 *                      patterns (i.e., variables denoted by a blank node)
 *
 * @author <a href="http://olafhartig.de">Olaf Hartig</a>
 */
public interface ITriplePatternElement<ConstantTermType,NamedVarType,AnonVarType>
{
    /**
     * Returns true if this element is a variable (specific or unspecified).
     * @return 
     */
    boolean isVariable();

    /**
     * Returns true if this element is a specific variable, and false if either
     * it is not a variable (but a URI or literal) or it is some variable that
     * is not specified. The latter (unspecified variables) is possible because
     * when a client requests a triple pattern fragment, it may omit triple
     * pattern related parameters.
     * @return 
     */
    boolean isSpecificVariable();

    /**
     * Returns true if this element is a specific variable that has a name
     * (i.e., it is denoted by a string that begins with a question mark),
     * and false if either it is not a specific variable or it is a specific
     * variable that is denoted by a blank node.
     *
     * If this element is a specific variable that has a name (that is, this
     * method returns true), the named variable can be obtained by the method
     * {@link #asNamedVariable()}.
     * @return 
     */
    boolean isNamedVariable();

    /**
     * Returns a representation of this element as a named variable (assuming
     * it is a specific variable that has a name).
     *
     * @return 
     * @throws UnsupportedOperationException
     *         If this element is not a specific variable that has a name
     *         (i.e., if {@link #isNamedVariable()} returns false).
     */
    NamedVarType asNamedVariable() throws UnsupportedOperationException;

    /**
     * Returns true if this element is a specific variable that does not have
     * a name (i.e., it is denoted by a blank node), and false if either it is
     * not a specific variable or it is a specific variable that has a name.
     *
     * If this element is a specific variable denoted by a blank node (that is,
     * this method returns true), the blank node can be obtained by the method
     * {@link #asAnonymousVariable()}.
     * @return 
     */
    boolean isAnonymousVariable();

    /**
     * Returns a representation of this element as a blank node (assuming
     * it is a specific, but non-named variable).
     *
     * @return 
     * @throws UnsupportedOperationException
     *         If this element is not a specific anonymous variable (i.e.,
     *         if {@link #isAnonymousVariable()} returns false).
     */
    AnonVarType asAnonymousVariable() throws UnsupportedOperationException;

    /**
     * Returns a representation of this element as a constant RDF term (i.e.,
     * a URI or a literal).
     *
     * @return 
     * @throws UnsupportedOperationException
     *         If this element is not a constant RDF term but a variable
     *         (i.e., if {@link #isVariable()} returns true).
     */
    ConstantTermType asConstantTerm() throws UnsupportedOperationException;
}
