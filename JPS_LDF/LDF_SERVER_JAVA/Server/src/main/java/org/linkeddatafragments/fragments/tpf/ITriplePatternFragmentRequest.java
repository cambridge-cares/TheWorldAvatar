package org.linkeddatafragments.fragments.tpf;

import org.linkeddatafragments.fragments.ILinkedDataFragmentRequest;

/**
 * Represents a request of a Triple Pattern Fragment (TPF).
 *
 * @param <ConstantTermType> type for representing constants in triple patterns
 *                           (i.e., URIs and literals)
 * @param <NamedVarType> type for representing named variables in triple patterns
 * @param <AnonVarType> type for representing anonymous variables in triple
 *                      patterns (i.e., variables denoted by a blank node)
 *
 * @author <a href="http://olafhartig.de">Olaf Hartig</a>
 */
public interface ITriplePatternFragmentRequest<ConstantTermType,NamedVarType,AnonVarType>
    extends ILinkedDataFragmentRequest
{

    /**
     *
     */
    public final static String PARAMETERNAME_SUBJ = "subject";

    /**
     *
     */
    public final static String PARAMETERNAME_PRED = "predicate";

    /**
     *
     */
    public final static String PARAMETERNAME_OBJ = "object";

    /**
     * Returns the subject position of the requested triple pattern.
     * @return 
     */
    ITriplePatternElement<ConstantTermType,NamedVarType,AnonVarType> getSubject();

    /**
     * Returns the predicate position of the requested triple pattern.
     * @return 
     */
    ITriplePatternElement<ConstantTermType,NamedVarType,AnonVarType> getPredicate();

    /**
     * Returns the object position of the requested triple pattern.
     * @return 
     */
    ITriplePatternElement<ConstantTermType,NamedVarType,AnonVarType> getObject();
}
