package org.linkeddatafragments.util;

import org.linkeddatafragments.fragments.tpf.ITriplePatternElement;
import org.linkeddatafragments.fragments.tpf.TriplePatternElementFactory;

/**
 * Parses strings (as obtained from HTTP request parameters) into
 * {@link ITriplePatternElement}s. 
 *
 * @param <ConstantTermType> type for representing constants in triple patterns
 *                           (i.e., URIs and literals)
 * @param <NamedVarType> type for representing named variables in triple patterns
 * @param <AnonVarType> type for representing anonymous variables in triple
 *                      patterns (i.e., variables denoted by a blank node)
 *
 * @author <a href="http://olafhartig.de">Olaf Hartig</a>
 * @author Ruben Verborgh
 */
abstract public
    class TriplePatternElementParser<ConstantTermType,NamedVarType,AnonVarType>
        extends RDFTermParser<ConstantTermType>
{

    /**
     *
     */
    public final TriplePatternElementFactory<ConstantTermType,NamedVarType,AnonVarType>
        factory = new TriplePatternElementFactory<ConstantTermType,NamedVarType,AnonVarType>();

    /**
     *
     * @param param
     * @return
     */
    public ITriplePatternElement<ConstantTermType,NamedVarType,AnonVarType>
                            parseIntoTriplePatternElement( final String param )
    {
        // nothing or empty indicates an unspecified variable
        if ( param == null || param.isEmpty() )
            return factory.createUnspecifiedVariable();

        // identify the kind of RDF term based on the first character
        char firstChar = param.charAt(0);
        switch ( firstChar )
        {
            // specific variable that has a name
            case '?':
            {
                final String varName = param.substring(1);
                final NamedVarType var = createNamedVariable( varName );
                return factory.createNamedVariable( var );
            }

            // specific variable that is denoted by a blank node
            case '_':
            {
                final AnonVarType var = createAnonymousVariable( param );
                return factory.createAnonymousVariable( var );
            }

            // assume it is an RDF term
            default:
                return factory.createConstantRDFTerm( parseIntoRDFNode(param) );
        }
    }

    /**
     *
     * @param varName
     * @return
     */
    abstract public NamedVarType createNamedVariable( final String varName );

    /**
     *
     * @param label
     * @return
     */
    abstract public AnonVarType createAnonymousVariable( final String label );
}
