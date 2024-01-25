package org.linkeddatafragments.util;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parses strings (as obtained from HTTP request parameters) into RDF terms. 
 *
 * @param <TermType> type for representing RDF terms
 *
 * @author <a href="http://olafhartig.de">Olaf Hartig</a>
 */
abstract public class RDFTermParser<TermType>
{

    /**
     *
     */
    public static final Pattern STRINGPATTERN
                  = Pattern.compile("^\"(.*)\"(?:@(.*)|\\^\\^<?([^<>]*)>?)?$");

    /**
     *
     * @param param
     * @return
     */
    public TermType parseIntoRDFNode( final String param )
    {
        if ( param == null || param.isEmpty() )
            return handleUnparsableParameter( param );

        // identify the kind of RDF term based on the first character
        char firstChar = param.charAt(0);
        switch ( firstChar )
        {
            // blank node
            case '_':
                return createBlankNode( param );

            // angular brackets indicate a URI
            case '<':
                return createURI( param.substring(1, param.length()-1) );

            // quotes indicate a string
            case '"':
                Matcher matcher = STRINGPATTERN.matcher( param );
                if ( matcher.matches() ) {
                    String label   = matcher.group(1);
                    String langTag = matcher.group(2);
                    String typeURI = matcher.group(3);

                    if ( langTag != null )
                        return createLanguageLiteral( label, langTag );

                    else if ( typeURI != null )
                        return createTypedLiteral( label, typeURI );

                    else
                        return createPlainLiteral( label );
                }
                else
                    return handleUnparsableParameter( param );

            // assume it is a URI without angular brackets
            default:
                return createURI( param );
        }
    }

    /**
     *
     * @param label
     * @return
     */
    abstract public TermType createBlankNode( final String label );

    /**
     *
     * @param uri
     * @return
     */
    abstract public TermType createURI( final String uri );

    /**
     *
     * @param label
     * @param typeURI
     * @return
     */
    abstract public TermType createTypedLiteral( final String label,
                                                 final String typeURI );

    /**
     *
     * @param label
     * @param langTag
     * @return
     */
    abstract public TermType createLanguageLiteral( final String label,
                                                    final String langTag );

    /**
     *
     * @param label
     * @return
     */
    abstract public TermType createPlainLiteral( final String label );

    /**
     *
     * @param param
     * @return
     */
    abstract public TermType handleUnparsableParameter( final String param );

}
