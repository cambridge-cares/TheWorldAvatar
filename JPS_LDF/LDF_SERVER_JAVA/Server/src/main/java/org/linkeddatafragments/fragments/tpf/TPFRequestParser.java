package org.linkeddatafragments.fragments.tpf;

import javax.servlet.http.HttpServletRequest;

import org.linkeddatafragments.config.ConfigReader;
import org.linkeddatafragments.fragments.FragmentRequestParserBase;
import org.linkeddatafragments.fragments.IFragmentRequestParser;
import org.linkeddatafragments.fragments.ILinkedDataFragmentRequest;
import org.linkeddatafragments.util.TriplePatternElementParser;

/**
 * An {@link IFragmentRequestParser} for {@link ITriplePatternFragmentRequest}s.
 *
 * @param <ConstantTermType>
 * @param <NamedVarType>
 *
 * @author <a href="http://olafhartig.de">Olaf Hartig</a>
 * @param <AnonVarType>
 */
public class TPFRequestParser<ConstantTermType,NamedVarType,AnonVarType>
    extends FragmentRequestParserBase
{
    public final TriplePatternElementParser<ConstantTermType,NamedVarType,AnonVarType> elmtParser;

    /**
     *
     * @param elmtParser
     */
    public TPFRequestParser(
                final TriplePatternElementParser<ConstantTermType,NamedVarType,AnonVarType> elmtParser )
    {
        this.elmtParser = elmtParser;
    }

    /**
     *
     * @param httpRequest
     * @param config
     * @return
     * @throws IllegalArgumentException
     */
    @Override
    protected Worker getWorker( final HttpServletRequest httpRequest,
                                final ConfigReader config )
                                               throws IllegalArgumentException
    {
        return new Worker( httpRequest, config );
    }

    /**
     *
     */
    protected class Worker extends FragmentRequestParserBase.Worker
    {   

        /**
         *
         * @param request
         * @param config
         */
        public Worker( final HttpServletRequest request,
                       final ConfigReader config )
        {
            super( request, config );
        }

        /**
         *
         * @return
         * @throws IllegalArgumentException
         */
        @Override
        public ILinkedDataFragmentRequest createFragmentRequest()
                                               throws IllegalArgumentException
        {
            return new TriplePatternFragmentRequestImpl<ConstantTermType,NamedVarType,AnonVarType>(
                                                         getFragmentURL(),
                                                         getDatasetURL(),
                                                         pageNumberWasRequested,
                                                         pageNumber,
                                                         getSubject(),
                                                         getPredicate(),
                                                         getObject() );
        }

        /**
         *
         * @return
         */
        public ITriplePatternElement<ConstantTermType,NamedVarType,AnonVarType> getSubject() {
            return getParameterAsTriplePatternElement(
                    ITriplePatternFragmentRequest.PARAMETERNAME_SUBJ );
        }

        /**
         *
         * @return
         */
        public ITriplePatternElement<ConstantTermType,NamedVarType,AnonVarType> getPredicate() {
            return getParameterAsTriplePatternElement(
                    ITriplePatternFragmentRequest.PARAMETERNAME_PRED );
        }

        /**
         *
         * @return
         */
        public ITriplePatternElement<ConstantTermType,NamedVarType,AnonVarType> getObject() {
            return getParameterAsTriplePatternElement(
                    ITriplePatternFragmentRequest.PARAMETERNAME_OBJ );
        }

        /**
         *
         * @param paramName
         * @return
         */
        public ITriplePatternElement<ConstantTermType,NamedVarType,AnonVarType>
                   getParameterAsTriplePatternElement( final String paramName )
        {
            final String parameter = request.getParameter( paramName );
            return elmtParser.parseIntoTriplePatternElement( parameter );
        }

    } // end of class Worker

}
