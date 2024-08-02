package org.linkeddatafragments.views;

import freemarker.template.TemplateException;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import javax.servlet.ServletOutputStream;

import org.json.JSONObject;
import org.linkeddatafragments.datasource.IDataSource;
import org.linkeddatafragments.fragments.ILinkedDataFragment;
import org.linkeddatafragments.fragments.ILinkedDataFragmentRequest;
import org.linkeddatafragments.fragments.tpf.ITriplePatternFragment;
import org.linkeddatafragments.fragments.tpf.ITriplePatternFragmentRequest;

/**
 * Base class of any implementation for ITriplePatternFragment.
 * 
 * @author Miel Vander Sande
 */
public abstract class TriplePatternFragmentWriterBase extends LinkedDataFragmentWriterBase implements ILinkedDataFragmentWriter {

    /**
     *
     * @param prefixes
     * @param datasources
     */
    public TriplePatternFragmentWriterBase(Map<String, String> prefixes, HashMap<String, IDataSource> datasources) {
        super(prefixes, datasources);
    }
    
    @Override
    public void writeFragment(JSONObject queryObject, ServletOutputStream outputStream, IDataSource datasource, ILinkedDataFragment fragment, ILinkedDataFragmentRequest ldfRequest) throws Exception {
        writeFragment(queryObject, outputStream, datasource, (ITriplePatternFragment) fragment, (ITriplePatternFragmentRequest) ldfRequest);
    }
    
    /**
     *
     * @param outputStream
     * @param datasource
     * @param fragment
     * @param tpfRequest
     * @throws IOException
     * @throws TemplateException
     */
    abstract public void writeFragment(JSONObject queryObject, ServletOutputStream outputStream, IDataSource datasource, ITriplePatternFragment fragment,  ITriplePatternFragmentRequest tpfRequest) throws IOException, TemplateException;
}
