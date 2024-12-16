package org.linkeddatafragments.views;

import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import freemarker.template.TemplateExceptionHandler;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletRequest;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.json.JSONObject;
import org.linkeddatafragments.datasource.IDataSource;
import org.linkeddatafragments.datasource.index.IndexDataSource;
import org.linkeddatafragments.fragments.ILinkedDataFragment;
import org.linkeddatafragments.fragments.tpf.ITriplePatternFragment;
import org.linkeddatafragments.fragments.tpf.ITriplePatternFragmentRequest;

//TODO: Refactor to a composable & flexible architecture using DataSource types, fragments types and request types

/**
 * Serializes an {@link ILinkedDataFragment} to the HTML format
 *
 * @author Miel Vander Sande
 */
public class HtmlTriplePatternFragmentWriterImpl extends TriplePatternFragmentWriterBase implements ILinkedDataFragmentWriter {
    private final Configuration cfg;
    
    private final Template indexTemplate;
    private final Template datasourceTemplate;
    private final Template notfoundTemplate;
    private final Template errorTemplate;
    
    private final String HYDRA = "http://www.w3.org/ns/hydra/core#"; 
    
    /**
     *
     * @param prefixes
     * @param datasources
     * @throws IOException
     */
    public HtmlTriplePatternFragmentWriterImpl(Map<String, String> prefixes, HashMap<String, IDataSource> datasources) throws IOException {
        super(prefixes, datasources);
        
        cfg = new Configuration(Configuration.VERSION_2_3_22);
        cfg.setClassForTemplateLoading(getClass(), "/views");
        cfg.setDefaultEncoding("UTF-8");
        cfg.setTemplateExceptionHandler(TemplateExceptionHandler.RETHROW_HANDLER);
        
        indexTemplate = cfg.getTemplate("index.ftl.html");
        datasourceTemplate = cfg.getTemplate("datasource.ftl.html");
        notfoundTemplate = cfg.getTemplate("notfound.ftl.html");
        errorTemplate = cfg.getTemplate("error.ftl.html");
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
    @Override
    public void writeFragment(JSONObject queryObject, ServletOutputStream outputStream, IDataSource datasource, ITriplePatternFragment fragment,  ITriplePatternFragmentRequest tpfRequest) throws IOException, TemplateException{
        Map data = new HashMap();
        
        // base.ftl.html
        data.put("assetsPath", "assets/");
        data.put("header", datasource.getTitle());
        data.put("date", new Date());
        
        // fragment.ftl.html
        data.put("datasourceUrl", tpfRequest.getDatasetURL());
        data.put("datasource", datasource);
        
        // Parse controls to template variables
        StmtIterator controls = fragment.getControls();
        while (controls.hasNext()) {
            Statement control = controls.next();
            
            String predicate = control.getPredicate().getURI();
            RDFNode object = control.getObject();
            if (!object.isAnon()) {
                String value = object.isURIResource() ? object.asResource().getURI() : object.asLiteral().getLexicalForm();
                data.put(predicate.replaceFirst(HYDRA, ""), value);
            }
        }
        
        // Add metadata
        data.put("totalEstimate", fragment.getTotalSize());
        data.put("itemsPerPage", fragment.getMaxPageSize());
        
        // Add triples and datasources
        List<Statement> triples = fragment.getTriples().toList();
        data.put("triples", triples);
        data.put("datasources", getDatasources());
        
        // Calculate start and end triple number
        Long start = ((tpfRequest.getPageNumber() - 1) * fragment.getMaxPageSize()) + 1;
        data.put("start", start);
        data.put("end", start + (triples.size() < fragment.getMaxPageSize() ? triples.size() : fragment.getMaxPageSize()));
        
        // Compose query object
        Map query = new HashMap();
        query.put("subject", !tpfRequest.getSubject().isVariable() ? tpfRequest.getSubject().asConstantTerm() : "");
        query.put("predicate", !tpfRequest.getPredicate().isVariable() ? tpfRequest.getPredicate().asConstantTerm() : "");
        query.put("object", !tpfRequest.getObject().isVariable() ? tpfRequest.getObject().asConstantTerm() : "");
        data.put("query", query);
       
        // Get the template (uses cache internally)
        Template temp = datasource instanceof IndexDataSource ? indexTemplate : datasourceTemplate;

        // Merge data-model with template
        temp.process(data, new OutputStreamWriter(outputStream));
    }

    @Override
    public void writeNotFound(ServletOutputStream outputStream, HttpServletRequest request) throws Exception {
        Map data = new HashMap();
        data.put("assetsPath", "assets/");
        data.put("datasources", getDatasources());
        data.put("date", new Date());
        data.put("url", request.getRequestURL().toString());
        
        notfoundTemplate.process(data, new OutputStreamWriter(outputStream));
    }

    @Override
    public void writeError(ServletOutputStream outputStream, Exception ex)  throws Exception {
        Map data = new HashMap();
        data.put("assetsPath", "assets/");
        data.put("date", new Date());
        data.put("error", ex);

        errorTemplate.process(data, new OutputStreamWriter(outputStream));
    }
}
