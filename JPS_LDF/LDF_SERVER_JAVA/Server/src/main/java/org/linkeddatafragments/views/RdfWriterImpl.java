package org.linkeddatafragments.views;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.util.HashMap;
import java.util.Map;
import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletRequest;

import org.apache.jena.query.ARQ;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RDFLanguages;
import org.json.JSONArray;
import org.json.JSONObject;
import org.linkeddatafragments.datasource.IDataSource;
import org.linkeddatafragments.fragments.ILinkedDataFragment;
import org.linkeddatafragments.fragments.ILinkedDataFragmentRequest;
import org.linkeddatafragments.fragments.SPARQLRegex;
import org.linkeddatafragments.servlet.CachingInterfaceJedis;
import com.fulmicoton.multiregexp.MultiPatternMatcher;
import com.fulmicoton.multiregexp.MultiPattern;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 *  Serializes an {@link ILinkedDataFragment} to an RDF format
 * 
 * @author Miel Vander Sande
 */
class RdfWriterImpl extends LinkedDataFragmentWriterBase implements ILinkedDataFragmentWriter {

    private final Lang contentType;
	private CachingInterfaceJedis cacheInterface;

    public RdfWriterImpl(Map<String, String> prefixes, HashMap<String, IDataSource> datasources, String mimeType) {
        super(prefixes, datasources);
        this.contentType = RDFLanguages.contentTypeToLang(mimeType);
        this.cacheInterface = new CachingInterfaceJedis();

        
        ARQ.init();
    }

    @Override
    public void writeNotFound(ServletOutputStream outputStream, HttpServletRequest request) throws IOException {
        outputStream.println(request.getRequestURL().toString() + " not found!");
        outputStream.close();
    }

    @Override
    public void writeError(ServletOutputStream outputStream, Exception ex) throws IOException {
        outputStream.println(ex.getMessage());
        outputStream.close();
    }

    @Override
    public void writeFragment(JSONObject queryObject, ServletOutputStream outputStream, IDataSource datasource, ILinkedDataFragment fragment, ILinkedDataFragmentRequest ldfRequest) throws Exception {
        final Model output = ModelFactory.createDefaultModel();
        output.setNsPrefixes(getPrefixes());
        output.add(fragment.getMetadata());
        output.add(fragment.getTriples());
        output.add(fragment.getControls());
        
        StringWriter outAsString = new StringWriter();
        RDFDataMgr.write(outAsString, output, Lang.JSONLD);
     	String modelAsString = outAsString.toString(); 
     	
     	if (queryObject == null) {
            try (Writer w = new OutputStreamWriter(outputStream, "UTF-8")) {
                w.write(modelAsString);
                String filename =String.valueOf(modelAsString.hashCode());
                FileWriter myWriter = new FileWriter("D://ont//" + filename);
                myWriter.write(modelAsString);
                myWriter.close();
                
            }  
     	}else {
     	
     	
     	String page = "1";
     	if(queryObject.has("page")) {
     		page = queryObject.getString("page");

     	}
     	
     	JSONArray reactants = queryObject.getJSONArray("reactants");
     	JSONArray products = queryObject.getJSONArray("products");
     	String ontology = queryObject.getString("ontology");
     	
     	String composed = SPARQLRegex.filter(ontology, page, modelAsString, reactants, products);
     	
 
     	String queryString = queryObject.getString("hasEquationQuery");
     	System.out.println("The cache key is :\n" + queryString);
 
        
		if (!this.cacheInterface.exist(queryString)) {// it is in the redis database, do nothing 
        	System.out.println("========= writing to the cache =========");
			this.cacheInterface.setValue(queryString, modelAsString);
		}
//		
//		// ================= all regex should happen after the caching =================
//        
//		
//        if (queryString == "handShake") {
//        	System.out.println(modelAsString);
//        }
//		
		 
        try (Writer w = new OutputStreamWriter(outputStream, "UTF-8")) {
        	 
            w.write(composed);
        }  
        
        // RDFDataMgr.write(outputStream, output, Lang.JSONLD);
        
    }
    }

}
