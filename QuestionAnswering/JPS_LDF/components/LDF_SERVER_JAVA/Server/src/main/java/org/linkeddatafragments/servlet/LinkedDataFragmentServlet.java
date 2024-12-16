package org.linkeddatafragments.servlet;

import com.google.gson.JsonObject;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map.Entry;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.http.HttpHeaders;
import org.apache.jena.query.ARQ;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.riot.Lang;
import org.apache.jena.sys.JenaSystem;
import org.json.JSONArray;
import org.json.JSONObject;
import org.linkeddatafragments.config.ConfigReader;
import org.linkeddatafragments.datasource.DataSourceFactory;
import org.linkeddatafragments.datasource.DataSourceTypesRegistry;
import org.linkeddatafragments.datasource.IDataSource;
import org.linkeddatafragments.datasource.IDataSourceType;
import org.linkeddatafragments.datasource.index.IndexDataSource;
import org.linkeddatafragments.exceptions.DataSourceNotFoundException;
import org.linkeddatafragments.fragments.FragmentRequestParserBase;
import org.linkeddatafragments.fragments.ILinkedDataFragment;
import org.linkeddatafragments.fragments.ILinkedDataFragmentRequest;
import org.linkeddatafragments.fragments.SPARQLRegex;
import org.linkeddatafragments.util.MIMEParse;
import org.linkeddatafragments.util.ResponseTemplates;
import org.linkeddatafragments.views.ILinkedDataFragmentWriter;
import org.linkeddatafragments.views.LinkedDataFragmentWriterFactory;

/**
 * Servlet that responds with a Linked Data Fragment.
 *
 * @author Ruben Verborgh
 * @author Bart Hanssens
 * @author <a href="http://olafhartig.de">Olaf Hartig</a>
 */
public class LinkedDataFragmentServlet extends HttpServlet {
	
	// TODO: Separate the handshakes and templates of responses
    private final static long serialVersionUID = 1L;
     // Parameters

    /**
     *
     */
    // private final CachingInterfaceJedis cacheInterface = new CachingInterfaceJedis();
    public final static String CFGFILE = "configFile";

    private ConfigReader config;
    private final HashMap<String, IDataSource> dataSources = new HashMap<>();
    private final Collection<String> mimeTypes = new ArrayList<>();

    private File getConfigFile(ServletConfig config) throws IOException {
        String path = config.getServletContext().getRealPath("/");
        File cfg = new File(path, "config.json");
        System.out.println("Getting config file: " + cfg.getAbsolutePath());

        if (!cfg.exists()) {
            throw new IOException("Configuration file " + cfg + " not found.");
        }
        if (!cfg.isFile()) {
            throw new IOException("Configuration file " + cfg + " is not a file.");
        }
        return cfg;
    }

    /**
     *
     * @param servletConfig
     * @throws ServletException
     */
    @Override
    public void init(ServletConfig servletConfig) throws ServletException {
        // Ensure ARQ has been initialized (needed for TDB requests)
        ARQ.init();
               
        try {
            // load the configuration
            System.out.println("Working Directory = " + System.getProperty("user.dir"));

        	
            File configFile = getConfigFile(servletConfig);
            
            String p = configFile.getAbsolutePath();
            System.out.println("file is at:" + p);
            config = new ConfigReader(new FileReader(configFile));
              
            // register data source types
            for ( Entry<String,IDataSourceType> typeEntry : config.getDataSourceTypes().entrySet() ) {
                DataSourceTypesRegistry.register( typeEntry.getKey(),
                                                  typeEntry.getValue() );
            }

            // register data sources
            for (Entry<String, JsonObject> dataSource : config.getDataSources().entrySet()) {
            	 
                dataSources.put(dataSource.getKey(), DataSourceFactory.create(dataSource.getValue()));
            }

            // register content types
            MIMEParse.register("text/html");
            MIMEParse.register(Lang.RDFXML.getHeaderString());
            MIMEParse.register(Lang.NTRIPLES.getHeaderString());
            MIMEParse.register(Lang.JSONLD.getHeaderString());
            MIMEParse.register(Lang.TTL.getHeaderString());
        } catch (Exception e) {
            throw new ServletException(e);
        }
    }

    /**
     *
     */
    @Override
    public void destroy()
    {
        for ( IDataSource dataSource : dataSources.values() ) {
            try {
                dataSource.close();
            }
            catch( Exception e ) {
                // ignore
            }
        }   
    }

    /**
     * Get the datasource
     *
     * @param request
     * @return
     * @throws IOException
     */
    private IDataSource getDataSource(HttpServletRequest request) throws DataSourceNotFoundException {
        String contextPath = request.getContextPath();
        String requestURI = request.getRequestURI();

        String path = contextPath == null
                ? requestURI
                : requestURI.substring(contextPath.length());

        if (path.equals("/") || path.isEmpty()) {
            final String baseURL = FragmentRequestParserBase.extractBaseURL(request, config);
            return new IndexDataSource(baseURL, dataSources);
        }

        String dataSourceName = path.substring(1);
        IDataSource dataSource = dataSources.get(dataSourceName);
        if (dataSource == null) {
            throw new DataSourceNotFoundException(dataSourceName);
        }
        return dataSource;
    }
    
 

    /**
     *
     * @param request
     * @param response
     * @throws ServletException
     */
    @Override
    public void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException {
        ILinkedDataFragment fragment = null;
        CachingInterfaceJedis cacheInterface = new CachingInterfaceJedis();
        String queryString = request.getQueryString();
        String ontology = request.getPathInfo().replace("/","");
 
               
        try {
            String acceptHeader = request.getHeader(HttpHeaders.ACCEPT);
            String bestMatch = MIMEParse.bestMatch(acceptHeader);

            // set additional response headers
            response.setHeader(HttpHeaders.SERVER, "Linked Data Fragments Server");
            response.setContentType(bestMatch);
            response.setCharacterEncoding(StandardCharsets.UTF_8.name());

            // create a writer depending on the best matching mimeType
            ILinkedDataFragmentWriter writer = LinkedDataFragmentWriterFactory.create(config.getPrefixes(), dataSources, bestMatch);
            
            try {
            
                final IDataSource dataSource = getDataSource( request );
                final ILinkedDataFragmentRequest ldfRequest =
                        dataSource.getRequestParser()
                                  .parseIntoFragmentRequest( request, config );

                fragment = dataSource.getRequestProcessor()
                                  .createRequestedFragment( ldfRequest );
                
 
                if (ontology.contentEquals("ontocompchem")) {
                	System.out.println("======= queryString ==========");

                	System.out.println(queryString);
                	if(queryString!= null) {
                	
                    if (cacheInterface.exist(queryString)) {
                       	String modelAsString = cacheInterface.getValue(queryString); // a cached result is returned. this is the usual case 
                    	try (Writer w = new OutputStreamWriter(response.getOutputStream(), "UTF-8")) {
                    		w.write(modelAsString);
                        }  
                    }
                    else {
                        writer.writeFragment(null, response.getOutputStream(), dataSource, fragment, ldfRequest);                	
                    }
                	}
                	else {
                    	try (Writer w = new OutputStreamWriter(response.getOutputStream(), "UTF-8")) {
                            	w.write(ResponseTemplates.OntoCompChemHandshake);
                        	}
                	}
                }
                
                
                
                else
                {
 
                if(queryString!= null) {
                	boolean isEquationQuery = QueryProcessor.isOntokinEquationQuery(request);
                	System.out.println("isEquationQuery: " + isEquationQuery);
                	if(isEquationQuery) { // queryString is "equationQueryKey"
                    	JSONObject queryObject = QueryProcessor.separateParameters(request);
                		String queryStringKey = queryObject.getString("hasEquationQuery");

                		System.out.println("cache key:" + queryStringKey);
                        if (cacheInterface.exist(queryStringKey)) {   
                        	
                           	String modelAsString = cacheInterface.getValue(queryStringKey); // a cached result is returned. this is the usual case 
                           	// filter the results with the given parameters. 
                           	
                           	System.out.println("key exist, going to the cache ");
                         	String page = "1";
                         	if(queryObject.has("page")) {
                         		page = queryObject.getString("page");
                         	}
                         	
                         	JSONArray reactants = queryObject.getJSONArray("reactants");
                         	JSONArray products = queryObject.getJSONArray("products");
                         	String composed = SPARQLRegex.filter(ontology, page, modelAsString, reactants, products);
                           	 
                        	try (Writer w = new OutputStreamWriter(response.getOutputStream(), "UTF-8")) {
                        		w.write(composed);
                            }  
                        }else 
                        
                        {
                           	System.out.println("key doesn't exist, ");
                   	  
                            writer.writeFragment(QueryProcessor.separateParameters(request), response.getOutputStream(), dataSource, fragment, ldfRequest);                	
                        }
                        
                	}else {
                	    
                        writer.writeFragment(null, response.getOutputStream(), dataSource, fragment, ldfRequest);                	
                	}
                	
                }
                else {
                	try (Writer w = new OutputStreamWriter(response.getOutputStream(), "UTF-8")) {
                    w.write(ResponseTemplates.OntokinHandshake);
                	}
                }
                
            }
             

 
        
            
            } catch (DataSourceNotFoundException ex) {
                try {
                    response.setStatus(404);
                    writer.writeNotFound(response.getOutputStream(), request);
                } catch (Exception ex1) {
                    throw new ServletException(ex1);
                }
            } catch (Exception e) {
                e.printStackTrace();
                response.setStatus(500);
                writer.writeError(response.getOutputStream(), e);
            }
          
        } catch (Exception e) {
            e.printStackTrace();
            throw new ServletException(e);
        }
        finally {
            // close the fragment
            if ( fragment != null ) {
                try {
                    fragment.close();
                }
                catch ( Exception e ) {
                    // ignore
                }
            }
        }
    }

}



//
//
//if (queryString!=null) {
//	boolean isEquationQuery = QueryProcessor.isOntoCompChemSpeciesQuery(request);
//	System.out.println("actual query for ontocompchem:" +queryString);
//	System.out.println("isEquationQuery:" + isEquationQuery);
//	 
//	
//	if (isEquationQuery) {
//    	JSONObject queryObject = QueryProcessor.separateParameters(request);
//		String queryStringKey = queryObject.getString("hasEquationQuery");
//		
//        if (cacheInterface.exist(queryStringKey)) {   
//        	
//           	String modelAsString = cacheInterface.getValue(queryStringKey); // a cached result is returned. this is the usual case 
//           	// filter the results with the given parameters. 
//           	
//           	System.out.println("key exist, going to the cache ");
//         	String page = "1";
//         	if(queryObject.has("page")) {
//         		page = queryObject.getString("page");
//         	}
//         	
//         	JSONArray reactants = queryObject.getJSONArray("reactants");
//         	JSONArray products = queryObject.getJSONArray("products");
//         	String composed = SPARQLRegex.filter(ontology, page, modelAsString, reactants, products);
//           	 
//        	try (Writer w = new OutputStreamWriter(response.getOutputStream(), "UTF-8")) {
//        		w.write(composed);
//            }  
//        }else 
//        
//        {
//           	System.out.println("key doesn't exist, ");
//            writer.writeFragment(null, response.getOutputStream(), dataSource, fragment, ldfRequest);                	
//        }
//
//
//	}
//	else {
//  
//        writer.writeFragment(null, response.getOutputStream(), dataSource, fragment, ldfRequest);   
//	}
//
//}
//else {
//	try (Writer w = new OutputStreamWriter(response.getOutputStream(), "UTF-8")) {
//		System.out.println("returning handshake");
//        w.write(ResponseTemplates.OntoCompChemHandshake);
//    }
//}