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
	

    private final static long serialVersionUID = 1L;
    private final static String handshake = "{\n"
    		+ "  \"@graph\" : [ {\n"
    		+ "    \"@id\" : \"_:b0\",\n"
    		+ "    \"@type\" : \"http://www.w3.org/2002/07/owl#Restriction\"\n"
    		+ "  }, {\n"
    		+ "    \"@id\" : \"_:b1\",\n"
    		+ "    \"property\" : \"rdf:subject\",\n"
    		+ "    \"variable\" : \"subject\"\n"
    		+ "  }, {\n"
    		+ "    \"@id\" : \"_:b2\",\n"
    		+ "    \"first\" : \"http://www.theworldavatar.com/kb/ontokin/ontokin.owl#Product\",\n"
    		+ "    \"rest\" : {\n"
    		+ "      \"@list\" : [ \"http://www.theworldavatar.com/kb/ontokin/ontokin.owl#Reactant\" ]\n"
    		+ "    }\n"
    		+ "  }, {\n"
    		+ "    \"@id\" : \"_:b4\",\n"
    		+ "    \"property\" : \"rdf:predicate\",\n"
    		+ "    \"variable\" : \"predicate\"\n"
    		+ "  }, \n"
    		+ "  {\n"
    		+ "    \"@id\" : \"_:b10\",\n"
    		+ "    \"property\" : \"rdf:product\",\n"
    		+ "    \"variable\" : \"product\"\n"
    		+ "  },\n"
    		+ "  {\n"
    		+ "    \"@id\" : \"_:b5\",\n"
    		+ "    \"@type\" : \"http://www.w3.org/2002/07/owl#Restriction\",\n"
    		+ "    \"allValuesFrom\" : \"http://www.theworldavatar.com/kb/ontokin/ontokin.owl#Element\",\n"
    		+ "    \"onProperty\" : \"http://www.theworldavatar.com/kb/ontokin/ontokin.owl#indicatesNumberOf\"\n"
    		+ "  }, {\n"
    		+ "    \"@id\" : \"_:b6\",\n"
    		+ "    \"first\" : \"http://www.theworldavatar.com/kb/ontokin/ontokin.owl#FallOffModelCoefficient\",\n"
    		+ "    \"rest\" : \"_:b7\"\n"
    		+ "  }, {\n"
    		+ "    \"@id\" : \"_:b8\",\n"
    		+ "    \"mapping\" : [ \"_:b1\", \"_:b4\", \"_:b9\"],\n"
    		+ "    \"template\" : \"http://localhost:8080/ldfserver/ontokin{?subject,predicate,object}\"\n"
    		+ "  }, {\n"
    		+ "    \"@id\" : \"_:b9\",\n"
    		+ "    \"property\" : \"rdf:object\",\n"
    		+ "    \"variable\" : \"object\"\n"
    		+ "  }, {\n"
    		+ "    \"@id\" : \"http://localhost:8080/ldfserver/ontokin\",\n"
    		+ "    \"@type\" : [ \"hydra:Collection\", \"hydra:PagedCollection\" ],\n"
    		+ "    \"void:triples\" : 22262124,\n"
    		+ "    \"firstPage\" : \"http://localhost:8080/ldfserver/ontokin?page=1\",\n"
    		+ "    \"hydra:itemsPerPage\" : 10000,\n"
    		+ "    \"nextPage\" : \"http://localhost:8080/ldfserver/ontokin?page=2\",\n"
    		+ "    \"hydra:totalItems\" : 22262124\n"
    		+ "  }, {\n"
    		+ "    \"@id\" : \"http://localhost:8080/ldfserver/ontokin#dataset\",\n"
    		+ "    \"@type\" : [ \"void:Dataset\", \"hydra:Collection\" ],\n"
    		+ "    \"subset\" : \"http://localhost:8080/ldfserver/ontokin\",\n"
    		+ "    \"hydra:itemsPerPage\" : {\n"
    		+ "      \"@type\" : \"xsd:long\",\n"
    		+ "      \"@value\" : \"10000\"\n"
    		+ "    },\n"
    		+ "    \"search\" : \"_:b8\"\n"
    		+ "  } ],\n"
    		+ "  \"@context\" : {\n"
    		+ "    \"firstPage\" : {\n"
    		+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#firstPage\",\n"
    		+ "      \"@type\" : \"@id\"\n"
    		+ "    },\n"
    		+ "    \"nextPage\" : {\n"
    		+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#nextPage\",\n"
    		+ "      \"@type\" : \"@id\"\n"
    		+ "    },\n"
    		+ "    \"triples\" : {\n"
    		+ "      \"@id\" : \"http://rdfs.org/ns/void#triples\",\n"
    		+ "      \"@type\" : \"http://www.w3.org/2001/XMLSchema#integer\"\n"
    		+ "    },\n"
    		+ "    \"totalItems\" : {\n"
    		+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#totalItems\",\n"
    		+ "      \"@type\" : \"http://www.w3.org/2001/XMLSchema#integer\"\n"
    		+ "    },\n"
    		+ "    \"itemsPerPage\" : {\n"
    		+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#itemsPerPage\",\n"
    		+ "      \"@type\" : \"http://www.w3.org/2001/XMLSchema#integer\"\n"
    		+ "    },\n"
    		+ "    \"variable\" : {\n"
    		+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#variable\"\n"
    		+ "    },\n"
    		+ "    \"property\" : {\n"
    		+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#property\",\n"
    		+ "      \"@type\" : \"@id\"\n"
    		+ "    },\n"
    		+ "    \"first\" : {\n"
    		+ "      \"@id\" : \"http://www.w3.org/1999/02/22-rdf-syntax-ns#first\",\n"
    		+ "      \"@type\" : \"@id\"\n"
    		+ "    },\n"
    		+ "    \"rest\" : {\n"
    		+ "      \"@id\" : \"http://www.w3.org/1999/02/22-rdf-syntax-ns#rest\",\n"
    		+ "      \"@type\" : \"@id\"\n"
    		+ "    },\n"
    		+ "    \"allValuesFrom\" : {\n"
    		+ "      \"@id\" : \"http://www.w3.org/2002/07/owl#allValuesFrom\",\n"
    		+ "      \"@type\" : \"@id\"\n"
    		+ "    },\n"
    		+ "    \"onProperty\" : {\n"
    		+ "      \"@id\" : \"http://www.w3.org/2002/07/owl#onProperty\",\n"
    		+ "      \"@type\" : \"@id\"\n"
    		+ "    },\n"
    		+ "    \"search\" : {\n"
    		+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#search\",\n"
    		+ "      \"@type\" : \"@id\"\n"
    		+ "    },\n"
    		+ "    \"subset\" : {\n"
    		+ "      \"@id\" : \"http://rdfs.org/ns/void#subset\",\n"
    		+ "      \"@type\" : \"@id\"\n"
    		+ "    },\n"
    		+ "    \"template\" : {\n"
    		+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#template\"\n"
    		+ "    },\n"
    		+ "    \"mapping\" : {\n"
    		+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#mapping\",\n"
    		+ "      \"@type\" : \"@id\"\n"
    		+ "    },\n"
    		+ "    \"hydra\" : \"http://www.w3.org/ns/hydra/core#\",\n"
    		+ "    \"dbpedia-owl\" : \"http://dbpedia.org/ontology/\",\n"
    		+ "    \"void\" : \"http://rdfs.org/ns/void#\",\n"
    		+ "    \"rdf\" : \"http://www.w3.org/1999/02/22-rdf-syntax-ns#\",\n"
    		+ "    \"xsd\" : \"http://www.w3.org/2001/XMLSchema#\",\n"
    		+ "    \"rdfs\" : \"http://www.w3.org/2000/01/rdf-schema#\",\n"
    		+ "    \"dbpedia\" : \"http://dbpedia.org/resource/\",\n"
    		+ "    \"dbpprop\" : \"http://dbpedia.org/property/\",\n"
    		+ "    \"foaf\" : \"http://xmlns.com/foaf/0.1/\",\n"
    		+ "    \"dc\" : \"http://purl.org/dc/terms/\"\n"
    		+ "  }\n"
    		+ "}";
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
        System.out.println("Getting config file: " + path);
        if (path == null) {
            // this can happen when running standalone
            path = System.getProperty("user.dir");
        }
        File cfg = new File(path, "config.json");
        
        System.out.println("=========== the cfg file =============");
        System.out.println(cfg.getPath());
        
        if (config.getInitParameter(CFGFILE) != null) {
            cfg = new File(config.getInitParameter(CFGFILE));
        }
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
        //System.out.println("================================ A request is received ==============================");
        String queryString = request.getQueryString();
        //System.out.println(queryString);

        
        // System.out.println("=======================================================================================");
        
        try {
            // do conneg
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
                
                
                // if the queryString has no subject/predicate/object 
                
                if(queryString!= null) {
                	boolean isEquationQuery = QueryProcessor.isEquationQuery(request);
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
                         	String composed = SPARQLRegex.filter(page, modelAsString, reactants, products);
                           	 
                        	try (Writer w = new OutputStreamWriter(response.getOutputStream(), "UTF-8")) {
                        		w.write(composed);
                            }  
                        }else 
                        
                        {
                           	System.out.println("key doesn't exist, ");
                            writer.writeFragment(QueryProcessor.separateParameters(request), response.getOutputStream(), dataSource, fragment, ldfRequest);                	
                        }
                        
                	}else {
                		
                		System.out.println("This is a non equation query:\n" + queryString);
                		System.out.println();
                		
                        writer.writeFragment(null, response.getOutputStream(), dataSource, fragment, ldfRequest);                	
                	}
                	
                }
                else {
                 	queryString = "handshake";     	
                	try (Writer w = new OutputStreamWriter(response.getOutputStream(), "UTF-8")) {
                    w.write(handshake);
                	
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
