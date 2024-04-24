package org.linkeddatafragments.test.datasource;

import com.google.gson.JsonObject;

import org.apache.jena.query.Dataset;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.tdb.TDBFactory;

import java.io.File;
import java.io.InputStream;
import org.apache.jena.rdf.model.RDFNode;

import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;

import org.junit.After;
import org.junit.AfterClass;

import org.junit.Before;
import org.junit.BeforeClass;

import org.linkeddatafragments.datasource.DataSourceFactory;
import org.linkeddatafragments.datasource.DataSourceTypesRegistry;
import org.linkeddatafragments.datasource.tdb.JenaTDBDataSourceType;
import org.linkeddatafragments.util.TriplePatternElementParser;
import org.linkeddatafragments.util.TriplePatternElementParserForJena;

/**
 *
 * @author <a href="mailto:bart.hanssens@fedict.be">Bart Hanssens</a>
 */
public class JenaTDBDataSourceTest extends DataSourceTest<RDFNode,String,String>
{
    private static File jena;
    private static Dataset dataset;

    /**
     *
     * @return
     */
    @Override
    protected TriplePatternElementParser<RDFNode,String,String>
                                               getTriplePatternElementParser()
    {
        return TriplePatternElementParserForJena.getInstance();
    }
            
    /**
     *
     * @throws Exception
     */
    @BeforeClass
    public static void setUpClass() throws Exception {
        final String typeName = "JenaSourceType";
        if ( ! DataSourceTypesRegistry.isRegistered(typeName) ) {
            DataSourceTypesRegistry.register( typeName,
                                              new JenaTDBDataSourceType() );
        }

        String tmpdir = System.getProperty("java.io.tmpdir");
        jena = new File(tmpdir, "ldf-jena-test");
        jena.mkdir();
        
        dataset = TDBFactory.createDataset(jena.getAbsolutePath());

        Model model = dataset.getDefaultModel();
        InputStream in = ClassLoader.getSystemResourceAsStream("demo.nt");
        RDFDataMgr.read(model, in, Lang.NTRIPLES);

        // Everything is in place, now create the LDF datasource                
        JsonObject config = createConfig("jena tdb test", "jena tdb test",
                                         typeName);
        
        JsonObject settings = new JsonObject();
        settings.addProperty("directory", jena.getAbsolutePath());
        config.add("settings", settings);

        setDatasource(DataSourceFactory.create(config));
    }

    /**
     *
     * @throws Exception
     */
    @AfterClass
    public static void tearDownClass() throws Exception {
        TDBFactory.release(dataset);
        File[] files = jena.listFiles();
        for (File f : files) {
            f.delete();
        }
        jena.delete();
 
    }

    /**
     *
     * @throws Exception
     */
    @Before
    public void setUp() throws Exception {                
    }
    
    /**
     *
     * @throws Exception
     */
    @After
    public void tearDown() throws Exception {
    }
}
