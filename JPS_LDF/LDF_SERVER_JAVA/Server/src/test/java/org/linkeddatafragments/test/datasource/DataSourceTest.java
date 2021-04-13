package org.linkeddatafragments.test.datasource;

import com.google.gson.JsonObject;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import java.nio.file.Files;
import java.nio.file.StandardCopyOption;

import org.junit.Assert;
import org.junit.Test;

import org.linkeddatafragments.datasource.IDataSource;
import org.linkeddatafragments.datasource.IFragmentRequestProcessor;
import org.linkeddatafragments.fragments.ILinkedDataFragment;
import org.linkeddatafragments.fragments.tpf.ITriplePatternElement;
import org.linkeddatafragments.fragments.tpf.ITriplePatternFragment;
import org.linkeddatafragments.fragments.tpf.ITriplePatternFragmentRequest;
import org.linkeddatafragments.fragments.tpf.TriplePatternFragmentRequestImpl;
import org.linkeddatafragments.util.TriplePatternElementParser;


/**
 *
 * @author <a href="mailto:bart.hanssens@fedict.be">Bart Hanssens</a>
 * @param <ConstantTermType>
 * @param <NamedVarType>
 * @param <AnonVarType>
 */
public abstract class DataSourceTest<ConstantTermType,NamedVarType,AnonVarType>
{
    private static IDataSource ds;

    /**
     * Get data source
     *
     * @return data source interface
     */
    public static IDataSource getDatasource() {
        return ds;
    }

    /**
     * Set the data source
     *
     * @param ds data source
     */
    public static void setDatasource(IDataSource ds) {
        DataSourceTest.ds = ds;
    }

    /**
     *
     * @return
     */
    protected abstract TriplePatternElementParser<ConstantTermType,NamedVarType,AnonVarType>
                                               getTriplePatternElementParser();

    /**
     * Copy the demo triple in the jar to a temp file.
     *
     * @return temp file
     * @throws IOException
     */
    public static File getResourceAsFile() throws IOException {
        File temp = File.createTempFile("ldf-test-hdt", ".ttl");
        temp.deleteOnExit();

        InputStream in = ClassLoader.getSystemResourceAsStream("demo.nt");
        Files.copy(in, temp.toPath(), StandardCopyOption.REPLACE_EXISTING);

        return temp;
    }

    /**
     * Generate a basic Json configuration
     *
     * @param title
     * @param desc
     * @param type
     * @return JSON object
     */
    public static JsonObject createConfig(String title, String desc, String type) {
        JsonObject config = new JsonObject();
        config.addProperty("title", title);
        config.addProperty("description", desc);
        config.addProperty("type", type);

        return config;
    }


    /**
     * Test total size of empty TPF
     *
     */
    @Test
    public void testEmpty() {
        final TriplePatternElementParser<ConstantTermType,NamedVarType,AnonVarType> tpeParser =
                                               getTriplePatternElementParser();

        final ITriplePatternFragmentRequest<ConstantTermType,NamedVarType,AnonVarType> request =
                new TriplePatternFragmentRequestImpl<ConstantTermType,NamedVarType,AnonVarType>(
                        "http://example.org/f", // fragmentURL
                        "http://example.org/",  // datasetURL,
                        true, // pageNumberWasRequested,
                        1L, //pageNumber,
                        tpeParser.parseIntoTriplePatternElement("http://nothing.ldf.org"), // subject,
                        tpeParser.parseIntoTriplePatternElement(null), // predicate,
                        tpeParser.parseIntoTriplePatternElement(null) ); //object

        final IFragmentRequestProcessor proc = getDatasource().getRequestProcessor();
        final ILinkedDataFragment ldf = proc.createRequestedFragment( request );
        final ITriplePatternFragment tpf = (ITriplePatternFragment) ldf;

        long totalSize = tpf.getTotalSize();

        Assert.assertTrue("Estimate is too big : " + totalSize, totalSize == 0);

    }

    /**
     * Test if estimate seems reasonable.
    
    @Test
    public void testEstimate() {
        final TriplePatternElementParser<ConstantTermType,NamedVarType,AnonVarType> tpeParser =
                                               getTriplePatternElementParser();

        final ITriplePatternFragmentRequest<ConstantTermType,NamedVarType,AnonVarType> request =
          new ITriplePatternFragmentRequest<ConstantTermType,NamedVarType,AnonVarType>() {
            public boolean isPageRequest() { return true; }
            public long getPageNumber() { return 1L; }
            public String getFragmentURL() { return "http://example.org/f"; }
            public String getDatasetURL() { return "http://example.org/"; }

            public ITriplePatternElement<ConstantTermType,NamedVarType,AnonVarType> getSubject() {
                return tpeParser.parseIntoTriplePatternElement("http://data.gov.be/catalog/ckanvl");
            }
            public ITriplePatternElement<ConstantTermType,NamedVarType,AnonVarType> getPredicate() {
                return tpeParser.parseIntoTriplePatternElement(null);
            }
            public ITriplePatternElement<ConstantTermType,NamedVarType,AnonVarType> getObject() {
                return tpeParser.parseIntoTriplePatternElement(null);
            }
        };

        final IFragmentRequestProcessor proc = getDatasource().getRequestProcessor();
        final ILinkedDataFragment ldf = proc.createRequestedFragment( request );
        final ITriplePatternFragment tpf = (ITriplePatternFragment) ldf;

        long totalSize = tpf.getTotalSize();

        Assert.assertTrue("Estimate is too small : " + totalSize, totalSize > 100);
    } */
}
