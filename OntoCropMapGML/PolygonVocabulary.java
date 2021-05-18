/**
 * Copyright (c) 2021 Computational Modelling Group
 * 
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, 
 * publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so, 
 * subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF 
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 * 
 * @author <a href="mailto:msff2@cam.ac.uk">Feroz Farazi</a>
 * @author <a href="mailto:arkadiusz.chadzynski@cares.cam.ac.uk">
 * Arkadiusz Chadzynski</a>
 * @author <a href="mailto:jwja2@cam.ac.uk">Jethro Akroyd</a>
 * @author <a href="mailto:sm453@cam.ac.uk">Sebastian Mosbach</a>
 * @author <a href="mailto:mk306@cam.ac.uk">Markus Kraft</a>
 * @since March 8, 2021
 * @version Version 1.0
 * 
 * This Java program creates a vocabulary and data type to
 * represent a hexagonal-shaped polygon's geometry in the
 * Blazegraph triple store when integrated into the Blazegraph
 * project provided at the following URL:
 *
 * <a href="#{@link}">{@link https://github.com/blazegraph/database}</a>.
 *
 * The program will enable users to perform geospatial queries
 * against the Blazegraph triple store to find points of interest
 * codified with geometry within a circle or bounding box by 
 * using the geospatial reasoning engine of Blazegraph.
 */

package com.bigdata.rdf.vocab;

import com.bigdata.rdf.store.AbstractTripleStore;
import com.bigdata.rdf.vocab.BaseVocabularyDecl;
import com.bigdata.rdf.vocab.Vocabulary;
import com.bigdata.rdf.vocab.core.BigdataCoreVocabulary_v20160317;
import org.openrdf.model.impl.URIImpl;

/**
* A {@link Vocabulary} covering the hexagonal shaped polygon data.
* Use the vocabulary by adding a property to the configuration 
* file (RWStore.properties) as shown below.
* <code>
* com.bigdata.rdf.store.AbstractTripleStore.vocabularyClass=
* com.bigdata.rdf.vocab.PolygonVocabulary
* </code>
*/
public class PolygonVocabulary extends BigdataCoreVocabulary_v20160317
{
    /**
     * Defines the URI of the POLYGON-2-14 data type. 
     *
     */
    private String[] uris = 
        {"http://theworldavatar.com/ontology/datatype/POLYGON-2-14"};

    /**
     * Default constructor.
     */
    public PolygonVocabulary() {
        super();
    }

    /**
     * Used by {@link AbstractTripleStore#create()}.
     *
     * @param namespace
     *            The namespace of the KB instance.
     */
    public PolygonVocabulary(final String namespace) {
        super( namespace );
    }
    
    @Override
    protected void addValues() {
        super.addValues();
        for (String uri: uris) {
            URIImpl impl = new URIImpl(uri);
            BaseVocabularyDecl decl = new BaseVocabularyDecl(impl);
            addDecl(decl);
        }
      }
}