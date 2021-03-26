package com.bigdata.rdf.vocab;

import com.bigdata.rdf.store.AbstractTripleStore;
import com.bigdata.rdf.vocab.BaseVocabularyDecl;
import com.bigdata.rdf.vocab.Vocabulary;
import com.bigdata.rdf.vocab.core.BigdataCoreVocabulary_v20160317;
import org.openrdf.model.impl.URIImpl;

/**
* 
* A {@link Vocabulary} covering the polygon data.
* Use the vocabulary by adding a property to the configuration file (RWStore.properties) per below.
* 
* <code>
* 
 com.bigdata.rdf.store.AbstractTripleStore.vocabularyClass=
 com.bigdata.rdf.vocab.PolygonVocabulary
* </code>
*
* @author <a href="mailto:msff2@cam.ac.uk">Feroz Farazi</a>
* @version $Id$
* 
*/
public class PolygonVocabulary extends BigdataCoreVocabulary_v20160317 {

private String[] uris = {"http://theworldavatar.com/ontology/datatype/POLYGON-2-14", "http://my-lat-lon-starttime-endtime-dt"};

/**
 * De-serialization ctor.
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