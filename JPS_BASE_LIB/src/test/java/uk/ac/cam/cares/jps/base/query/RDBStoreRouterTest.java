package uk.ac.cam.cares.jps.base.query;

import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Field;

import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import uk.ac.cam.cares.jps.base.interfaces.CacheInterface;
import uk.ac.cam.cares.jps.base.interfaces.TripleStoreClientInterface;

public class RDBStoreRouterTest {

    static final String TEST_LABEL = "test1";
    static final String TEST_URL = "jdbc:postgresql://localhost:5432/";

    @Test
    void testCacheInitialisation() throws NoSuchFieldException, IllegalAccessException {

        RDBStoreRouter rdbStoreRouter = RDBStoreRouter.getInstance();

        assertNotNull(rdbStoreRouter.getClass().getSuperclass().getDeclaredField("cache"));
        Field cacheField = rdbStoreRouter.getClass().getSuperclass().getDeclaredField("cache");;
        cacheField.setAccessible(true);
        @SuppressWarnings("unchecked")
        CacheInterface<String, String> cache = (CacheInterface<String, String>) cacheField.get(rdbStoreRouter);
        assertNotNull(cache);
        assertTrue(cache.capacity()>0);
    }

    @Test
    void testEndpointInitialisation() {
        assertNotNull(RDBStoreRouter.RDBStoreRouterEndpoint);
    }

    @Test
    void testGetFromStore() {

        TripleStoreClientInterface mockStore = new mockOntordbrouter().create();

        String result = RDBStoreRouter.getInstance().getFromStore(TEST_LABEL,mockStore);
        assertEquals(TEST_URL+TEST_LABEL, result);
    }

    @Test
    void testGetFromStoreUnknownLabel() {

        TripleStoreClientInterface mockStore = new mockOntordbrouter().create();

        String result = RDBStoreRouter.getInstance().getFromStore("unknown",mockStore);
        assertNull(result);
    }

    @Test
    void testGet() {

        String expected = TEST_URL+TEST_LABEL;

        TripleStoreClientInterface mockStore = new mockOntordbrouter().create();

        RDBStoreRouter rdbStoreRouter = RDBStoreRouter.getInstance();

        //Use mocked StoreClient as triple store
        RDBStoreRouter spyRDBStoreRouter = Mockito.spy(rdbStoreRouter);
        Mockito.doReturn(mockStore).when(spyRDBStoreRouter).getRouterStoreClient();

        //Not in cache
        assertEquals(expected, spyRDBStoreRouter.get(TEST_LABEL));
        Mockito.verify(spyRDBStoreRouter, Mockito.times(1)).getRouterStoreClient();
        Mockito.verify(spyRDBStoreRouter, Mockito.times(1)).getFromStore(TEST_LABEL, mockStore);

        //In cache so getFromStore and getRouterStoreClient are not called again
        assertEquals(expected, spyRDBStoreRouter.get(TEST_LABEL));
        Mockito.verify(spyRDBStoreRouter, Mockito.times(1)).getRouterStoreClient();
        Mockito.verify(spyRDBStoreRouter, Mockito.times(1)).getFromStore(TEST_LABEL, mockStore);

        //Label does not exist
        String label = "unknown";
        assertNull(spyRDBStoreRouter.get(label));
        Mockito.verify(spyRDBStoreRouter, Mockito.times(2)).getRouterStoreClient();
        Mockito.verify(spyRDBStoreRouter, Mockito.times(1)).getFromStore(label, mockStore); //only called once with these arguments
    }

    @Test
    public void testGetRouterStoreClient(){

        RDBStoreRouter rdbStoreRouter = RDBStoreRouter.getInstance();

        TripleStoreClientInterface storeClient = rdbStoreRouter.getRouterStoreClient();

        assertNotNull(storeClient);
        assertEquals(RDBStoreRouter.RDBStoreRouterEndpoint, storeClient.getQueryEndpoint());
    }

    @Test
    public void testISRemoteTargetResourceID() {
        assertTrue(RDBStoreRouter.isRemoteTargetResourceID("http://theworldavatar.com/kb/ontokin"));
        assertTrue(RDBStoreRouter.isRemoteTargetResourceID("ontokin"));
        assertTrue(RDBStoreRouter.isRemoteTargetResourceID("ontokin123"));
        assertTrue(RDBStoreRouter.isRemoteTargetResourceID("123"));
        assertTrue(RDBStoreRouter.isRemoteTargetResourceID("citieskg-berlin"));
        assertTrue(RDBStoreRouter.isRemoteTargetResourceID("kb_ontokin"));
        assertFalse(RDBStoreRouter.isRemoteTargetResourceID("test/ontokin"));
    }

    @Test
    public void testGetLabelFromTargetResourceID() {
        assertEquals("ontokin", RDBStoreRouter.getLabelFromTargetResourceID("http://theworldavatar.com/kb/ontokin"));
        assertEquals("ontokin", RDBStoreRouter.getLabelFromTargetResourceID("http:///kb/ontokin"));
        assertEquals("ontokin", RDBStoreRouter.getLabelFromTargetResourceID("http:/kb/ontokin"));
        assertEquals("ontokin", RDBStoreRouter.getLabelFromTargetResourceID("ontokin"));
    }

    ///////////////////////////////////////////
    //Mock ontordbrouter triple store

    private class mockOntordbrouter{

        private MockStoreClient mockStore;

        //construct mock store with no query no update endpoint
        public mockOntordbrouter() {
            mockStore = createMockStore();
        }

        //return created mock ontordbrouter storeclient
        public MockStoreClient create() {
            return mockStore;
        }

        private MockStoreClient createMockStore() {

            MockStoreClient mockStore = new MockStoreClient();
            mockStore.addTriple(
                    "<http://www.theworldavatar.com/kb/ontordbrouter/"+TEST_LABEL+">",
                    "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>",
                    "<http://www.theworldavatar.com/kg/ontordbrouter/TargetRDBResource>");
            mockStore.addTriple(
                    "<http://www.theworldavatar.com/kb/ontordbrouter/"+TEST_LABEL+">",
                    "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>",
                    "<http://www.w3.org/2002/07/owl#NamedIndividual");
            mockStore.addTriple(
                    "<http://www.theworldavatar.com/kb/ontordbrouter/"+TEST_LABEL+">",
                    "<http://www.w3.org/2000/01/rdf-schema#label>",
                    TEST_LABEL);
            mockStore.addTriple(
                    "<http://www.theworldavatar.com/kb/ontordbrouter/"+TEST_LABEL+">",
                    "<http://www.theworldavatar.com/kg/ontordbrouter/hasUrl>",
                    "jdbc:postgresql://localhost:5432/"+TEST_LABEL);

            //Pad store with other data
            mockStore.addTriple(
                    "<http://www.theworldavatar.com/kb/ontordbrouter/test2>",
                    "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>",
                    "<http://www.theworldavatar.com/kg/ontordbrouter/TargetRDBResource>");
            mockStore.addTriple(
                    "<http://www.theworldavatar.com/kb/ontordbrouter/test2>",
                    "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>",
                    "<http://www.w3.org/2002/07/owl#NamedIndividual");
            mockStore.addTriple(
                    "<http://www.theworldavatar.com/kb/ontordbrouter/test2>",
                    "<http://www.w3.org/2000/01/rdf-schema#label>",
                    "test2");
            mockStore.addTriple(
                    "<http://www.theworldavatar.com/kb/ontordbrouter/test2>",
                    "<http://www.theworldavatar.com/kg/ontordbrouter/hasUrl>",
                    "jdbc:postgresql://localhost:5432/test2");


            return mockStore;
        }

    }
}
