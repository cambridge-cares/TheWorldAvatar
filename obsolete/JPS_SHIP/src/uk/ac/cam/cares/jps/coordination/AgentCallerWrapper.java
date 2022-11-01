package uk.ac.cam.cares.jps.coordination;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

/*
* Wrapper to call AgentCaller.executeGet in the
* supplyAsync method. A problem arises because Mockito.MockedStatic:
* "only affects the thread on which this static mock was created and
* it is not safe to use this object from another thread."
* This wrapper allows testing the getNewWasteAsync method
* with mocks.
* */
public class AgentCallerWrapper{
    public String executeGet(String A, String... B){
        return AgentCaller.executeGet(A, B);
    }
}
