package uk.ac.cam.cares.jps.base.router.test;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import uk.ac.cam.cares.jps.base.router.AgentRouter;

class AgentRouterTest {

	@Test
	void testInitialisation() {
		
		assertNotNull(AgentRouter.getInstance());
	}
	
	@Test
	void testSetRouterEndpoint() {
		fail("Not yet implemented");
	}
	
	@Test
	void testGet() {
		fail("Not yet implemented");
	}
	
	@Test
	void testGetFromStore() {
		fail("Not yet implemented");
	}
	
	@Test
	void testGetQuery() {
		fail("Not yet implemented");
	}
	
}
