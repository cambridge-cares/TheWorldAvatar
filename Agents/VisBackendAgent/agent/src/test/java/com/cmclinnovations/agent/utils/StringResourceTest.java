package com.cmclinnovations.agent.utils;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import com.cmclinnovations.agent.service.FileServiceTest;

class StringResourceTest {
  @Test
  void testGetLocalName() {
    assertEquals(FileServiceTest.IRI_LOCAL_NAME_TEST_CASE1,
        StringResource.getLocalName(FileServiceTest.IRI_TEST_CASE1));
    assertEquals(FileServiceTest.IRI_LOCAL_NAME_TEST_CASE2,
        StringResource.getLocalName(FileServiceTest.IRI_TEST_CASE2));
    assertEquals(FileServiceTest.IRI_LOCAL_NAME_TEST_CASE3,
        StringResource.getLocalName(FileServiceTest.IRI_TEST_CASE3));
  }
}
