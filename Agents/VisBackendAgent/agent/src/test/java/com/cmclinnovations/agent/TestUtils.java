package com.cmclinnovations.agent;

import org.springframework.test.web.servlet.ResultMatcher;

public class TestUtils {
  /** Check if the result response entity contains the expected substring.
   * 
   * @param expectedSubstring The expected substring.
   * @return
   */
  public static ResultMatcher contentContains(final String expectedSubstring) {
    return result -> {
      String content = result.getResponse().getContentAsString();
      if (!content.contains(expectedSubstring)) {
        throw new AssertionError("Response content does not contain the expected substring: " + expectedSubstring);
      }
    };
  }
}