package com.cmclinnovations.agent.model.response;

import org.springframework.http.ResponseEntity;

/**
 * A model for an API response to be returned from the agent.
 */
public class ApiResponse {
  private String iri;
  private String message;

  public ApiResponse() {
  }

  public ApiResponse(String message) {
    this.message = message;
  }

  public ApiResponse(ResponseEntity<String> response) {
    this.message = response.getBody();
  }

  public ApiResponse(String message, String iri) {
    this.message = message;
    this.iri = iri;
  }

  /**
   * Retrieve the message.
   */
  public String getMessage() {
    return this.message;
  }

  public void setMessage(String message) {
    this.message = message;
  }

  /**
   * Retrieve the iri.
   */
  public String getIri() {
    return this.iri;
  }

  public void setIri(String iri) {
    this.iri = iri;
  }
}
