package com.cmclinnovations.agent.exception;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.context.request.WebRequest;

@ControllerAdvice
public class AgentExceptionHandler {
  @ExceptionHandler(Exception.class)
  public ResponseEntity<String> globalExceptionHandling(Exception exception, WebRequest request) {
    return new ResponseEntity<>(
        exception.getMessage() + String.valueOf(System.currentTimeMillis()),
        HttpStatus.INTERNAL_SERVER_ERROR);
  }
}
