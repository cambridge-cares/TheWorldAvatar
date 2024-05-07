package uk.ac.cam.cares.jps.model;

/**
 * This is a data model using Java record keyword that encapsulate the JSON result from both user and to do API
 */
public record TodoWithUser(Todo todo, User user) {
}
