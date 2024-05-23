package com.cmclinnovations.agent.repo;

import org.springframework.stereotype.Repository;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * Service class for interactions with the knowledge graph.
 */
@Repository
public class KGRepositoryService implements KGRepository {
  /**
   * Queries the knowledge graph with the specified namespace and query.
   * 
   * @param namespace the namespace of the blazegraph.
   * @param query     the query for execution.
   */
  @Override
  public String queryKg(String namespace, String query) {
    // WIP: Something is fiddly with the BlazegraphClient in stack-clients package,
    // so the workaround is to enforce an endpoint directly for the moment
    RemoteStoreClient client = new RemoteStoreClient(namespace);
    return client.executeQuery(query).toString();
  }
}
