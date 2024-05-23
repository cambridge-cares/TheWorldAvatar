package com.cmclinnovations.agent.service.testdouble;

import com.cmclinnovations.agent.model.DropdownResourceTest;
import com.cmclinnovations.agent.repo.KGRepository;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

public class FakeKGRepository implements KGRepository {
  private static final ObjectMapper MAPPER = new ObjectMapper();

  @Override
  public String queryKg(String namespace, String query) {
    try {
      ArrayNode response = MAPPER.createArrayNode();
      ObjectNode optionNode = MAPPER.createObjectNode();
      optionNode.put("Property", DropdownResourceTest.SAMPLE_OPTION_GROUP);
      optionNode.put("Value", DropdownResourceTest.SAMPLE_OPTION_ONE);
      response.add(optionNode);
      return MAPPER.writerWithDefaultPrettyPrinter().writeValueAsString(response);
    } catch (Exception e) {
      // Do nothing for exception
    }
    return "";
  }
}
