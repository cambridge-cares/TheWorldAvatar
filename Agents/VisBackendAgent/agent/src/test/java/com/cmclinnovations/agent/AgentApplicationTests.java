package com.cmclinnovations.agent;

import java.io.File;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.servlet.MockMvc;

import com.cmclinnovations.agent.service.FileService;
import com.cmclinnovations.agent.service.FileServiceTest;

@SpringBootTest
@AutoConfigureMockMvc
class AgentApplicationTests {
  @Autowired
  private MockMvc mockMvc;

  @Test
  void testStatusRoute() throws Exception {
    this.mockMvc.perform(get("/status"))
        .andDo(print())
        .andExpect(status().isOk())
        .andExpect(content().string("Agent is ready to receive requests."));
  }

  @Test
  void testFormRoute_MissingFormResource() throws Exception {
    this.mockMvc.perform(get("/form/invalid"))
        .andDo(print())
        .andExpect(status().isInternalServerError())
        .andExpect(TestUtils.contentContains("Error encountered at"))
        .andExpect(TestUtils.contentContains(
            "Resource at file:/usr/local/tomcat/resources/application-form.json is not found. Please ensure you have a valid resource in the file path."));
  }

  @Test
  void testFormRoute_InvalidRoute() throws Exception {
    File sampleFile = FileServiceTest.genSampleFile("/" + FileService.APPLICATION_FORM_RESOURCE, "{}");
    try {
      this.mockMvc.perform(get("/form/invalid"))
          .andDo(print())
          .andExpect(status().isBadRequest())
          .andExpect(content().string(
              "Route is invalid at /invalid! If this route is intended to be enabled, please contact your technical team for assistance."));
    } finally {
      sampleFile.delete();
    }
  }

  @Test
  void testInstanceRoute_MissingFormResource() throws Exception {
    this.mockMvc.perform(get("/type/invalid"))
        .andDo(print())
        .andExpect(status().isInternalServerError())
        .andExpect(TestUtils.contentContains("Error encountered at"))
        .andExpect(TestUtils.contentContains(
            "Resource at file:/usr/local/tomcat/resources/application-form.json is not found. Please ensure you have a valid resource in the file path."));
  }

  @Test
  void testInstanceRoute_InvalidRoute() throws Exception {
    File sampleFile = FileServiceTest.genSampleFile("/" + FileService.APPLICATION_FORM_RESOURCE, "{}");
    try {
      this.mockMvc.perform(get("/type/invalid"))
          .andDo(print())
          .andExpect(status().isBadRequest())
          .andExpect(content().string(
              "Route is invalid at /invalid! If this route is intended to be enabled, please contact your technical team for assistance."));
    } finally {
      sampleFile.delete();
    }
  }
}