package com.cmclinnovations.agent;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.servlet.MockMvc;

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
}