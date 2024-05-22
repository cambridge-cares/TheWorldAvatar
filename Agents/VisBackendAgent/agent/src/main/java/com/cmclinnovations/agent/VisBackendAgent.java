package com.cmclinnovations.agent;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.cmclinnovations.agent.service.UIService;

@RestController
public class VisBackendAgent {
	private final UIService uiControlService;
	private static final Logger LOGGER = LogManager.getLogger(VisBackendAgent.class);

	public VisBackendAgent(UIService uiControlService) {
		this.uiControlService = uiControlService;
	}

	@GetMapping("/ui")
	public String getUIControls(@RequestParam(value = "target", defaultValue = "id1") String target) {
		try {
			return this.uiControlService.getControls(target);
		} catch (Exception e) {
			LOGGER.error(e);
			return e.getMessage();
		}
	}
}