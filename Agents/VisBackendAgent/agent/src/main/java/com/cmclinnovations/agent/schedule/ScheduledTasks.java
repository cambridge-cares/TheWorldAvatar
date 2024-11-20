package com.cmclinnovations.agent.schedule;

import java.text.SimpleDateFormat;
import java.util.Date;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import com.cmclinnovations.agent.service.LifecycleService;

@Component
@ConditionalOnProperty(name = "tasks.enabled", havingValue = "true", matchIfMissing = false)
public class ScheduledTasks {
  private final LifecycleService lifecycleService;

  private static final SimpleDateFormat dateFormat = new SimpleDateFormat("HH:mm:ss");
  private static final Logger LOGGER = LogManager.getLogger(ScheduledTasks.class);

  public ScheduledTasks(LifecycleService lifecycleService) {
    this.lifecycleService = lifecycleService;
  }

  @Scheduled(cron = "0 0 6 * * *")
  public void generateTasksForToday() {
    LOGGER.info("Performing the scheduled task to generate today's tasks at {} ", dateFormat.format(new Date()));
    this.lifecycleService.genActiveServiceOccurrences();
  }
}
