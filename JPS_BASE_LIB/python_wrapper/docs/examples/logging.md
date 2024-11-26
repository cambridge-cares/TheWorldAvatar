`agentlogging` is originally placed [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/f290fb98ce746b591d8b8c93cca1e89a409c959e/Agents/utils/python-utils). It is now packaged and released as part of `twa` python wrapper.

One can import and use it as below:

```python
from twa import agentlogging

dev_logger = agentlogging.get_logger("dev")
dev_logger.debug("This is a DEBUG statement")
dev_logger.info("This is an INFO statement")

prod_logger = agentlogging.get_logger("prod")
prod_logger.debug("This is a DEBUG statement")
prod_logger.info("This is an INFO statement")
```

For more details, see the [Logging](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Logging) page on TWA Wiki.
