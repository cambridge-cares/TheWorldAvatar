This agent is able to run MoDS to perform multi-objective optimisation (MOO) and multi-criteria decision making (MCDM).
To submit a job to be run on the CMCL knowledge graph server via curl or a web browser use this URL https://kg.cmclinnovations.com/mods-agent/request, with a "query" parameter with a value (URL-encoded if working through a browser) similar to the following JSON object:
```JSON
{
	"SimulationType": "MOO",
	"Algorithms": [{
			"name": "algorithm1",
			"type": "MOO",
			"maxNumberOfResults": 10,
			"variables": [{
					"name": "var1",
					"type": "input"
				}, {
					"name": "var2",
					"type": "input"
				}, {
					"name": "var3",
					"type": "input"
				}, {
					"name": "var4",
					"type": "output",
					"objective": "Maximise",
					"minimum": 0.5,
					"weight": 0.5
				}, {
					"name": "var5",
					"type": "output",
					"objective": "Minimise",
					"maximum": 1.5,
					"weight": 0.1
				}, {
					"name": "var6",
					"type": "output",
					"objective": "Maximise",
					"minimum": 2.5,
					"weight": 0.7
				}
			]
		}
	],
	"Inputs": [{
			"name": "var1",
			"values": ["0.1", "0.3", "0.6", "0.1", "0.2"]
		}, {
			"name": "var2",
			"values": ["0.4", "0.9", "0.0", "0.1", "0.8"]
		}, {
			"name": "var3",
			"values": ["0.5", "0.1", "0.2", "0.3", "0.5"]
		}, {
			"name": "var4",
			"values": ["0.1", "0.9", "0.1", "0.7", "0.1"]
		}, {
			"name": "var5",
			"values": ["1.2", "2.0", "1.0", "1.6", "1.7"]
		}, {
			"name": "var6",
			"values": ["2.5", "3.0", "1.2", "2.1", "4.0"]
		}
	]
}
```

The above example can be run by clicking on [this link](https://kg.cmclinnovations.com/mods-agent/request?query=%7B%22SimulationType%22%3A%22MOO%22%2C%22Algorithms%22%3A%5B%7B%22name%22%3A%22algorithm1%22%2C%22type%22%3A%22MOO%22%2C%22maxNumberOfResults%22%3A10%2C%22variables%22%3A%5B%7B%22name%22%3A%22var1%22%2C%22type%22%3A%22input%22%7D%2C%7B%22name%22%3A%22var2%22%2C%22type%22%3A%22input%22%7D%2C%7B%22name%22%3A%22var3%22%2C%22type%22%3A%22input%22%7D%2C%7B%22name%22%3A%22var4%22%2C%22type%22%3A%22output%22%2C%22objective%22%3A%22Maximise%22%2C%22minimum%22%3A0.5%2C%22weight%22%3A0.5%7D%2C%7B%22name%22%3A%22var5%22%2C%22type%22%3A%22output%22%2C%22objective%22%3A%22Minimise%22%2C%22maximum%22%3A1.5%2C%22weight%22%3A0.1%7D%2C%7B%22name%22%3A%22var6%22%2C%22type%22%3A%22output%22%2C%22objective%22%3A%22Maximise%22%2C%22minimum%22%3A2.5%2C%22weight%22%3A0.7%7D%5D%7D%5D%2C%22Inputs%22%3A%5B%7B%22name%22%3A%22var1%22%2C%22values%22%3A%5B%220.1%22%2C%220.3%22%2C%220.6%22%2C%220.1%22%2C%220.2%22%5D%7D%2C%7B%22name%22%3A%22var2%22%2C%22values%22%3A%5B%220.4%22%2C%220.9%22%2C%220.0%22%2C%220.1%22%2C%220.8%22%5D%7D%2C%7B%22name%22%3A%22var3%22%2C%22values%22%3A%5B%220.5%22%2C%220.1%22%2C%220.2%22%2C%220.3%22%2C%220.5%22%5D%7D%2C%7B%22name%22%3A%22var4%22%2C%22values%22%3A%5B%220.1%22%2C%220.9%22%2C%220.1%22%2C%220.7%22%2C%220.1%22%5D%7D%2C%7B%22name%22%3A%22var5%22%2C%22values%22%3A%5B%221.2%22%2C%222.0%22%2C%221.0%22%2C%221.6%22%2C%221.7%22%5D%7D%2C%7B%22name%22%3A%22var6%22%2C%22values%22%3A%5B%222.5%22%2C%223.0%22%2C%221.2%22%2C%222.1%22%2C%224.0%22%5D%7D%5D%7D).

The JSON object returned by this query (URL-encoded if working through a browser) can then be passed as the "query" parameter to this URL https://kg.cmclinnovations.com/mods-agent/output/request.