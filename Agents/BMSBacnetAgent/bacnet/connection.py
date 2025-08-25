import BAC0
class Connection():
    def connect(self, ip):
        self.bacnet = BAC0.connect()
        self.bacnet.discover(networks=[1, 1161], limits=(0, 4194303), global_broadcast=True)

    def extractValue(self, devices):
        for key, vs in devices.items():
            for prop, value in vs:
                if prop =='presentValue':
                    return value

    def readMultiple(self, device, prop_dict):
        values = self.bacnet.readMultiple(device, request_dict=prop_dict)
        return self.extractValue(values)


