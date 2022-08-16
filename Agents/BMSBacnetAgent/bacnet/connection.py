import BAC0
class Connection():
    def connect(self, ip):
        self.bacnet = BAC0.connect(ip=ip)
        self.bacnet.discover(networks=[1, 1161], limits=(0, 4194303), global_broadcast=True)

    def extractValue(self, devices):

        data = []
        for key, vs in devices.items():
            for prop, value in vs:
                if prop =='presentValue':
                    data.append(value)
        return data

    def readMultiple(self, device, prop_dict):
        print(self.bacnet)
        values = self.bacnet.readMultiple(device, request_dict=prop_dict)
        return self.extractValue(values)


