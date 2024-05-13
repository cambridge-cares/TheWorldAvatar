# Tutorial

This directory provides a starter guide for new users to setup and deploy a sample visualisation platform.

Transfer the following contents from the `<root>/uploads` [directory](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/twa-vis-platform/uploads/) into this directory. First, the contents of the `<root>/uploads/images/defaults` and `<root>/uploads/images/utils` directory must be copied into this directory's `images` path. Second, copy the `<root>/uploads/optional-pages/help-page.md` file into this directory's `optional-pages` path. 

Once transferred, copy the contents of the `example/uploads` directory into your required directory path. Start your visualisation following any of the deployment workflow. The original `uploads` [directory](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/twa-vis-platform/uploads) hosts the minimal setup required to run the platform, and must be transferred into the example for it to function.

For stack deployments, ensure the `vip.json` config is added to the `stack-manager/inputs/config/services` directory and start the stack with the added service. The service will be exposed at the `visualisation` path, e.g. `localhost:3838/visualisation`.

## Configuration options

### <root>/uploads/config/data.json

An example for displaying subset of layers is seen in the college example, in which users can set different views for the colleges - all college and separated by their founding year.