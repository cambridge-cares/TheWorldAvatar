# Tutorial

This directory provides a starter guide for new users to setup and deploy a sample visualisation platform.

Copy the contents of the `example/uploads` directory into your required directory path either at `./code/public` or your preferred directory. Start your visualisation following any of the deployment workflow. The minimal setup required to run the platform is described [here](#required-template-resources).

For stack deployments, ensure the `vip.json` config is added to the `stack-manager/inputs/config/services` directory and start the stack with the added service. The service will be exposed at the `visualisation` path, e.g. `localhost:3838/visualisation`.

## Configuration options

### <root>/uploads/config/data.json

An example for displaying subset of layers is seen in the college example, in which users can set different views for the colleges - all college and separated by their founding year.

The `expanded` parameter is deliberately implemented for nested groups with layers to optimise initial user interactions for a smoother experience. If the parameter is implemented at the root level, all layers will be displayed once expanded, which can lead to poor render times for larger groups.

### Required template resources

When customising your platform, default resources have already been set up within the `public` mounted directory within the container. A minimal deployment set-up will require a `ui-settings.json`, `map-settings.json`, and `data-settings.json` with the corresponding `data.json` files. Additional image or icon resources can be placed into the `images` directory.