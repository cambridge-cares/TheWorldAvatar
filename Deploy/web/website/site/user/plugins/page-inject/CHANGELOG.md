# v1.4.4
## 01/29/2021

1. [](#bugfix)
   * NextGen Editor: Fixed Page Inject UI links missing the base_root [getgrav/grav-premium-issues#30](https://github.com/getgrav/grav-premium-issues/issues/30)
   * NextGen Editor: Moved list of available templates to input text to support partials and any twig template [getgrav/grav-premium-issues#24](https://github.com/getgrav/grav-premium-issues/issues/24)
   
# v1.4.3
## 01/15/2021

1. [](#improved)
   * NextGen Editor: Updated upcast/downcast syntax to support latest version

# v1.4.2
## 12/20/2020

1. [](#bugfix)
    * Fixed `undefined` value when inserting a new Page-Inject shortcode, preventing Page picker to load

# v1.4.1
## 12/18/2020

1. [](#improved)
    * NextGen Editor: Properly restore the initial stored path when loading the Page Picker

# v1.4.0
## 12/02/2020

1. [](#new)
    * NEW support for NextGen Editor
    * Added a new `taskPageInjectData` to be used by NextGen Editor integration
1. [](#bugfix)
    * Added missing admin nonce

# v1.3.1
## 04/15/2019

1. [](#bugfix)
    * Fixed issue with Feed plugin and Page-Inject by forcing template to `html` [feed#42](https://github.com/getgrav/grav-plugin-feed/issues/42)

# v1.3.0
## 12/08/2017

1. [](#new)
    * Added multi-lang support to Page Inject plugin [#10](https://github.com/getgrav/grav-plugin-page-inject/issues/10)

# v1.2.0
## 10/11/2016

1. [](#improved)
    * Support Grav-style link route resolution (e.g. `../your-route`) [#5](https://github.com/getgrav/grav-plugin-page-inject/issues/5)
1. [](#bugfix)
    * Fixed issue with `page-inject` processing Twig twice [#7](https://github.com/getgrav/grav-plugin-page-inject/issues/7)

# v1.1.1
## 10/21/2015

1. [](#new)
    * Added `active` config option to enable/disable site-wide
1. [](#bugfix)
    * Fixed issue with plugin not processing reliably with cache-enabled

# v1.1.0
## 08/25/2015

1. [](#improved)
    * Added blueprints for Grav Admin plugin

# v1.0.0
## 06/18/2015

1. [](#new)
    * ChangeLog started...
