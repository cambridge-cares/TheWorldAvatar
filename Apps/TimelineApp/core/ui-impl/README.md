# UI-Impl Module

This module hosts shared ViewModels that can be accessed across different feature modules. The ViewModels in this module typically follow the activity lifecycle and manage app-level states such as user preferences and login status.

In addition to ViewModels, the module also includes UI components that depend on these shared ViewModels. These components are commonly reused across multiple feature modules.

Separating `UI-Impl` from `UI-Base` allows `UI-Base` to remain a static UI module, eliminating unnecessary dependencies on other core modules such as core:login, core:data, and core:network.