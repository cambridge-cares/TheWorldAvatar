# Grav Page Inject Plugin

`Page Inject` is a powerful [Grav][grav] Plugin that lets you inject entire pages or page content into other pages using simple markdown syntax

# Installation

Installing the Page Inject plugin can be done in one of two ways. Our GPM (Grav Package Manager) installation method enables you to quickly and easily install the plugin with a simple terminal command, while the manual method enables you to do so via a zip file.

## GPM Installation (Preferred)

The simplest way to install this plugin is via the [Grav Package Manager (GPM)](http://learn.getgrav.org/advanced/grav-gpm) through your system's Terminal (also called the command line).  From the root of your Grav install type:

    bin/gpm install page-inject

This will install the Page Inject plugin into your `/user/plugins` directory within Grav. Its files can be found under `/your/site/grav/user/plugins/page-inject`.

## Manual Installation

To install this plugin, just download the zip version of this repository and unzip it under `/your/site/grav/user/plugins`. Then, rename the folder to `page-inject`. You can find these files either on [GitHub](https://github.com/getgrav/grav-plugin-page-inject) or via [GetGrav.org](http://getgrav.org/downloads/plugins#extras).

You should now have all the plugin files under

    /your/site/grav/user/plugins/page-inject

# Config Defaults

```
enabled: true
active: true
processed_content: true
```

If you need to change any value, then the best process is to copy the [page-inject.yaml](page-inject.yaml) file into your `users/config/plugins/` folder (create it if it doesn't exist), and then modify there.  This will override the default settings.

The `active` option allows you to enable/disable the plugin site-wide, and then enable it on page via Page Config overrides. This is useful to optimize performance.

the `processed_content` option means the page is pre-rendered before being injected.  This is the default behavior and means that relative image links and other path-sensitive content works correctly.  You can however set this to `false` and then the raw markdown is inject and processed along with the rest of the current page. This is relevant for `content-inject` links **only**.

### Page Config

You can override the plugin options by adding overrides in the page header frontmatter:

```
page-inject:
    active: true
    processed_content: true
```

# Usage

There are two ways to use this plugin in your markdown content:

1. **Page Injection**

    ```
    [plugin:page-inject](/route/to/page)
    ```

    This approach includes an entire page rendered with the associated template.  This works best for modular page content or content that uses a specific template that provides appropriate styling that is intended to be part of other pages.  You can also pass an optional template name and use that template to render the page (as long as you also provide the template in your theme):

    ```
    [plugin:page-inject](/route/to/page?template=custom-template)
    ```

2. **Content Injection**

    ```
    [plugin:content-inject](/route/to/page)
    ```

    Sometimes you just want the content of another page injected directly into your current page.  Use `content-inject` for this purpose.  The content is not rendered with the associated twig template, merely injected into the current page.

[grav]: http://github.com/getgrav/grav
