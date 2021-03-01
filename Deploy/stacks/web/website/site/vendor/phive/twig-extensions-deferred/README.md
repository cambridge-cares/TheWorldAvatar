Deferred Twig Extension
=======================
[![Build Status](https://travis-ci.org/rybakit/twig-extensions-deferred.svg?branch=master)](https://travis-ci.org/rybakit/twig-extensions-deferred)

An extension for Twig that allows to defer block rendering.

## Installation

The recommended way to install the extension is through [Composer](http://getcomposer.org):

```sh
$ composer require phive/twig-extensions-deferred:^1.0
```


## Initialization

```php
$twig = new Twig_Environment($loader);
$twig->addExtension(new Phive\Twig\Extensions\Deferred\DeferredExtension());
```

## Simple Example

```jinja
{# outputs bar #}
{% block foo deferred %}
    {{ bar }}
{% endblock %}

{% set bar = 'bar' %}
```


## Advanced Example

Just for example purposes, first create a [global twig variable](http://twig.sensiolabs.org/doc/advanced.html#globals):

```php
$twig = new Twig_Environment($loader);
$twig->addGlobal('data', new ArrayObject());
```

Then build the following set of templates:

```jinja
{# layout.html.twig #}
<!DOCTYPE html>
<html>
    <head>
        ...
    </head>
    <body>
        {% block content '' %}

        {{ data.append('/js/layout-header.js') }}

        {% block javascripts deferred %}
            {% for item in data %}
                <script src="{{ item }}"></script>
            {% endfor %}
        {% endblock %}

        {{ data.append('/js/layout-footer.js') }}
    </body>
</html>


{# page.html.twig #}
{% extends "layout.html.twig" %}

{% block content %}
    {{ data.append('/js/page-header.js') }}

    {% if foo is not defined %}
        {{ include("subpage1.html.twig") }}
    {% else %}
        {{ include("subpage2.html.twig") }}
    {% endif %}

    {{ data.append('/js/page-footer.js') }}
{% endblock %}


{# subpage1.html.twig #}
{{ data.append('/js/subpage1.js') }}


{# subpage2.html.twig #}
{{ data.append('/js/subpage2.js') }}
```

The resulting html will be the following:

```html
<!DOCTYPE html>
<html>
    <head>
        ...
    </head>
    <body>
        <script src="/js/layout-header.js"></script>
        <script src="/js/page-header.js"></script>
        <script src="/js/subpage1.js"></script>
        <script src="/js/page-footer.js"></script>
        <script src="/js/layout-footer.js"></script>
    </body>
</html>
```


## Block overriding

```jinja
{# index.twig #}
{% extends "base.twig" %}
{% block foo %}foo is not deferred anymore{% endblock %}
{% block bar deferred %}bar is deferred now{% endblock %}

{# base.twig #}
{% block foo deferred %}foo is deferred{% endblock %}
{% block bar %}bar is not deferred{% endblock %}
```


## License

Deferred Twig Extension is released under the MIT License. See the bundled [LICENSE](LICENSE) file for details.
