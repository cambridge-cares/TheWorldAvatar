<?php

namespace RocketTheme\Toolbox\ArrayTraits;

use Symfony\Component\Yaml\Exception\DumpException;
use Symfony\Component\Yaml\Yaml;

/**
 * Implements ExportInterface.
 *
 * @package RocketTheme\Toolbox\ArrayTraits
 * @author RocketTheme
 * @license MIT
 */
trait Export
{
    /**
     * Convert object into an array.
     *
     * @return array
     */
    public function toArray()
    {
        return $this->items;
    }

    /**
     * Convert object into YAML string.
     *
     * @param int $inline The level where you switch to inline YAML.
     * @param int $indent The amount of spaces to use for indentation of nested nodes.
     * @return string A YAML string representing the object.
     * @throws DumpException
     */
    public function toYaml($inline = 3, $indent = 2)
    {
        return Yaml::dump($this->toArray(), $inline, $indent, Yaml::PARSE_EXCEPTION_ON_INVALID_TYPE);
    }

    /**
     * Convert object into JSON string.
     *
     * @return string
     */
    public function toJson()
    {
        $string = json_encode($this->toArray());
        if (!\is_string($string)) {
            throw new \RuntimeException('Failed to encode array', 500);
        }

        return $string;
    }
}
