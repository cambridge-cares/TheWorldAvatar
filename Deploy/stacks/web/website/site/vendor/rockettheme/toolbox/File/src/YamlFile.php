<?php

namespace RocketTheme\Toolbox\File;

use Symfony\Component\Yaml\Exception\DumpException;
use Symfony\Component\Yaml\Exception\ParseException;
use Symfony\Component\Yaml\Yaml as YamlParser;
use RocketTheme\Toolbox\Compat\Yaml\Yaml as FallbackYamlParser;

/**
 * Implements YAML File reader.
 *
 * @package RocketTheme\Toolbox\File
 * @author RocketTheme
 * @license MIT
 */
class YamlFile extends File
{
    /** @var static[] */
    static protected $instances = [];
    /** @var array */
    static protected $globalSettings = [
        'compat' => true,
        'native' => true
    ];

    /**
     * @param array|null $var
     * @return array
     */
    public function content($var = null)
    {
        /** @var array $content */
        $content = parent::content($var);

        return $content;
    }

    /**
     * Set/get settings.
     *
     * @param array|null $settings
     * @return array
     */
    public static function globalSettings(array $settings = null)
    {
        if ($settings !== null) {
            static::$globalSettings = $settings;
        }

        return static::$globalSettings;
    }

    /**
     * Constructor.
     */
    protected function __construct()
    {
        parent::__construct();

        $this->extension = '.yaml';
    }

    /**
     * Set/get settings.
     *
     * @param array|null $settings
     * @return array
     */
    public function settings(array $settings = null)
    {
        if ($settings !== null) {
            $this->settings = $settings;
        }

        return $this->settings + static::$globalSettings;
    }

    /**
     * Get setting.
     *
     * @param string $setting
     * @param mixed $default
     * @return mixed
     */
    public function setting($setting, $default = null)
    {
        $value = parent::setting($setting);
        if (null === $value) {
            $value = isset(static::$globalSettings[$setting]) ? static::$globalSettings[$setting] : $default;
        }

        return $value;
    }

    /**
     * Check contents and make sure it is in correct format.
     *
     * @param mixed $var
     * @return array
     */
    protected function check($var)
    {
        if (!(\is_array($var) || \is_object($var))) {
            throw new \RuntimeException('Provided data is not an array');
        }

        return (array)$var;
    }

    /**
     * Encode contents into RAW string.
     *
     * @param array $var
     * @return string
     * @throws DumpException
     */
    protected function encode($var)
    {
        return YamlParser::dump($var, $this->setting('inline', 5), $this->setting('indent', 2), YamlParser::PARSE_EXCEPTION_ON_INVALID_TYPE);
    }

    /**
     * Decode RAW string into contents.
     *
     * @param string $var
     * @return array
     * @throws ParseException
     */
    protected function decode($var)
    {
        // Try native PECL YAML PHP extension first if available.
        if (\function_exists('yaml_parse') && $this->setting('native', true)) {
            // Safely decode YAML.
            $saved = @ini_get('yaml.decode_php');
            @ini_set('yaml.decode_php', '0');
            $data = @yaml_parse($var);
            if ($saved !== false) {
                @ini_set('yaml.decode_php', $saved);
            }

            if ($data !== false) {
                return (array)$data;
            }
        }

        try {
            return (array)YamlParser::parse($var);
        } catch (ParseException $e) {
            if ($this->setting('compat', true)) {
                return (array)FallbackYamlParser::parse($var);
            }

            throw $e;
        }
    }
}
