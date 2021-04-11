<?php

namespace RocketTheme\Toolbox\File;

/**
 * Implements PHP File reader.
 *
 * @package RocketTheme\Toolbox\File
 * @author RocketTheme
 * @license MIT
 */
class PhpFile extends File
{
    /** @var string */
    protected $extension = '.php';

    /** @var static[] */
    static protected $instances = [];

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
     * Saves PHP file and invalidates opcache.
     *
     * @param  mixed  $data  Optional data to be saved, usually array.
     * @return void
     * @throws \RuntimeException
     */
    public function save($data = null)
    {
        parent::save($data);

        // Invalidate configuration file from the opcache.
        if (null !== $this->filename && \function_exists('opcache_invalidate')) {
            @opcache_invalidate($this->filename, true);
        }
    }

    /**
     * Check contents and make sure it is in correct format.
     *
     * @param mixed $var
     * @return array
     * @throws \RuntimeException
     */
    protected function check($var)
    {
        if (!(\is_array($var) || \is_object($var))) {
            throw new \RuntimeException('Provided data is not an array');
        }

        return (array)$var;
    }

    /**
     * Encode configuration object into RAW string (PHP class).
     *
     * @param array $var
     * @return string
     * @throws \RuntimeException
     */
    protected function encode($var)
    {
        // Build the object variables string
        return "<?php\nreturn {$this->encodeArray((array) $var)};\n";
    }

    /**
     * Method to get an array as an exported string.
     *
     * @param array $a The array to get as a string.
     * @param int $level Used internally to indent rows.
     * @return string
     */
    protected function encodeArray(array $a, $level = 0)
    {
        $r = [];
        foreach ($a as $k => $v) {
            if (\is_array($v) || \is_object($v)) {
                $r[] = var_export($k, true) . ' => ' . $this->encodeArray((array) $v, $level + 1);
            } else {
                $r[] = var_export($k, true) . ' => ' . var_export($v, true);
            }
        }

        $space = str_repeat('    ', $level);

        return "[\n    {$space}" . implode(",\n    {$space}", $r) . "\n{$space}]";
    }

    /**
     * Decode PHP file into contents.
     *
     * @param string $var
     * @return array
     */
    protected function decode($var)
    {
        return (array)include $this->filename;
    }
}
