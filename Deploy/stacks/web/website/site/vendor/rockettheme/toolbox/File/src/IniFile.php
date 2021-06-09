<?php

namespace RocketTheme\Toolbox\File;

/**
 * Implements INI File reader.
 *
 * @package RocketTheme\Toolbox\File
 * @author RocketTheme
 * @license MIT
 */
class IniFile extends File
{
    /** @var string */
    protected $extension = '.ini';

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
     * Check contents and make sure it is in correct format.
     *
     * @param mixed $var
     * @return array
     * @throws \RuntimeException
     */
    protected function check($var)
    {
        if (!\is_array($var)) {
            throw new \RuntimeException('Provided data is not an array');
        }

        return $var;
    }

    /**
     * Encode configuration object into RAW string (INI).
     *
     * @param array $var
     * @return string
     * @throws \RuntimeException
     */
    protected function encode($var)
    {
        $string = '';
        foreach ($var as $key => $value) {
            $string .= $key . '="' .  preg_replace(
                    ['/"/', '/\\\/', "/\t/", "/\n/", "/\r/"],
                    ['\"',  '\\\\', '\t',   '\n',   '\r'],
                    $value
                ) . "\"\n";
        }
        return $string;
    }

    /**
     * Decode INI file into contents.
     *
     * @param string $var
     * @return array
     * @throws \RuntimeException
     */
    protected function decode($var)
    {
        $decoded = null !== $this->filename && file_exists($this->filename) ? @parse_ini_file($this->filename) : [];

        if ($decoded === false) {
            throw new \RuntimeException("Decoding file '{$this->filename}' failed'");
        }

        return $decoded;
    }
}
