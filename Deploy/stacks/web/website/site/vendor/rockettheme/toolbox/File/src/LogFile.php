<?php

namespace RocketTheme\Toolbox\File;

/**
 * Implements Log File reader.
 *
 * @package RocketTheme\Toolbox\File
 * @author RocketTheme
 * @license MIT
 */
class LogFile extends File
{
    /** @var static[] */
    static protected $instances = [];

    /**
     * Constructor.
     */
    protected function __construct()
    {
        parent::__construct();

        $this->extension = '.log';
    }

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
     */
    protected function check($var)
    {
        if (!(\is_array($var) || \is_object($var))) {
            throw new \RuntimeException('Provided data is not an array');
        }

        return (array)$var;
    }

    /**
     * Encode contents into RAW string (unsupported).
     *
     * @param string $var
     * @return string
     * @throws \BadMethodCallException
     */
    protected function encode($var)
    {
        throw new \BadMethodCallException('Saving log file is forbidden.');
    }

    /**
     * Decode RAW string into contents.
     *
     * @param string $var
     * @return array
     */
    protected function decode($var)
    {
        $lines = preg_split('#(\r\n|\n|\r)#', $var) ?: [];

        $results = [];
        foreach ($lines as $line) {
            preg_match('#^\[(.*)\] (.*)  @  (.*)  @@  (.*)$#', $line, $matches);
            if ($matches) {
                $results[] = ['date' => $matches[1], 'message' => $matches[2], 'url' => $matches[3], 'file' => $matches[4]];
            }
        }

        return $results;
    }
}
