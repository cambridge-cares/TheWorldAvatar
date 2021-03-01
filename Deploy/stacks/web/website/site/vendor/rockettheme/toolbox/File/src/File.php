<?php

namespace RocketTheme\Toolbox\File;

/**
 * Implements Universal File Reader.
 *
 * @package RocketTheme\Toolbox\File
 * @author RocketTheme
 * @license MIT
 */
class File extends AbstractFile
{
    /**
     * Get/set parsed file contents.
     *
     * @param string|null $var
     * @return string
     * @throws \RuntimeException
     */
    public function content($var = null)
    {
        /** @var string $content */
        $content = parent::content($var);

        return $content;
    }
}
