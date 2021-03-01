<?php

namespace RocketTheme\Toolbox\ResourceLocator;

/**
 * Implements recursive iterator over filesystem.
 *
 * @package RocketTheme\Toolbox\ResourceLocator
 * @author RocketTheme
 * @license MIT
 */
class RecursiveUniformResourceIterator extends UniformResourceIterator implements \RecursiveIterator
{
    /** @var string|null */
    protected $subPath;

    /**
     * @return RecursiveUniformResourceIterator
     */
    public function getChildren()
    {
        $subPath = $this->getSubPathName();

        return (new static($this->getUrl(), $this->flags, $this->locator))->setSubPath($subPath);
    }

    /**
     * @param bool|null $allow_links
     * @return bool
     */
    public function hasChildren($allow_links = null)
    {
        $allow_links = (bool)($allow_links !== null ? $allow_links : $this->flags & \FilesystemIterator::FOLLOW_SYMLINKS);

        return $this->isDir() && !$this->isDot() && ($allow_links || !$this->isLink());
    }

    /**
     * @return string|null
     */
    public function getSubPath()
    {
        return $this->subPath;
    }

    /**
     * @return string
     */
    public function getSubPathName()
    {
        return ($this->subPath ? $this->subPath . '/' : '') . $this->getFilename();
    }

    /**
     * @param string $path
     * @return $this
     * @internal
     */
    public function setSubPath($path)
    {
        $this->subPath = $path;

        return $this;
    }
}
