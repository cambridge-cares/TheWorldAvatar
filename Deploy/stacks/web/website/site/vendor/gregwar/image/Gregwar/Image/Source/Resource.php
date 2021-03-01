<?php

namespace Gregwar\Image\Source;

/**
 * Have the image directly in a specific resource.
 */
class Resource extends Source
{
    /** @var Resource */
    protected $resource;

    /**
     * Resource constructor.
     *
     * @param resource $resource
     */
    public function __construct($resource)
    {
        $this->resource = $resource;
    }

    /**
     * @return Resource
     */
    public function getResource()
    {
        return $this->resource;
    }
}
