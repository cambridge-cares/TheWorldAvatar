<?php

namespace RocketTheme\Toolbox\ArrayTraits;

/**
 * Implements Constructor for setting items.
 *
 * @package RocketTheme\Toolbox\ArrayTraits
 * @author RocketTheme
 * @license MIT
 */
trait Constructor
{
    /**
     * Constructor to initialize array.
     *
     * @param array $items Initial items inside the iterator.
     */
    public function __construct(array $items = [])
    {
        $this->items = $items;
    }
}
