<?php

namespace RocketTheme\Toolbox\ArrayTraits;

/**
 * Implements \Countable interface.
 *
 * @package RocketTheme\Toolbox\ArrayTraits
 * @author RocketTheme
 * @license MIT
 */
trait Countable
{
    /**
     * Implements Countable interface.
     *
     * @return int
     */
    public function count()
    {
        return \count($this->items);
    }
}
