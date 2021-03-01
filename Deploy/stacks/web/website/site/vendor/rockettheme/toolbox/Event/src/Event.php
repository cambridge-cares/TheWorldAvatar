<?php

namespace RocketTheme\Toolbox\Event;

use RocketTheme\Toolbox\ArrayTraits\ArrayAccess;
use RocketTheme\Toolbox\ArrayTraits\Constructor;
use RocketTheme\Toolbox\ArrayTraits\Export;
use Symfony\Component\EventDispatcher\Event as BaseEvent;

/**
 * Implements Symfony Event interface.
 *
 * @package RocketTheme\Toolbox\Event
 * @author RocketTheme
 * @license MIT
 * @deprecated Event classes will be removed in the future. Use PSR-14 implementation instead.
 */
class Event extends BaseEvent implements \ArrayAccess
{
    use ArrayAccess, Constructor, Export;

    /** @var array */
    protected $items = [];
}
