<?php

namespace Phive\Twig\Extensions\Deferred;

class DeferredExtension extends \Twig_Extension
{
    /**
     * @var array
     */
    private $blocks = array();

    /**
     * {@inheritdoc}
     */
    public function getTokenParsers()
    {
        return array(new DeferredTokenParser());
    }

    /**
     * {@inheritdoc}
     */
    public function getNodeVisitors()
    {
        return array(new DeferredNodeVisitor());
    }

    /**
     * {@inheritdoc}
     */
    public function getName()
    {
        return 'deferred';
    }

    public function defer(\Twig_Template $template, $blockName)
    {
        $templateName = $template->getTemplateName();
        $this->blocks[$templateName][] = $blockName;
        ob_start();
    }

    public function resolve(\Twig_Template $template, array $context, array $blocks)
    {
        $templateName = $template->getTemplateName();
        if (empty($this->blocks[$templateName])) {
            return;
        }

        while ($blockName = array_pop($this->blocks[$templateName])) {
            $buffer = ob_get_clean();

            $blocks[$blockName] = array($template, 'block_'.$blockName.'_deferred');
            $template->displayBlock($blockName, $context, $blocks);

            echo $buffer;
        }

        if ($parent = $template->getParent($context)) {
            $this->resolve($parent, $context, $blocks);
        }
    }
}
