<?php
/**
 * PageInject
 *
 * This plugin embeds other Grav pages from markdown URLs
 *
 * Licensed under MIT, see LICENSE.
 */

namespace Grav\Plugin;

use Grav\Common\Config\Config;
use Grav\Common\Grav;
use Grav\Common\Helpers\Excerpts;
use Grav\Common\Page\Interfaces\PageInterface;
use Grav\Common\Page\Pages;
use Grav\Common\Plugin;
use Grav\Common\Page\Page;
use Grav\Common\Uri;
use Grav\Framework\Psr7\Response;
use Grav\Framework\RequestHandler\Exception\RequestException;
use Grav\Plugin\Admin\Admin;
use Psr\Http\Message\ResponseInterface;
use RocketTheme\Toolbox\Event\Event;

class PageInjectPlugin extends Plugin
{
    /**
     * Return a list of subscribed events.
     *
     * @return array    The list of events of the plugin of the form
     *                      'name' => ['method_name', priority].
     */
    public static function getSubscribedEvents()
    {
        return [
            'onPluginsInitialized' => ['onPluginsInitialized', 0],
            'registerNextGenEditorPlugin' => ['registerNextGenEditorPlugin', 0],
        ];
    }

    /**
     * Initialize configuration.
     */
    public function onPluginsInitialized()
    {
        if ($this->isAdmin()) {
            $this->enable([
                'onAdminTaskExecute' => ['onAdminTaskExecute', 0],
            ]);
            return;
        }

        $this->enable([
            'onPageContentRaw' => ['onPageContentRaw', 0],
        ]);
    }

    /**
     *
     * @param Event $e
     */
    public function onAdminTaskExecute(Event $e): void
    {
        if ($e['method'] === 'taskPageInjectData') {
            header('Content-type: application/json');
            header('Cache-Control: no-cache, no-store, must-revalidate');
            $controller = $e['controller'];

            if (!$controller->authorizeTask('pageInject', ['admin.pages', 'admin.super'])) {
                http_response_code(401);
                $json_response = [
                    'status'  => 'error',
                    'message' => '<i class="fa fa-warning"></i> Unable to get PageInject data',
                    'details' => 'Insufficient permissions for this user.'
                ];
                echo json_encode($json_response);
                exit;
            }

            error_reporting(1);
            set_time_limit(0);

            $json_response = $this->getPageInjectData();

            echo json_encode($json_response);
            exit;
        }
    }

    protected function getPageInjectData()
    {
        $request = $this->grav['request'];
        $data = $request->getParsedBody();
        $page_routes = $data['routes'] ?? [];

        /** @var Pages $pages */
        $pages = Admin::enablePages();

        foreach($page_routes as $route) {
            /** @var PageInterface */
            $page = $pages->find($route);

            if (!$page) {
                $data = [
                    'status' => 'Error',
                    'message' => 'Page not found',
                    'data' => []
                ];
            } else {
                $data = [
                    'status'  => 'success',
                    'message' => 'Page found',
                    'data' => [
                        'title' => $page->title(),
                        'route' => $page->route(),
                        'modified' => $page->modified(),
                        'template' => $page->template(),
                    ]
                ];
            }

            $json['data'][] = $data;
            $json['available_templates'] = $pages->pageTypes();
        }

        return $json;
    }

    /**
     * Add content after page content was read into the system.
     *
     * @param  Event  $event An event object, when `onPageContentRaw` is fired.
     */
    public function onPageContentRaw(Event $event)
    {
        /** @var Page $page */
        $page = $event['page'];

        /** @var Config $config */
        $config = $this->mergeConfig($page);


        if ($config->get('enabled') && $config->get('active')) {
            // Get raw content and substitute all formulas by a unique token
            $raw = $page->getRawContent();

            // build an anonymous function to pass to `parseLinks()`
            $function = function ($matches) use (&$page, &$twig, &$config) {

                $search = $matches[0];
                $type = $matches[1];
                $page_path = $matches[3] ?: $matches[2];
                $template = $matches[4];

                $page_path = Uri::convertUrl($page, $page_path, 'link', false, true);

                $inject = $page->find($page_path);
                if ($inject) {
                    // Force HTML to avoid issues with News Feeds
                    $inject->templateFormat('html');
                    if ($type == 'page-inject') {
                        if ($template) {
                            $inject->template($template);
                        }
                        $inject->modularTwig(true);
                        $replace = $inject->content();

                    } else {
                        if ($config->get('processed_content')) {
                            $replace = $inject->content();
                        } else {
                            $replace = $inject->rawMarkdown();
                        }
                    }

                } else {
                    // replace with what you started with
                    $replace = $matches[0];
                }

                // do the replacement
                return str_replace($search, $replace, $search);
            };

            // set the parsed content back into as raw content
            $page->setRawContent($this->parseInjectLinks($raw, $function));
        }
    }

    protected function parseInjectLinks($content, $function)
    {
        $regex = '/\[plugin:(content-inject|page-inject)\]\(((.*)\?template=(.*)|(.*))\)/i';
        return preg_replace_callback($regex, $function, $content);
    }

    public function registerNextGenEditorPlugin($event) {
        $plugins = $event['plugins'];

        // page-inject
        $plugins['css'][] = 'plugin://page-inject/nextgen-editor/plugins/page-inject/page-inject.css';
        $plugins['js'][] = 'plugin://page-inject/nextgen-editor/plugins/page-inject/page-inject.js';

        $event['plugins']  = $plugins;
        return $event;
    }
}
