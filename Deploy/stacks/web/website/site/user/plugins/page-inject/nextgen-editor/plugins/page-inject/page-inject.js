(function() {

let availableTemplates = {};

const Widget = window.nextgenEditor.classes.widget.class;
const Command = window.nextgenEditor.classes.core.command.class;
const { toWidget } = window.nextgenEditor.classes.widget.utils;
const { showPagePicker, showSettingsPopup } = window.nextgenEditor.exports;

const itemTypes = {
  page: 'Page Injection',
  content: 'Content Injection',
};

window.nextgenEditor.addHook('hookInit', () => {
  window.nextgenEditor.addButtonGroup('page-inject', {
    label: 'Page Inject',
  });

  window.nextgenEditor.addButton('page-inject-page', {
    group: 'page-inject',
    command: { name: 'page-inject', params: { type: 'page', value: '' } },
    label: 'Page Injection',
  });

  window.nextgenEditor.addButton('page-inject-content', {
    group: 'page-inject',
    command: { name: 'page-inject', params: { type: 'content', value: '' } },
    label: 'Content Injection',
  });
});

function uncollapse(input) {
  const domOutput = new DOMParser().parseFromString(input, 'text/html');

  let innerHTML = '';

  const attributes = [...domOutput.body.firstChild.attributes]
    .reduce((acc, attribute) => ({ ...acc, [attribute.name]: attribute.value }), {});

  /* eslint-disable indent, no-multi-spaces */
  innerHTML += '<span class="pi-wrapper">';
  innerHTML +=   `<span class="pi-type">${itemTypes[attributes.type] || ''}</span>`;
  innerHTML +=   `<span class="pi-title">${attributes.title || ''}</span>`;
  innerHTML +=   `<a_reserved target="_blank" class="pi-route" href="${GravAdmin?.config?.base_url_simple || ''}${attributes.route || ''}">${attributes.route || ''}</a_reserved>`;
  innerHTML +=   '<svg class="pi-route-settings" viewBox="0 0 24 24" fill="currentColor" stroke="none" onclick="pageInjectRouteSettings.call(this)">';
  innerHTML +=     '<path d="M9 4.58V4c0-1.1.9-2 2-2h2a2 2 0 0 1 2 2v.58a8 8 0 0 1 1.92 1.11l.5-.29a2 2 0 0 1 2.74.73l1 1.74a2 2 0 0 1-.73 2.73l-.5.29a8.06 8.06 0 0 1 0 2.22l.5.3a2 2 0 0 1 .73 2.72l-1 1.74a2 2 0 0 1-2.73.73l-.5-.3A8 8 0 0 1 15 19.43V20a2 2 0 0 1-2 2h-2a2 2 0 0 1-2-2v-.58a8 8 0 0 1-1.92-1.11l-.5.29a2 2 0 0 1-2.74-.73l-1-1.74a2 2 0 0 1 .73-2.73l.5-.29a8.06 8.06 0 0 1 0-2.22l-.5-.3a2 2 0 0 1-.73-2.72l1-1.74a2 2 0 0 1 2.73-.73l.5.3A8 8 0 0 1 9 4.57zM7.88 7.64l-.54.51-1.77-1.02-1 1.74 1.76 1.01-.17.73a6.02 6.02 0 0 0 0 2.78l.17.73-1.76 1.01 1 1.74 1.77-1.02.54.51a6 6 0 0 0 2.4 1.4l.72.2V20h2v-2.04l.71-.2a6 6 0 0 0 2.41-1.4l.54-.51 1.77 1.02 1-1.74-1.76-1.01.17-.73a6.02 6.02 0 0 0 0-2.78l-.17-.73 1.76-1.01-1-1.74-1.77 1.02-.54-.51a6 6 0 0 0-2.4-1.4l-.72-.2V4h-2v2.04l-.71.2a6 6 0 0 0-2.41 1.4zM12 16a4 4 0 1 1 0-8 4 4 0 0 1 0 8zm0-2a2 2 0 1 0 0-4 2 2 0 0 0 0 4z"></path>';
  innerHTML +=   '</svg>';

  if (attributes.type === 'page') {
    const templateValue = attributes.template
      ? availableTemplates[attributes.template]
        ? `${availableTemplates[attributes.template]} template`
        : `${attributes.template} template`
      : 'No template selected';

    innerHTML += `<span class="pi-template">${templateValue}</span>`;
  }

  innerHTML += '</span>';
  innerHTML += '<span class="pi-settings">';
  innerHTML +=   '<svg viewBox="0 0 24 24" fill="currentColor" stroke="none" onclick="pageInjectSettings.call(this)">';
  innerHTML +=     '<path d="M9 4.58V4c0-1.1.9-2 2-2h2a2 2 0 0 1 2 2v.58a8 8 0 0 1 1.92 1.11l.5-.29a2 2 0 0 1 2.74.73l1 1.74a2 2 0 0 1-.73 2.73l-.5.29a8.06 8.06 0 0 1 0 2.22l.5.3a2 2 0 0 1 .73 2.72l-1 1.74a2 2 0 0 1-2.73.73l-.5-.3A8 8 0 0 1 15 19.43V20a2 2 0 0 1-2 2h-2a2 2 0 0 1-2-2v-.58a8 8 0 0 1-1.92-1.11l-.5.29a2 2 0 0 1-2.74-.73l-1-1.74a2 2 0 0 1 .73-2.73l.5-.29a8.06 8.06 0 0 1 0-2.22l-.5-.3a2 2 0 0 1-.73-2.72l1-1.74a2 2 0 0 1 2.73-.73l.5.3A8 8 0 0 1 9 4.57zM7.88 7.64l-.54.51-1.77-1.02-1 1.74 1.76 1.01-.17.73a6.02 6.02 0 0 0 0 2.78l.17.73-1.76 1.01 1 1.74 1.77-1.02.54.51a6 6 0 0 0 2.4 1.4l.72.2V20h2v-2.04l.71-.2a6 6 0 0 0 2.41-1.4l.54-.51 1.77 1.02 1-1.74-1.76-1.01.17-.73a6.02 6.02 0 0 0 0-2.78l-.17-.73 1.76-1.01-1-1.74-1.77 1.02-.54-.51a6 6 0 0 0-2.4-1.4l-.72-.2V4h-2v2.04l-.71.2a6 6 0 0 0-2.41 1.4zM12 16a4 4 0 1 1 0-8 4 4 0 0 1 0 8zm0-2a2 2 0 1 0 0-4 2 2 0 0 0 0 4z"></path>';
  innerHTML +=   '</span>';
  innerHTML += '</span>';
  /* eslint-enable indent, no-multi-spaces */

  domOutput.body.firstChild.innerHTML = innerHTML;

  return domOutput.body.innerHTML;
}

window.nextgenEditor.addHook('hookMarkdowntoHTML', {
  weight: -50,
  async handler(options, input) {
    let output = input;

    const items = [...output.matchAll(/\[plugin:(?<type>page|content)-inject\]\((?<route>[^?)]+)(?<query>\?[^)]*)?\)/g)];

    const body = new FormData();
    const reqUrl = `${window.GravAdmin.config.base_url_relative}/task:pageInjectData`;

    body.append('admin-nonce', window.GravAdmin.config.admin_nonce);
    items.forEach((matches) => {
      body.append('routes[]', matches.groups.route);
    });

    if (!items.length) {
      body.append('routes[]', 'not_exists_route');
    }

    const resp = await fetch(reqUrl, { body, method: 'POST' })
      .then((resp) => (resp.ok ? resp.json() : null))
      .then((resp) => (resp && resp.status !== 'error' ? resp : {}));

    availableTemplates = resp.available_templates;

    const pages = resp.data
      .filter((page) => page.status === 'success')
      .reduce((acc, page) => ({ ...acc, [page.data.route]: page.data }), {});

    items.forEach((matches) => {
      const { type, route, query } = matches.groups;
      const template = new URLSearchParams(query).get('template') || '';
      const title = (pages[route] && pages[route].title) || '';

      output = output.replace(matches[0], `[[page-inject type="${type}" title="${title}" route="${route}" template="${template}"]]`);
    });

    return output;
  },
});

window.nextgenEditor.addHook('hookMarkdowntoHTML', {
  weight: 50,
  handler(options, input) {
    let output = input;

    output = output.replace(/(<p>)?(\[\[page-inject(?<attributes>[^\]]*)\]\])(<\/p>)?/g, (...matches) => {
      const { attributes } = matches.pop();
      return uncollapse(`<page-inject${attributes}></page-inject>`);
    });

    return output;
  },
});

window.nextgenEditor.addHook('hookHTMLtoMarkdown', {
  weight: -50,
  handler(options, editor, input) {
    let output = input;

    output = output.replace(/<page-inject[^>]*>(((?!(<\/page-inject>)).)|\n)*<\/page-inject>/g, (...matches) => {
      const domPageInject = new DOMParser().parseFromString(matches[0], 'text/html').body.firstChild;

      const type = domPageInject.getAttribute('type');
      const route = domPageInject.getAttribute('route');
      const template = domPageInject.getAttribute('template');

      const queryString = template
        ? `?template=${template}`
        : '';

      return `[plugin:${type}-inject](${route}${queryString})`;
    });

    return output;
  },
});

class GravPageInjectCommand extends Command {
  execute(params) {
    showPagePicker(params.value, (page) => {
      const dataPageInject = uncollapse(`<page-inject type="${params.type}" title="${page.name}" route="${page.value}" template=""></page-inject>`);
      const viewPageInject = this.editor.data.processor.toView(dataPageInject).getChild(0);
      const modelPageInject = this.editor.data.toModel(viewPageInject).getChild(0);

      const selectedBlocks = [...this.editor.model.document.selection.getSelectedBlocks()];
      const lastBlock = selectedBlocks[selectedBlocks.length - 1];

      this.editor.model.change((modelWriter) => {
        let insertPosition = modelWriter.createPositionAfter(lastBlock);

        if (lastBlock && lastBlock.name === 'paragraph' && lastBlock.childCount === 0) {
          insertPosition = modelWriter.createPositionBefore(lastBlock);
          modelWriter.remove(lastBlock);
        }

        modelWriter.insert(modelPageInject, insertPosition);
        modelWriter.setSelection(modelPageInject, 'on');
      });
    });
  }
}

window.nextgenEditor.addPlugin('GravPageInject', {
  requires: [Widget],
  init() {
    this.editor.commands.add('page-inject', new GravPageInjectCommand(this.editor));

    this.editor.model.schema.register('page-inject', {
      isObject: true,
      isInline: true,
      allowWhere: '$text',
      allowContentOf: '$block',
      allowAttributes: [
        'type',
        'title',
        'route',
        'template',
      ],
    });

    this.editor.conversion.for('upcast').elementToElement({
      view: 'page-inject',
      model(viewElement, { writer }) {
        return writer.createElement('page-inject', viewElement.getAttributes());
      },
    });

    this.editor.conversion.for('dataDowncast').elementToElement({
      model: 'page-inject',
      view(modelElement, { writer }) {
        return writer.createContainerElement('page-inject', modelElement.getAttributes());
      },
    });

    this.editor.conversion.for('editingDowncast').elementToElement({
      model: 'page-inject',
      view(modelElement, { writer }) {
        const container = writer.createContainerElement('page-inject', modelElement.getAttributes());
        return toWidget(container, writer);
      },
    });
  },
});

window.pageInjectRouteSettings = function pageInjectRouteSettings() {
  const { editor } = window.nextgenEditor;

  const domPageInject = this.closest('page-inject');
  const viewPageInject = editor.editing.view.domConverter.mapDomToView(domPageInject);
  const modelPageInject = editor.editing.mapper.toModelElement(viewPageInject);
  const route = modelPageInject.getAttribute('route');

  showPagePicker(route, (page) => {
    if (page.value === route) {
      return;
    }

    editor.model.change((modelWriter) => {
      const attributes = [...modelPageInject.getAttributes()]
        .reduce((acc, pair) => ({ ...acc, [pair.shift()]: pair.pop() }), {});

      const dataNewPageInject = uncollapse(`<page-inject type="${attributes.type}" title="${page.name}" route="${page.value}" template="${attributes.template}"></page-inject>`);
      const viewNewPageInject = editor.data.processor.toView(dataNewPageInject).getChild(0);
      const modelNewPageInject = editor.data.toModel(viewNewPageInject, '$block').getChild(0);
      const insertPosition = modelWriter.createPositionBefore(modelPageInject);

      modelWriter.remove(modelPageInject);
      modelWriter.insert(modelNewPageInject, insertPosition);
      modelWriter.setSelection(modelNewPageInject, 'on');
    });
  });
};

window.pageInjectSettings = function pageInjectSettings() {
  const { editor } = window.nextgenEditor;

  const domPageInject = this.closest('page-inject');
  const viewPageInject = editor.editing.view.domConverter.mapDomToView(domPageInject);
  let modelPageInject = editor.editing.mapper.toModelElement(viewPageInject);

  const currentAttributes = [...modelPageInject.getAttributes()]
    .reduce((acc, pair) => ({ ...acc, [pair.shift()]: pair.pop() }), {});

  const attributes = {
    type: {
      title: 'Type',
      widget: {
        type: 'select',
        values: Object.keys(itemTypes).map((value) => ({ value, label: itemTypes[value] })),
      },
    },
    template: {
      title: 'Template',
      widget: {
        type: 'input-text',
        // type: 'select',
        // values: [
        //   { value: '', label: '' },
        //   ...Object.keys(availableTemplates).map((value) => ({ value, label: availableTemplates[value] })),
        // ],
        visible: ({ attributes }) => attributes.type === 'page',
      },
    },
  };

  const argsForPopup = {
    title: 'Page Inject',
    domDisplayPoint: this,
    debounceDelay: 1000,
    attributes,
    currentAttributes,
  };

  argsForPopup.deleteItem = () => {
    editor.model.change((modelWriter) => modelWriter.remove(modelPageInject));
  };

  argsForPopup.changeAttributes = (changeCallback) => {
    editor.model.change((modelWriter) => {
      const dataNewPageInject = uncollapse(`<page-inject type="${currentAttributes.type}" title="${currentAttributes.title}" route="${currentAttributes.route}" template="${currentAttributes.template}"></page-inject>`);
      const viewNewPageInject = editor.data.processor.toView(dataNewPageInject).getChild(0);
      const modelNewPageInject = editor.data.toModel(viewNewPageInject, '$block').getChild(0);
      const insertPosition = modelWriter.createPositionBefore(modelPageInject);

      modelWriter.remove(modelPageInject);
      modelWriter.insert(modelNewPageInject, insertPosition);
      modelWriter.setSelection(modelNewPageInject, 'on');

      modelPageInject = modelNewPageInject;
    });

    if (currentAttributes.type !== 'page' && currentAttributes.template) {
      currentAttributes.template = '';
      changeCallback();
    }
  };

  showSettingsPopup(argsForPopup);
};

})();
