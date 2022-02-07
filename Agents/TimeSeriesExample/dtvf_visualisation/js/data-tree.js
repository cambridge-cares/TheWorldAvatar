/* ==============
| DATATREE.JS
| @author: Mitya (acroxall@espresso.co.uk)
| @Docs & demo: http://www.mitya.co.uk/scripts/XML-Tree---visualise-and-traverse-your-XML-186
============== */

DataTree = function(params, subTreeRequest) {

	/* ---
	| PREP & VALIDATION
	--- */

	const 	thiss = this,
			treeRenderedDfd = new $.Deferred;

	//ensure was instantiated, not merely called
	if (!(this instanceof DataTree)) {
		debug('DataTree was called but not instantiated');
		return;
	}

	//validate some params
	let error;
	if (!params.fpath && !params.xml && typeof params.json != 'object') error = 'no data or data source URI passed';
	else if ((!params.container || !$(params.container).length) && !params.justReturn) error = "no container selector passed or does not match element in DOM";
	if (error) return debug(error);

	//some vars
	var	li,
		appendTo,
		attrLI,
		container = $(params.container),
		rand = Math.floor(Math.random() * 10000000);

	//establish tree container - if making the outer tree, create a new UL. If this is a sub-tree request, i.e. called by self,
	//merge new tree into existing UL of caller LI
	this.tree = !subTreeRequest ? $('<ul>') : container.children('ul');

	//log this instance of the tree and update global instances tracker
	this.instanceID = DataTree.instancesCounter;
	this.tree.attr('id', 'tree_'+this.instanceID);
	DataTree.instancesCounter++;

	//add a few classes to tree, unless it's a sub-tree (i.e. being inserted later into branch of master tree, in which case it can
	//just inherit master tree's classes
	if (!subTreeRequest) {
		this.tree.addClass('xmltree');
		if (params.class) this.tree.addClass(params['class']);
		if (params.startExpanded) this.tree.addClass('startExpanded');
	}
	
	//and any data?
	if (params.data) for (let i in params.data) this.tree.data(i, params.data[i]);

	//if it is a sub-tree request, add .forcePlusMin to tree (i.e. expanded LI) so plus/min icon of sub-tree shows, doesn't inherit CSS from parent to hide it
	if (subTreeRequest) this.tree.addClass('forcePlusMin');

	//insert master UL, unless just returning tree, not inserting it
	if (!params.justReturn) this.tree.appendTo(container);

	/* ---
	| ACT ON XML (private) - once we have the data, start outputting from it.
	--- */

	function actOnXML(xml, isSubTreeRequest) {

		//establish XML
		if (typeof xml == 'string') {
			let parser = new DOMParser();
			this.xml = parser.parseFromString(xml, 'text/xml');
		} else
			this.xml = xml;

		//if is sub-tree request, we don't want the root, just the items
		if (subTreeRequest) this.xml = this.xml.children[0];

		//perform any XML manipulation rules stipulated
		if (params.XMLCallback) this.xml = params.XMLCallback(this.xml);

		debug('data fed to tree:', this.xml);

		//open the tree at a specific point once output? Log as attribute on the XML node, so later we can spot this and
		//open from that point - https://bit.ly/37t9oTO - https://mzl.la/2SPKauM
		if (params.openAtPath && !isSubTreeRequest) {
			let xpRes = this.xml.evaluate(params.openAtPath, this.xml, null, XPathResult.UNORDERED_NODE_SNAPSHOT_TYPE), itNode;
			debug('openAtPath: found '+xpRes.snapshotLength+' item(s) from following XPath expression:', params.openAtPath);
			for (let i=0; i<xpRes.snapshotLength; i++) {
				let node = xpRes.snapshotItem(i);
				if (node.toString() == '[object Attr]') {
					debug('openAtPath: skipping node '+(i+1)+' as it is an attribute. openAtPath must point to element(s) only. Node was:', node);
					continue;
				};
				node.setAttribute('currSel', 'true');
			}
		}

		//start delving. Since JS seems to add another, outer root element, our (real) root it is child
		$(this.xml).children().each((i, el) => delve.call(thiss, $(el), thiss.tree));

		//if sub-tree, if we ended up with no data, remove tree and also corresponding plus/min. Else show tree.
		if (subTreeRequest) this.tree.show();
		//if (subTreeRequest) this.xml.children().length ? this.tree.show() : this.tree.prev('.plusMin').andSelf().remove();

		//do post-build stuff after delving complete
		postBuild.call(this, !!subTreeRequest);
	}

	/* ---
	| DELVE (private) - for outputting XML as HTML. Called recursively for all levels of tree
	--- */

	function delve(node, appendTo) {

		//what's this node's tag name?
		let tagName = node[0].tagName.replace(new RegExp('_'+rand+'$', 'i'), '').toLowerCase();

		//build LI and sub-UL for this node (note, tagname is applied as class to LI, for easy post-tree traversal)
		let LITxtHolder = $('<span>').addClass('LIText'),
			ul = $('<ul>'),
			li = $('<li>').addClass(tagName).append(LITxtHolder).append(ul).data('node', node);
		node.htmlNode = li;
		appendTo.append(li);

		//plus/mins indicator
		li.append($('<span>', {html: params.startExpanded ? '&#8211;' : '+'}).addClass('plusMin collapsed'));

		//attributes...
		let attrs = node[0].attributes;

		//...add node attributes as classes? If true, all, else if array, only attributes specified in that array
		//For each eligible attribute, two classes are added: attr and attr-value
		if (params.attrsAsClasses) {
			for (let i=0; i<attrs.length; i++)
				if (params.attrsAsClasses === true || (typeof params.attrsAsClasses == 'string' && params.attrsAsClasses == attrs[i].name) || (params.attrsAsClasses instanceof Array && $.inArray(attrs[i].name, params.attrsAsClasses) != -1))
					li.addClass(attrs[i].name+'-'+attrs[i].value+' '+attrs[i].name);
		}

		//...add node attributes as element data? " " " " " " "
		if (params.attrsAsData) {
			for (let i=0; i<attrs.length; i++)
				if (params.attrsAsData === true || (typeof params.attrsAsData == 'string' && params.attrsAsData == attrs[i].name) || (params.attrsAsData instanceof Array && $.inArray(attrs[i].name, params.attrsAsData) != -1))
					li.attr('data-'+attrs[i].name, attrs[i].value);
		}

		//...output attributes as LIs? (yes, no, or yes but hidden)
		if (!params.attrs || params.attrs != 'ignore') {
			if (attrs) {
				for(let i=0; i<attrs.length; i++) {
					if (attrs[i].value) {
						ul.append(attrLI = $('<li>').append($('<span>', {text: attrs[i].value}).addClass('attrValue')).addClass('attr '+attrs[i].name).prepend($('<span>', {text: '@'+attrs[i].name+':'})));
						if (params.attrs && params.attrs == 'hidden') attrLI.hide();
					}
				}
			}
		} else
			attrs = false;

		//node has children? (for current purposes, attributes are considered children). If contains only attributes, and params.attrs
		//== 'hidden', count as having no kids
		let kids = node.children();
		if (!kids.length && (!attrs.length || (attrs.length && params.attrs && params.attrs == 'hidden'))) li.addClass('noKids');

		//span to show node name
		tagName = $('<span>', {text: tagName}).addClass('tree_node');

		//if no children, simply append text (if any), otherwise iteratively call self on children
		if (!kids.length)
			LITxtHolder.prepend(node.text()).prepend(tagName);

		//if children, set stored procedures that will run and create them only when branch expanded - unless:
		//starting expanded; LI is a sub-tree node (or all LIs are); opening at an XPath
		else {
			LITxtHolder.prepend(immediateText(node)+(!params.noDots ? '..' : '')).prepend(tagName);
			storedProcedure = ((kids, parent) => { return () => {
				kids.each((i, el) => delve.call(thiss, $(el), parent));
				if (params.renderCallback) params.renderCallback(parent, this, subTreeRequest);
			}; })(kids, ul);
			if (!params.startExpanded && !params.openAtPath && params.subTreeBranches !== true && li.is('.subTreeNode')) {
				li.children('.plusMin').on('click.sp', function() {
					storedProcedure();
					$(this).off('click.sp');
				});
			} else
				storedProcedure();
		}

	}

	/* ---
	| POST BUILD (private) - e.g. click events, any user-defined HTML rules, update hash log in URL etc
	--- */

	function postBuild(isSubTreeRequest) {

		//if doing sub-tree requests, ensure relevent branches always have plus-min icons visible
		if (params.subTreeBranches) {
			if (params.subTreeBranches === true)
				this.tree.addClass('subTreeRequestsOnAllNodes');
			else
				this.tree.find(params.subTreeBranches).addClass('subTreeNode');
		}

		//listen for clicks to expand/collapse nodes.

		this.tree.on('click', '.plusMin', function(evt) {

			//prep
			evt.stopPropagation();
			let uls = $(this).parent().children('ul'),
				currState = uls.filter(':hidden').length || !uls.length ? 'closed' : 'open',
				xPathToNode = returnXPathToNode($(this).parent()),
				li = $(this).parent();
			uls[currState == 'closed' ? 'show' : 'hide']();

			//plus/min click callback? Pass LI, LI's XPath, event obj. and string 'open' or 'close'
			if (params.plusMinCallback) params.plusMinCallback(li, xPathToNode, evt, currState);


			//Sub-tree request on expand? This should be a callback that returns a request URI that will load a sub-tree into
			//the current branch. Callback receives same args as plusMinCallback above. If data previously fetched (denoted
			//by data element on node), ignore.

			if (params.subTreeBranches && (params.subTreeBranches === true || $(this).parent().is('.subTreeNode')) && params.subTreeRequest && currState == 'closed' && (!li.data('subTreeDataFetched') || params.noSubTreeCache)) {
				let subTreeReqURI = params.subTreeRequest(li, xPathToNode, evt, currState);
				if (subTreeReqURI && typeof subTreeReqURI == 'string') {
					let tree = new DataTree($.extend(params, {fpath: subTreeReqURI, container: li}), true);
					if (tree) li.data('subTreeDataFetched', true);
				}
			}

			//flip plus/minus indicator and class
			$(this).html(currState == 'closed' ? '&#8211;' : '+').removeClass('expanded collapsed').addClass(currState == 'closed' ? 'expanded' : 'collapsed');

			//Log curr tree pos in URL hash, made up of comma-sep LI indexes of open ULs (LIs with multiple open ULs are sub-sep by -)
			if (!params.noURLTracking) {
				let paths = [];
				thiss.tree.find('ul:visible').filter(function() { return !$(this).find('ul:visible').length; }).each(function() {
					let thisPathIndecies = [];
					$(this).parents('li').each(function() { thisPathIndecies.unshift($(this).index()); });
					paths.push(thisPathIndecies.join(','));
				});
				location.replace('#tree'+thiss.instanceID+':'+paths.join('|')+';');
			}

		});

		//do callback on click to actual nodes? Pass LI, LI's xPath and event obj.
		if (params.clickCallback)
			this.tree.on('.LIText', 'click', function(evt) {
				let li = $(this).closest('li'); params.clickCallback(li, returnXPathToNode(li), evt);
			});

		//hide attrs if params say so
		if (params.hideAttrs && !params.subTree) this.tree.addClass('hideAttrs');

		//hide node names, if params say so
		if (params.hideNodeNames && !params.subTree) this.tree.addClass('hideNodeNames');

		//render callback?
		if (params.renderCallback) params.renderCallback(this.tree, this, subTreeRequest);

		//onload - re-entry point(s) stipulated in URL hash or in params (@openAtPath)?

		//...stipulated in hash
		let paths = new RegExp('#tree'+this.instanceID+':([0-9,\-\|]+);').exec(location.hash);
		if (paths) {
			let paths = paths[1].split('|');
			for(let y in paths) {
				let parts = paths[y].split(',');
				let selStr = [];
				for(let i in parts) selStr.push('li:eq('+parts[i]+') > ul');
				this.tree.find(selStr.join(' > ')).parents('ul').andSelf().show().each(function() {
					$(this).parent().children('.plusMin').html('-');
				});
			}

		//...stipulated in params
		} else
			this.tree.find('.currSel').parentsUntil('.xmltree').children('.plusMin').trigger('click');

	}

	/* ---
	| ESTABLISH XML - either from file or passed manually. If latter, temporarily rename all tags so any shared
	| names of self-closing HTML tags aren't mullered by jQuery during delving
	--- */

	//from file...
	if (params.fpath) {

		debug('file path URI:', params.fpath);

		//...get data...
		let dataType = !params.jsonp ? (!params.json || typeof params.json == 'object' ? 'xml' : 'json') : 'jsonp';
		$.ajax({
			url: params.fpath,
			type: !params.post ? 'GET' : 'POST',
			data: params.req_data,
			cache: params.cache == undefined ? true : params.cache,
			dataType: dataType
		})
			.error(function() { debug('could not load XML from '+params.fpath); })

			//...success. Establish XML. If params.json, convert JSON respone to XML text then reinitialise
			.done(function(data) {
				if (params.json && typeof params.json != 'object') {
					if (params.jsonCallback) data = params.jsonCallback(data);
					delete params.fpath;
					params.xml = json_to_xml(data);
					return new DataTree(params, subTreeRequest);
				}
				if (params.jsonp) data = decodeURIComponent(data).replace(/\{space\}/g, ' ');
				thiss.xml = data;
				actOnXML.call(thiss, data, !!subTreeRequest);
				treeRenderedDfd.resolve();
			});

	//from passed string (XML)
	} else if (typeof params.xml == 'string') {

		this.xml = params.xml;

		//rename tags
		if (!params.noTagRenaming)
			this.xml = params.xml
				.replace(/<(\/)?(\w+)([^>]*)>/g, (...rpl) => { return '<'+(rpl[1] ? rpl[1] : '')+rpl[2]+'_'+rand+(rpl[3] ? rpl[3] : '')+'>'; })
				.replace(/<\?xml[^>]+>\s*/, '');

		//also strip out entities as they break JS XML parsing
		//this.xml = this.xml.replace(/&amp;|&(?= )/g, 'and').replace(/&\w+;/g, '');
		actOnXML.call(this, params.xml, !!subTreeRequest);
		treeRenderedDfd.resolve();

	//from passed object (JSON)
	} else if (params.json) {
		if (params.jsonCallback) data = params.jsonCallback(data);
		params.xml = json_to_xml(params.json);
		actOnXML.call(this, params.xml, !!subTreeRequest);
		treeRenderedDfd.resolve();
	}

	/* ---
	| PUBLIC API
	--- */

	//jump to - specific branch of tree, corresponding to passed jQuery selector
	this.jumpTo = (selector, closeOthers) => {
		treeRenderedDfd.done(() => {
			if (!selector || typeof selector != 'string') return debug('jumpTo() - @selector not passed or is not string');
			if (closeOthers) this.tree.find('.plusMin.expanded').trigger('click');
			let el = this.tree.find(selector).filter('li');
			if (!el.length) return debug('jumpTo() - no branch (<li>) found matching selector', selector);
			if (el.children('.plusMin').is('.collapsed'))
				el.parentsUntil('.xmltree').andSelf().children('.plusMin.collapsed').trigger('click');
		});
	}

	//get XML node/JS object relating to LI matching selector
	this.getNode = (selector, thoseOpen) => {
		if (!selector || typeof selector != 'string') return debug('getNode() - @selector not passed or is not string');
		let el = this.tree.find(selector).filter('li');
		if (el.length !== 1) return debug('getNode() - @selector returned more than one node', selector);
		return el.data('node')[0];
	}

	/* ---
	| PRIVATE UTILS
	--- */

	//JSON > XML convertor (creates XML string)...
	function json_to_xml(obj, root_name, depth) {
	    
		//...prep
		let xml = '';
		depth = depth || 0;
		root_name = root_name || 'root';
		if (!depth) xml = '<'+root_name+'>';

		//...recurse over passed object (for-in) or array (for)
		if (obj.toString() == '[object Object]') for (let i in obj) xml += build_node(i, obj[i]);
		else if (obj instanceof Array) for (let i=0, len = obj.length; i<len; i++) xml += build_node('node', obj[i]);

		//...build individual XML node. Tags named after object key or, if array, 'node'. Coerce tag name to be valid.
		function build_node(tag_name, val) {
			var
			tag_name = tag_name.replace(/[^\w\-_]/g, '-').replace(/-{2,}/g, '-').replace(/^[^a-z]/, function($0) { return 'node-'+$0; }),
			padder = new Array(depth + 2).join('\t'),
			node = '\n'+padder+'<'+tag_name+'>\n'+padder+'\t';
			if (val) node += typeof val != 'object' ? val : json_to_xml(val, null, depth + 1);
			return node + '\n'+padder+'</'+tag_name+'>\n';
		}

		if (!depth) xml += '</'+root_name+'>';

		//...return XML string, cleaning it up a bit first
		return xml
			.replace(/&(?= )/g, '&amp;')
			.replace(/^\n(?=<)/, '')
			.replace(/\n{2,}/g, '\n')
			.replace(/^\t+\n/mg, '');
	}

	//get immediate text
	function immediateText(node) { return $(node).clone().children().remove().end().text(); }

	//XPath - return XPath of clicked node
	function returnXPathToNode(nodeEl) {
		let path = [];
		nodeEl.parents('li').andSelf().each(function() {
			let nodeName = $(this).children('.LIText').children('.node').text();
			let step = nodeName;
			let index = $(this).prevAll().filter(function() { return $(this).children('.LIText').children('.node').text() == nodeName; }).length + 1;
			if (index > 1) step += '['+index+']'
			path.push(step);
		 });
		return path.join('/');
	}

	//debug
	function debug() { 
		let msgs = Array.from(arguments);
		msgs.unshift('DataTree.js says: ');
		console.debug.apply(null, msgs);
	}

}

//log instances
DataTree.instancesCounter = 0;