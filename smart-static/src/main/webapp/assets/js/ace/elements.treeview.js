/**
 <b>Treeview</b>. A wrapper for FuelUX treeview element.
 It's just a wrapper so you still need to include FuelUX treeview script first.
*/
(function($ , undefined) {

	$.fn.aceTree = $.fn.ace_tree = function(options) {
		var $defaults = {
			'open-icon' : ace.vars['icon'] + 'fa fa-folder-open',
			'close-icon' : ace.vars['icon'] + 'fa fa-folder',
			'selectable' : true,
			'selected-icon' : ace.vars['icon'] + 'fa fa-check',
			'unselected-icon' : ace.vars['icon'] + 'fa fa-times',
			'loadingHTML': 'Loading...'
		}

		this.each(function() {
		
			var attrib_values = ace.helper.getAttrSettings(this, $defaults);
			var $options = $.extend({}, $defaults, options, attrib_values);

			var $this = $(this);
			$this.addClass('tree').attr('role', 'tree');
			$this.html(
			'<li class="tree-branch hide" data-template="treebranch" role="treeitem" aria-expanded="false">\
				<div class="tree-branch-header">\
					<span class="tree-branch-name">\
						<i class="icon-folder '+$options['close-icon']+'"></i>\
						<span class="tree-label"></span>\
					</span>\
				</div>\
				<ul class="tree-branch-children" role="group"></ul>\
				<div class="tree-loader" role="alert">'+$options['loadingHTML']+'</div>\
			</div>\
			<li class="tree-item hide" data-template="treeitem" role="treeitem">\
				<span class="tree-item-name">\
				  '+($options['unselected-icon'] == null ? '' : '<i class="icon-item '+$options['unselected-icon']+'"></i>')+'\
				  <span class="tree-label"></span>\
				</span>\
			</li>');
			
			$this.addClass($options['selectable'] == true ? 'tree-selectable' : 'tree-unselectable');
			
			$this.tree($options);
		});

		return this;
	}

})(window.jQuery);
