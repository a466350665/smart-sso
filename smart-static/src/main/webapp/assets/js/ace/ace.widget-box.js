/**
 <b>Widget boxes</b>
*/
(function($ , undefined) {

	var Widget_Box = function(box, options) {
		this.$box = $(box);
		var that = this;
		//this.options = $.extend({}, $.fn.widget_box.defaults, options);

		this.reload = function() {
			var $box = this.$box;
			var $remove_position = false;
			if($box.css('position') == 'static') {
				$remove_position = true;
				$box.addClass('position-relative');
			}
			$box.append('<div class="widget-box-overlay"><i class="'+ ace.vars['icon'] + 'loading-icon fa fa-spinner fa-spin fa-2x white"></i></div>');

			$box.one('reloaded.ace.widget', function() {
				$box.find('.widget-box-overlay').remove();
				if($remove_position) $box.removeClass('position-relative');
			});
		}

		this.close = function() {
			var $box = this.$box;
			var closeSpeed = 300;
			$box.fadeOut(closeSpeed , function(){
					$box.trigger('closed.ace.widget');
					$box.remove();
				}
			)
		}
		
		this.toggle = function(type, button) {
			var $box = this.$box;
			var $body = $box.find('.widget-body').eq(0);
			var $icon = null;
			
			var event_name = typeof type !== 'undefined' ? type : ($box.hasClass('collapsed') ? 'show' : 'hide');
			var event_complete_name = event_name == 'show' ? 'shown' : 'hidden';

			if(typeof button === 'undefined') {
				button = $box.find('> .widget-header a[data-action=collapse]').eq(0);
				if(button.length == 0) button = null;
			}

			if(button) {
				$icon = button.find(ace.vars['.icon']).eq(0);

				var $match
				var $icon_down = null
				var $icon_up = null
				if( ($icon_down = $icon.attr('data-icon-show')) ) {
					$icon_up = $icon.attr('data-icon-hide')
				}
				else if( $match = $icon.attr('class').match(/fa\-(.*)\-(up|down)/) ) {
					$icon_down = 'fa-'+$match[1]+'-down'
					$icon_up = 'fa-'+$match[1]+'-up'
				}
			}

			var expandSpeed   = 250;
			var collapseSpeed = 200;

			if( event_name == 'show' ) {
				if($icon) $icon.removeClass($icon_down).addClass($icon_up);

				$body.hide();
				$box.removeClass('collapsed');
				$body.slideDown(expandSpeed, function(){
					$box.trigger(event_complete_name+'.ace.widget')
				})
			}
			else {
				if($icon) $icon.removeClass($icon_up).addClass($icon_down);
				$body.slideUp(collapseSpeed, function(){
						$box.addClass('collapsed')
						$box.trigger(event_complete_name+'.ace.widget')
					}
				);
			}
		}
		
		this.hide = function() {
			this.toggle('hide');
		}
		this.show = function() {
			this.toggle('show');
		}
		
		
		this.fullscreen = function() {
			var $icon = this.$box.find('> .widget-header a[data-action=fullscreen]').find(ace.vars['.icon']).eq(0);
			var $icon_expand = null
			var $icon_compress = null
			if( ($icon_expand = $icon.attr('data-icon1')) ) {
				$icon_compress = $icon.attr('data-icon2')
			}
			else {
				$icon_expand = 'fa-expand';
				$icon_compress = 'fa-compress';
			}
			
			
			if(!this.$box.hasClass('fullscreen')) {
				$icon.removeClass($icon_expand).addClass($icon_compress);
				this.$box.addClass('fullscreen');
				
				applyScrollbars(this.$box, true);
			}
			else {
				$icon.addClass($icon_expand).removeClass($icon_compress);
				this.$box.removeClass('fullscreen');
				
				applyScrollbars(this.$box, false);
			}
			
			this.$box.trigger('fullscreened.ace.widget')
		}

	}
	
	$.fn.widget_box = function (option, value) {
		var method_call;

		var $set = this.each(function () {
			var $this = $(this);
			var data = $this.data('widget_box');
			var options = typeof option === 'object' && option;

			if (!data) $this.data('widget_box', (data = new Widget_Box(this, options)));
			if (typeof option === 'string') method_call = data[option](value);
		});

		return (method_call === undefined) ? $set : method_call;
	};


	$(document).on('click.ace.widget', '.widget-header a[data-action]', function (ev) {
		ev.preventDefault();

		var $this = $(this);
		var $box = $this.closest('.widget-box');
		if( $box.length == 0 || $box.hasClass('ui-sortable-helper') ) return;

		var $widget_box = $box.data('widget_box');
		if (!$widget_box) {
			$box.data('widget_box', ($widget_box = new Widget_Box($box.get(0))));
		}

		var $action = $this.data('action');
		if($action == 'collapse') {
			var event_name = $box.hasClass('collapsed') ? 'show' : 'hide';

			var event
			$box.trigger(event = $.Event(event_name+'.ace.widget'))
			if (event.isDefaultPrevented()) return

			$widget_box.toggle(event_name, $this);
		}
		else if($action == 'close') {
			var event
			$box.trigger(event = $.Event('close.ace.widget'))
			if (event.isDefaultPrevented()) return

			$widget_box.close();
		}
		else if($action == 'reload') {
			$this.blur();
			var event
			$box.trigger(event = $.Event('reload.ace.widget'))
			if (event.isDefaultPrevented()) return

			$widget_box.reload();
		}
		else if($action == 'fullscreen') {
			var event
			$box.trigger(event = $.Event('fullscreen.ace.widget'))
			if (event.isDefaultPrevented()) return
		
			$widget_box.fullscreen();
		}
		else if($action == 'settings') {
			$box.trigger('setting.ace.widget')
		}

	});
	
		
	function applyScrollbars($widget, enable) {
		var $main = $widget.find('.widget-main').eq(0);
		$(window).off('resize.widget.scroll');
		
		//IE8 has an unresolvable issue!!! re-scrollbaring with unknown values?!
		var nativeScrollbars = ace.vars['old_ie'] || ace.vars['touch'];
		
		if(enable) {
			var ace_scroll = $main.data('ace_scroll');
			if( ace_scroll ) {
				$main.data('save_scroll', {size: ace_scroll['size'], lock: ace_scroll['lock'], lock_anyway: ace_scroll['lock_anyway']});
			}
			
			var size = $widget.height() - $widget.find('.widget-header').height() - 10;//extra paddings
			size = parseInt(size);
			
			$main.css('min-height', size);
			if( !nativeScrollbars ) {
				if( ace_scroll ) {
					$main.ace_scroll('update', {'size': size, 'mouseWheelLock': true, 'lockAnyway': true});
				}
				else {
					$main.ace_scroll({'size': size, 'mouseWheelLock': true, 'lockAnyway': true});
				}
				$main.ace_scroll('enable').ace_scroll('reset');
			}
			else {
				if( ace_scroll ) $main.ace_scroll('disable');
				$main.css('max-height', size).addClass('overflow-scroll');
			}
			
			
			$(window)
			.on('resize.widget.scroll', function() {
				var size = $widget.height() - $widget.find('.widget-header').height() - 10;//extra paddings
				size = parseInt(size);
				
				$main.css('min-height', size);
				if( !nativeScrollbars ) {
					$main.ace_scroll('update', {'size': size}).ace_scroll('reset');
				}
				else {
					$main.css('max-height', size).addClass('overflow-scroll');
				}
			});
		}
		
		else  {
			$main.css('min-height', '');
			var saved_scroll = $main.data('save_scroll');
			if(saved_scroll) {
				$main
				.ace_scroll('update', {'size': saved_scroll['size'], 'mouseWheelLock': saved_scroll['lock'], 'lockAnyway': saved_scroll['lock_anyway']})
				.ace_scroll('enable')
				.ace_scroll('reset');
			}
			
			if( !nativeScrollbars ) {				
				if(!saved_scroll) $main.ace_scroll('disable');				
			}
			else {
				$main.css('max-height', '').removeClass('overflow-scroll');
			}
		}
	}

})(window.jQuery);