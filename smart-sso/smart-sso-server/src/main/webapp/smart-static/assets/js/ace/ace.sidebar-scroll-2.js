/**
 <b>Scrollbars for sidebar</b>. This approach can be used on fixed or normal sidebar.
 It uses <u>"overflow:hidden"</u> so you can't use <u>.hover</u> submenus and it will be disabled when sidebar is minimized.
 It may also be slightly faster especially when resizing browser window.
 Needs some work! Has a few issues!
*/

(function($ , undefined) {
	//if( !$.fn.ace_scroll ) return;

	var old_safari = ace.vars['safari'] && navigator.userAgent.match(/version\/[1-5]/i)
	//NOTE
	//Safari on windows has not been updated for a long time.
	//And it has a problem when sidebar is fixed&scrollable and there is a CSS3 animation inside page content.
	//Very probably windows users of safari have migrated to another browser by now!



	var is_element_pos =
	'getComputedStyle' in window ?
	//el.offsetHeight is used to force redraw and recalculate 'el.style.position' esp. for webkit!
	function(el, pos) { el.offsetHeight; return window.getComputedStyle(el).position == pos }
	:
	function(el, pos) { el.offsetHeight; return $(el).css('position') == pos }

	
	function Sidebar_Scroll(sidebar , settings) {
		var self = this;
		
		var $window = $(window);
		
		var $sidebar = $(sidebar),
			$nav = $sidebar.find('.nav-list'),
			$toggle = $sidebar.find('.sidebar-toggle').eq(0),
			$shortcuts = $sidebar.find('.sidebar-shortcuts').eq(0),

			nav = $nav.get(0);

		if(!nav) return;

		
		var attrib_values = ace.helper.getAttrSettings(sidebar, $.fn.ace_sidebar_scroll.defaults);
		this.settings = $.extend({}, $.fn.ace_sidebar_scroll.defaults, settings, attrib_values);
				
		var scroll_to_active = self.settings.scroll_to_active;
		this.only_if_fixed = self.settings.only_if_fixed;
		

			
		var ace_sidebar = $sidebar.ace_sidebar('ref');
		$sidebar.attr('data-sidebar-scroll', 'true');

		var submenu_hover = function() {
			return $sidebar.first('li.hover > .submenu').css('position') == 'absolute'
		}
		
		
		var scroll_div = null,
			scroll_content = null,
			scroll_content_div = null,
			bar = null,
			ace_scroll = null;

		this.is_scrolling = false;
		var _initiated = false;
		this.sidebar_fixed = is_element_pos(sidebar, 'fixed');

		var $avail_height, $content_height;

		var available_height = function() {
			//available window space
			var offset = $nav.parent().offset();//because `$nav.offset()` considers the "scrolled top" amount as well
			if(self.sidebar_fixed) offset.top -= ace.helper.scrollTop();

			return $window.innerHeight() - offset.top - ( self.settings.include_toggle ? 0 : $toggle.outerHeight() );
		}
		var content_height = function() {
			return nav.scrollHeight;
		}
		
		var initiate = function(on_page_load) {
			if( _initiated ) return;
			if( (self.only_if_fixed && !self.sidebar_fixed) || submenu_hover() ) return;//eligible??
			//return if we want scrollbars only on "fixed" sidebar and sidebar is not "fixed" yet!

			//initiate once
			$nav.wrap('<div class="nav-wrap-up" />');
			if(self.settings.include_shortcuts && $shortcuts.length != 0) $nav.parent().prepend($shortcuts);
			if(self.settings.include_toggle && $toggle.length != 0) $nav.parent().append($toggle);

			scroll_div = $nav.parent()
			.ace_scroll({
				size: available_height(),
				reset: true,
				mouseWheelLock: true,
				lockAnyway: self.settings.lock_anyway,
				styleClass: self.settins.scroll_style,
				hoverReset: false
			})
			.closest('.ace-scroll').addClass('nav-scroll');
			
			ace_scroll = scroll_div.data('ace_scroll');

			scroll_content = scroll_div.find('.scroll-content').eq(0);

			if(old_safari && !self.settings.include_toggle) {
				var toggle = $toggle.get(0);
				if(toggle) scroll_content.on('scroll.safari', function() {
					ace.helper.redraw(toggle);
				});
			}

			_initiated = true;

			//if the active item is not visible, scroll down so that it becomes visible
			//only the first time, on page load
			if(on_page_load == true) {
				self.reset();//try resetting at first

				if( scroll_to_active ) {
					self.scroll_to_active();
				}
				scroll_to_active = false;
			}
		}
		
		
		this.scroll_to_active = function() {
			if( !ace_scroll || !ace_scroll.is_active() ) return;
			try {
				//sometimes there's no active item or not 'offsetTop' property
				var $active;
				
				var vars = ace_sidebar['vars']();

				var nav_list = $sidebar.find('.nav-list')
				if(vars['minimized'] && !vars['collapsible']) {
					$active = nav_list.find('> .active')
				}
				else {
					$active = $nav.find('> .active.hover')
					if($active.length == 0)	$active = $nav.find('.active:not(.open)')
				}

				var top = $active.outerHeight();

				nav_list = nav_list.get(0);
				var active = $active.get(0);
				while(active != nav_list) {
					top += active.offsetTop;
					active = active.parentNode;
				}

				var scroll_amount = top - scroll_div.height();
				if(scroll_amount > 0) {
					scroll_content.scrollTop(scroll_amount);
				}
			}catch(e){}
			
			
			
		}
		
		
		this.reset = function(recalc) {
			if(recalc === true) {
				this.sidebar_fixed = is_element_pos(sidebar, 'fixed');
			}
		
			if( (this.only_if_fixed && !this.sidebar_fixed) || submenu_hover() ) {
				this.disable();
				return;//eligible??
			}
			//return if we want scrollbars only on "fixed" sidebar and sidebar is not "fixed" yet!

			if( !_initiated ) initiate();
			//initiate scrollbars if not yet
			
			$sidebar.addClass('sidebar-scroll');
			
			var vars = ace_sidebar['vars']();
			

			//enable if:
			//menu is not minimized
			//menu is not collapsible mode (responsive navbar-collapse mode which has default browser scroller)
			//menu is not horizontal or horizontal but mobile view (which is not navbar-collapse)
			//and available height is less than nav's height			
			var enable_scroll = !vars['minimized'] && !vars['collapsible'] && !vars['horizontal']
								&& ($avail_height = available_height()) < ($content_height = nav.parentNode.scrollHeight);

			this.is_scrolling = true;
			if( enable_scroll && ace_scroll ) {
				//scroll_content_div.css({height: $content_height, width: 8});
				//scroll_div.prev().css({'max-height' : $avail_height})
				ace_scroll.update({size: $avail_height});
				ace_scroll.enable();
				ace_scroll.reset();
			}
			if( !enable_scroll || !ace_scroll.is_active() ) {
				if(this.is_scrolling) this.disable();
			}
			//return is_scrolling;
		}
		
		this.disable = function() {
			this.is_scrolling = false;
			if(ace_scroll) ace_scroll.disable();
			
			$sidebar.removeClass('sidebar-scroll');
		}
		
		this.prehide = function(height_change) {
			if(!this.is_scrolling || ace_sidebar.get('minimized')) return;//when minimized submenu's toggle should have no effect

			if(content_height() + height_change < available_height()) {
				this.disable();
			}
			else if(height_change < 0) {
				//if content height is decreasing
				//let's move nav down while a submenu is being hidden
				var scroll_top = scroll_content.scrollTop() + height_change
				if(scroll_top < 0) return;

				scroll_content.scrollTop(scroll_top);
			}
		}
		
		this._reset = function(recalc) {
			if(recalc === true) {
				this.sidebar_fixed = is_element_pos(sidebar, 'fixed');
			}
			
			if(ace.vars['webkit']) 
				setTimeout(function() { self.reset() } , 0);
			else this.reset();
		}
		
		this.get = function(name) {
			if(this.hasOwnProperty(name)) return this[name];
		}
		this.set = function(name, value) {
			if(this.hasOwnProperty(name)) this[name] = value;
		}
		this.ref = function() {
			//return a reference to self
			return this;
		}
		
		this.updateStyle = function(styleClass) {
			if(ace_scroll == null) return;
			ace_scroll.update({styleClass: styleClass});
		}
		
		
		//change scrollbar size after a submenu is hidden/shown
		//but don't change if sidebar is minimized
		$sidebar.on('hidden.ace.submenu.sidebar_scroll shown.ace.submenu.sidebar_scroll', '.submenu', function(e) {
			e.stopPropagation();

			if( !ace_sidebar.get('minimized') ) {
				//webkit has a little bit of a glitch!!!
				self._reset();
			}
		})

		initiate(true);//true = on_page_load
	}
		


	//reset on document and window changes
	$(document).on('settings.ace.sidebar_scroll', function(ev, event_name, event_val){
		$('.sidebar[data-sidebar-scroll=true]').each(function() {
			var $this = $(this);
			var sidebar_scroll = $this.ace_sidebar_scroll('ref');

			if( event_name == 'sidebar_collapsed' ) {
				if( $this.attr('data-sidebar-hover') == 'true' ) $this.ace_sidebar_hover('reset');
			
				if(event_val == true) sidebar_scroll.disable();//disable scroll if collapsed
				else sidebar_scroll.reset();
			}
			else if( event_name === 'sidebar_fixed' || event_name === 'navbar_fixed' ) {
				var is_scrolling = sidebar_scroll.get('is_scrolling');
				var sidebar_fixed = is_element_pos(this, 'fixed')
				sidebar_scroll.set('sidebar_fixed', sidebar_fixed);

				if(sidebar_fixed && !is_scrolling) {
					sidebar_scroll.reset();
				}
				else if( !sidebar_fixed  && sidebar_scroll.get('only_if_fixed') ) {
					sidebar_scroll.disable();
				}
			}		
		})
	})

	$(window).on('resize.ace.sidebar_scroll', function(){
		$('.sidebar[data-sidebar-scroll=true]').each(function() {
			var sidebar_scroll = $(this).ace_sidebar_scroll('ref');
			
			var sidebar_fixed = is_element_pos(this, 'fixed')
			sidebar_scroll.set('sidebar_fixed', sidebar_fixed);
			sidebar_scroll.reset();
		});
	})
	
	
	
	/////////////////////////////////////////////
	if(!$.fn.ace_sidebar_scroll) {
	 $.fn.ace_sidebar_scroll = function (option, value) {
		var method_call;

		var $set = this.each(function () {
			var $this = $(this);
			var data = $this.data('ace_sidebar_scroll');
			var options = typeof option === 'object' && option;

			if (!data) $this.data('ace_sidebar_scroll', (data = new Sidebar_Scroll(this, options)));
			if (typeof option === 'string' && typeof data[option] === 'function') {
				method_call = data[option](value);
			}
		});

		return (method_call === undefined) ? $set : method_call;
	 }
	 
	  $.fn.ace_sidebar_scroll.defaults = {
		'scroll_to_active': true,
		'include_shortcuts': true,
		'include_toggle': false,
		'scroll_style': '',
		'lock_anyway': false,
		'only_if_fixed': true
     }
	 
	}

})(window.jQuery);