/**
 <b>Scrollbars for sidebar</b>. This approach can <span class="text-danger">only</span> be used on <u>fixed</u> sidebar.
 It doesn't use <u>"overflow:hidden"</u> CSS property and therefore can be used with <u>.hover</u> submenus and minimized sidebar.
 Except when in mobile view and menu toggle button is not in the navbar.
*/

(function($ , undefined) {
	//if( !$.fn.ace_scroll ) return;

	var old_safari = ace.vars['safari'] && navigator.userAgent.match(/version\/[1-5]/i)
	//NOTE
	//Safari on windows has not been updated for a long time.
	//And it has a problem when sidebar is fixed & scrollable and there is a CSS3 animation inside page content.
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
			$shortcuts = $sidebar.find('.sidebar-shortcuts').eq(0);
			
		var nav = $nav.get(0);
		if(!nav) return;
		
		
		var attrib_values = ace.helper.getAttrSettings(sidebar, $.fn.ace_sidebar_scroll.defaults);
		this.settings = $.extend({}, $.fn.ace_sidebar_scroll.defaults, settings, attrib_values);
		var scroll_to_active = self.settings.scroll_to_active;
	
	
		var ace_sidebar = $sidebar.ace_sidebar('ref');
		$sidebar.attr('data-sidebar-scroll', 'true');
			
		
		var scroll_div = null,
			scroll_content = null,
			scroll_content_div = null,
			bar = null,
			track = null,
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
			return nav.clientHeight;//we don't use nav.scrollHeight here, because hover submenus are considered in calculating scrollHeight despite position=absolute!
		}

		
		
		var initiate = function(on_page_load) {
			if( _initiated ) return;
			if( !self.sidebar_fixed ) return;//eligible??
			//return if we want scrollbars only on "fixed" sidebar and sidebar is not "fixed" yet!

			//initiate once
			$nav.wrap('<div class="nav-wrap-up pos-rel" />');
			$nav.after('<div><div></div></div>');

			$nav.wrap('<div class="nav-wrap" />');
			if(!self.settings.include_toggle) $toggle.css({'z-index': 1});
			if(!self.settings.include_shortcuts) $shortcuts.css({'z-index': 99});

			scroll_div = $nav.parent().next()
			.ace_scroll({
				size: available_height(),
				//reset: true,
				mouseWheelLock: true,
				hoverReset: false,
				dragEvent: true,
				styleClass: self.settings.scroll_style,
				touchDrag: false//disable touch drag event on scrollbars, we'll add a custom one later
			})
			.closest('.ace-scroll').addClass('nav-scroll');
			
			ace_scroll = scroll_div.data('ace_scroll');

			scroll_content = scroll_div.find('.scroll-content').eq(0);
			scroll_content_div = scroll_content.find(' > div').eq(0);
			
			track = $(ace_scroll.get_track());
			bar = track.find('.scroll-bar').eq(0);

			if(self.settings.include_shortcuts && $shortcuts.length != 0) {
				$nav.parent().prepend($shortcuts).wrapInner('<div />');
				$nav = $nav.parent();
			}
			if(self.settings.include_toggle && $toggle.length != 0) {
				$nav.append($toggle);
				$nav.closest('.nav-wrap').addClass('nav-wrap-t');//it just helps to remove toggle button's top border and restore li:last-child's bottom border
			}

			$nav.css({position: 'relative'});
			if( self.settings.scroll_outside == true ) scroll_div.addClass('scrollout');
			
			nav = $nav.get(0);
			nav.style.top = 0;
			scroll_content.on('scroll.nav', function() {
				nav.style.top = (-1 * this.scrollTop) + 'px';
			});
			
			//mousewheel library available?
			$nav.on(!!$.event.special.mousewheel ? 'mousewheel.ace_scroll' : 'mousewheel.ace_scroll DOMMouseScroll.ace_scroll', function(event){
				if( !self.is_scrolling || !ace_scroll.is_active() ) {
					return !self.settings.lock_anyway;
				}
				//transfer $nav's mousewheel event to scrollbars
				return scroll_div.trigger(event);
			});
			
			$nav.on('mouseenter.ace_scroll', function() {
				track.addClass('scroll-hover');
			}).on('mouseleave.ace_scroll', function() {
				track.removeClass('scroll-hover');
			});


			/**
			$(document.body).on('touchmove.nav', function(event) {
				if( self.is_scrolling && $.contains(sidebar, event.target) ) {
					event.preventDefault();
					return false;
				}
			})
			*/

			//you can also use swipe event in a similar way //swipe.nav
			var content = scroll_content.get(0);
			$nav.on('ace_drag.nav', function(event) {
				if( !self.is_scrolling || !ace_scroll.is_active() ) {
					event.retval.cancel = true;
					return;
				}
				
				//if submenu hover is being scrolled let's cancel sidebar scroll!
				if( $(event.target).closest('.can-scroll').length != 0 ) {
					event.retval.cancel = true;
					return;
				}

				if(event.direction == 'up' || event.direction == 'down') {
					
					ace_scroll.move_bar(true);
					
					var distance = event.dy;
					
					distance = parseInt(Math.min($avail_height, distance))
					if(Math.abs(distance) > 2) distance = distance * 2;
					
					if(distance != 0) {
						content.scrollTop = content.scrollTop + distance;
						nav.style.top = (-1 * content.scrollTop) + 'px';
					}
				}
			});
			

			//for drag only
			if(self.settings.smooth_scroll) {
				$nav
				.on('touchstart.nav MSPointerDown.nav pointerdown.nav', function(event) {
					$nav.css('transition-property', 'none');
					bar.css('transition-property', 'none');
				})
				.on('touchend.nav touchcancel.nav MSPointerUp.nav MSPointerCancel.nav pointerup.nav pointercancel.nav', function(event) {
					$nav.css('transition-property', 'top');
					bar.css('transition-property', 'top');
				});
			}
			
			

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
			
			
			
			if( typeof self.settings.smooth_scroll === 'number' && self.settings.smooth_scroll > 0) {
				$nav.css({'transition-property': 'top', 'transition-duration': (self.settings.smooth_scroll / 1000).toFixed(2)+'s'})
				bar.css({'transition-property': 'top', 'transition-duration': (self.settings.smooth_scroll / 1500).toFixed(2)+'s'})
				
				scroll_div
				.on('drag.start', function(e) {
					e.stopPropagation();
					$nav.css('transition-property', 'none')
				})
				.on('drag.end', function(e) {
					e.stopPropagation();
					$nav.css('transition-property', 'top')
				});
			}
			
			if(ace.vars['android']) {
				//force hide address bar, because its changes don't trigger window resize and become kinda ugly
				var val = ace.helper.scrollTop();
				if(val < 2) {
					window.scrollTo( val, 0 );
					setTimeout( function() {
						self.reset();
					}, 20 );
				}
				
				var last_height = ace.helper.winHeight() , new_height;
				$(window).on('scroll.ace_scroll', function() {
					if(self.is_scrolling && ace_scroll.is_active()) {
						new_height = ace.helper.winHeight();
						if(new_height != last_height) {
							last_height = new_height;
							self.reset();
						}
					}
				});
			}
		}
		
		
		
		
		this.scroll_to_active = function() {
			if( !ace_scroll || !ace_scroll.is_active() ) return;
			try {
				//sometimes there's no active item or not 'offsetTop' property
				var $active;
				
				var vars = ace_sidebar['vars']()

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
					nav.style.top = -scroll_amount + 'px';
					scroll_content.scrollTop(scroll_amount);
				}
			}catch(e){}
		}
		
		
		
		this.reset = function(recalc) {
			if(recalc === true) {
				this.sidebar_fixed = is_element_pos(sidebar, 'fixed');
			}
			
			if( !this.sidebar_fixed ) {
				this.disable();
				return;//eligible??
			}

			//return if we want scrollbars only on "fixed" sidebar and sidebar is not "fixed" yet!

			if( !_initiated ) initiate();
			//initiate scrollbars if not yet
			
			var vars = ace_sidebar['vars']();
			

			//enable if:
			//menu is not collapsible mode (responsive navbar-collapse mode which has default browser scrollbar)
			//menu is not horizontal or horizontal but mobile view (which is not navbar-collapse)
			//and available height is less than nav's height
			

			var enable_scroll = !vars['collapsible'] && !vars['horizontal']
								&& ($avail_height = available_height()) < ($content_height = nav.clientHeight);
								//we don't use nav.scrollHeight here, because hover submenus are considered in calculating scrollHeight despite position=absolute!

								
			this.is_scrolling = true;
			if( enable_scroll ) {
				scroll_content_div.css({height: $content_height, width: 8});
				scroll_div.prev().css({'max-height' : $avail_height})
				ace_scroll.update({size: $avail_height})
				ace_scroll.enable();
				ace_scroll.reset();
			}
			if( !enable_scroll || !ace_scroll.is_active() ) {
				if(this.is_scrolling) this.disable();
			}
			else {
				$sidebar.addClass('sidebar-scroll');
			}
			
			//return this.is_scrolling;
		}
		
		
		
		this.disable = function() {
			this.is_scrolling = false;
			if(scroll_div) {
				scroll_div.css({'height' : '', 'max-height' : ''});
				scroll_content_div.css({height: '', width: ''});//otherwise it will have height and takes up some space even when invisible
				scroll_div.prev().css({'max-height' : ''})
				ace_scroll.disable();
			}

			if(parseInt(nav.style.top) < 0 && self.settings.smooth_scroll && $.support.transition.end) {
				$nav.one($.support.transition.end, function() {
					$sidebar.removeClass('sidebar-scroll');
					$nav.off('.trans');
				});
			} else {
				$sidebar.removeClass('sidebar-scroll');
			}

			nav.style.top = 0;
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

				nav.style.top = (-1 * scroll_top) + 'px';
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
		
		
		this.set_hover = function() {
			if(track) track.addClass('scroll-hover');
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
				if( e.type == 'shown' ) self.set_hover();
			}
		});

		
		initiate(true);//true = on_page_load
	}
	

	
	//reset on document and window changes
	$(document).on('settings.ace.sidebar_scroll', function(ev, event_name, event_val){
		$('.sidebar[data-sidebar-scroll=true]').each(function() {
			var $this = $(this);
			var sidebar_scroll = $this.ace_sidebar_scroll('ref');

			if( event_name == 'sidebar_collapsed' && is_element_pos(this, 'fixed') ) {
				if( $this.attr('data-sidebar-hover') == 'true' ) $this.ace_sidebar_hover('reset');
				sidebar_scroll._reset();
			}
			else if( event_name === 'sidebar_fixed' || event_name === 'navbar_fixed' ) {
				var is_scrolling = sidebar_scroll.get('is_scrolling');
				var sidebar_fixed = is_element_pos(this, 'fixed')
				sidebar_scroll.set('sidebar_fixed', sidebar_fixed);

				if(sidebar_fixed && !is_scrolling) {
					sidebar_scroll._reset();
				}
				else if( !sidebar_fixed ) {
					sidebar_scroll.disable();
				}
			}
		
		});
	});
	
	$(window).on('resize.ace.sidebar_scroll', function(){
		$('.sidebar[data-sidebar-scroll=true]').each(function() {
			var $this = $(this);
			if( $this.attr('data-sidebar-hover') == 'true' ) $this.ace_sidebar_hover('reset');
			/////////////
			var sidebar_scroll = $(this).ace_sidebar_scroll('ref');
			
			var sidebar_fixed = is_element_pos(this, 'fixed')
			sidebar_scroll.set('sidebar_fixed', sidebar_fixed);
			sidebar_scroll._reset();
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
		'smooth_scroll': 150,
		'scroll_outside': false,
		'scroll_style': '',
		'lock_anyway': false
     }
	 
	}

})(window.jQuery);