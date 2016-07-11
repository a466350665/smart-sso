/**
 <b>Submenu hover adjustment</b>. Automatically move up a submenu to fit into screen when some part of it goes beneath window.
 Pass a "true" value as an argument and submenu will have native browser scrollbars when necessary.
*/

(function($ , undefined) {

 if( ace.vars['very_old_ie'] ) return;
 //ignore IE7 & below
 
 var hasTouch = ace.vars['touch'];
 var nativeScroll = ace.vars['old_ie'] || hasTouch;
 

 var is_element_pos =
	'getComputedStyle' in window ?
	//el.offsetHeight is used to force redraw and recalculate 'el.style.position' esp. for webkit!
	function(el, pos) { el.offsetHeight; return window.getComputedStyle(el).position == pos }
	:
	function(el, pos) { el.offsetHeight; return $(el).css('position') == pos }



 $(window).on('resize.sidebar.ace_hover', function() {
	$('.sidebar[data-sidebar-hover=true]').ace_sidebar_hover('update_vars').ace_sidebar_hover('reset');
 })

 $(document).on('settings.ace.ace_hover', function(e, event_name, event_val) {
	if(event_name == 'sidebar_collapsed') $('.sidebar[data-sidebar-hover=true]').ace_sidebar_hover('reset');
	else if(event_name == 'navbar_fixed') $('.sidebar[data-sidebar-hover=true]').ace_sidebar_hover('update_vars');
 })
 
 var sidebars = [];

 function Sidebar_Hover(sidebar , settings) {
	var self = this, that = this;
	
	var attrib_values = ace.helper.getAttrSettings(sidebar, $.fn.ace_sidebar_hover.defaults);
	this.settings = $.extend({}, $.fn.ace_sidebar_hover.defaults, settings, attrib_values);
	

	var $sidebar = $(sidebar), nav_list = $sidebar.find('.nav-list').get(0);
	$sidebar.attr('data-sidebar-hover', 'true');
	
	sidebars.push($sidebar);

	var sidebar_vars = {};
	var old_ie = ace.vars['old_ie'];

	
	
	var scroll_right = false;
	//scroll style class
	
	if(hasTouch) self.settings.sub_hover_delay = parseInt(Math.max(self.settings.sub_hover_delay, 2500));//for touch device, delay is at least 2.5sec

	var $window = $(window);
	//navbar used for adding extra offset from top when adjusting submenu
	var $navbar = $('.navbar').eq(0);
	var navbar_fixed = $navbar.css('position') == 'fixed';
	this.update_vars = function() {
		navbar_fixed = $navbar.css('position') == 'fixed';
	}

	self.dirty = false;
	//on window resize or sidebar expand/collapse a previously "pulled up" submenu should be reset back to its default position
	//for example if "pulled up" in "responsive-min" mode, in "fullmode" should not remain "pulled up"
	this.reset = function() {
		if( self.dirty == false ) return;
		self.dirty = false;//so don't reset is not called multiple times in a row!
	
		$sidebar.find('.submenu').each(function() {
			var $sub = $(this), li = $sub.parent();
			$sub.css({'top': '', 'bottom': '', 'max-height': ''});
			
			if($sub.hasClass('ace-scroll')) {
				$sub.ace_scroll('disable');
			}
			else {
				$sub.removeClass('sub-scroll');
			}
			 
			if( is_element_pos(this, 'absolute') ) $sub.addClass('can-scroll');
			else $sub.removeClass('can-scroll');

			li.removeClass('pull_up').find('.menu-text:first').css('margin-top', '');
		})

		$sidebar.find('.hover-show').removeClass('hover-show hover-shown hover-flip');
	}
	
	this.updateStyle = function(newStyle) {
		sub_scroll_style = newStyle;
		$sidebar.find('.submenu.ace-scroll').ace_scroll('update', {styleClass: newStyle});
	}
	this.changeDir = function(dir) {
		scroll_right = (dir === 'right');
	}
	
	
	//update submenu scrollbars on submenu hide & show

	var lastScrollHeight = -1;
	//hide scrollbars if it's going to be not needed anymore!
	if(!nativeScroll)
	$sidebar.on('hide.ace.submenu.sidebar_hover', '.submenu', function(e) {
		if(lastScrollHeight < 1) return;

		e.stopPropagation();
		var $sub = $(this).closest('.ace-scroll.can-scroll');
		if($sub.length == 0 || !is_element_pos($sub[0], 'absolute')) return;

		if($sub[0].scrollHeight - this.scrollHeight < lastScrollHeight) {
			$sub.ace_scroll('disable');
		}
	});

	
	
	
	//reset scrollbars 
	if(!nativeScroll)
	$sidebar.on('shown.ace.submenu.sidebar_hover hidden.ace.submenu.sidebar_hover', '.submenu', function(e) {
		if(lastScrollHeight < 1) return;
	
		var $sub = $(this).closest('.ace-scroll.can-scroll');
		if($sub.length == 0 || !is_element_pos($sub[0], 'absolute') ) return;
		
		var sub_h = $sub[0].scrollHeight;
		
		if(lastScrollHeight > 14 && sub_h - lastScrollHeight > 4) {
			$sub.ace_scroll('enable').ace_scroll('reset');//don't update track position
		}
		else {
			$sub.ace_scroll('disable');
		}
	});


	///////////////////////


	var currentScroll = -1;

	//some mobile browsers don't have mouseenter
	var event_1 = !hasTouch ? 'mouseenter.sub_hover' : 'touchstart.sub_hover';// pointerdown.sub_hover';
	var event_2 = !hasTouch ? 'mouseleave.sub_hover' : 'touchend.sub_hover touchcancel.sub_hover';// pointerup.sub_hover pointercancel.sub_hover';
	
	$sidebar.on(event_1, '.nav-list li, .sidebar-shortcuts', function (e) {
		sidebar_vars = $sidebar.ace_sidebar('vars');
		
	
		//ignore if collapsible mode (mobile view .navbar-collapse) so it doesn't trigger submenu movements
		//or return if horizontal but not mobile_view (style 1&3)
		if( sidebar_vars['collapsible'] /**|| sidebar_vars['horizontal']*/ ) return;
		
		var $this = $(this);

		var shortcuts = false;
		var has_hover = $this.hasClass('hover');
		
		var sub = $this.find('> .submenu').get(0);
		if( !(sub || ((this.parentNode == nav_list || has_hover || (shortcuts = $this.hasClass('sidebar-shortcuts'))) /**&& sidebar_vars['minimized']*/)) ) {
			if(sub) $(sub).removeClass('can-scroll');
			return;//include .compact and .hover state as well?
		}
		
		var target_element = sub, is_abs = false;
		if( !target_element && this.parentNode == nav_list ) target_element = $this.find('> a > .menu-text').get(0);
		if( !target_element && shortcuts ) target_element = $this.find('.sidebar-shortcuts-large').get(0);
		if( (!target_element || !(is_abs = is_element_pos(target_element, 'absolute'))) && !has_hover ) {
			if(sub) $(sub).removeClass('can-scroll');
			return;
		}
		
		
		var sub_hide = getSubHide(this);
		//var show_sub = false;

		if(sub) {
		 if(is_abs) {
			self.dirty = true;
			
			var newScroll = ace.helper.scrollTop();
			//if submenu is becoming visible for first time or document has been scrolled, then adjust menu
			if( !sub_hide.is_visible() || (!hasTouch && newScroll != currentScroll) || old_ie ) {
				//try to move/adjust submenu if the parent is a li.hover or if submenu is minimized
				//if( is_element_pos(sub, 'absolute') ) {//for example in small device .hover > .submenu may not be absolute anymore!
					$(sub).addClass('can-scroll');
					//show_sub = true;
					if(!old_ie && !hasTouch) adjust_submenu.call(this, sub);
					else {
						//because ie8 needs some time for submenu to be displayed and real value of sub.scrollHeight be kicked in
						var that = this;
						setTimeout(function() {	adjust_submenu.call(that, sub) }, 0)
					}
				//}
				//else $(sub).removeClass('can-scroll');
			}
			currentScroll = newScroll;
		 }
		 else {
			$(sub).removeClass('can-scroll');
		 }
		}
		//if(show_sub) 
		sub_hide.show();
		
	 }).on(event_2, '.nav-list li, .sidebar-shortcuts', function (e) {
		sidebar_vars = $sidebar.ace_sidebar('vars');
		
		if( sidebar_vars['collapsible'] /**|| sidebar_vars['horizontal']*/ ) return;

		if( !$(this).hasClass('hover-show') ) return;

		getSubHide(this).hideDelay();
	 });
	 
	
	function subHide(li_sub) {
		var self = li_sub, $self = $(self);
		var timer = null;
		var visible = false;
		
		this.show = function() {
			if(timer != null) clearTimeout(timer);
			timer = null;		

			$self.addClass('hover-show hover-shown');
			visible = true;

			//let's hide .hover-show elements that are not .hover-shown anymore (i.e. marked for hiding in hideDelay)
			for(var i = 0; i < sidebars.length ; i++)
			{
			  sidebars[i].find('.hover-show').not('.hover-shown').each(function() {
				getSubHide(this).hide();
			  })
			}
		}
		
		this.hide = function() {
			visible = false;
			
			$self.removeClass('hover-show hover-shown hover-flip');
			
			if(timer != null) clearTimeout(timer);
			timer = null;
			
			var sub = $self.find('> .submenu').get(0);
			if(sub) getSubScroll(sub, 'hide');
		}
		
		this.hideDelay = function(callback) {
			if(timer != null) clearTimeout(timer);
			
			$self.removeClass('hover-shown');//somehow marked for hiding
			
			timer = setTimeout(function() {
				visible = false;
				$self.removeClass('hover-show hover-flip');
				timer = null;
				
				var sub = $self.find('> .submenu').get(0);
				if(sub) getSubScroll(sub, 'hide');
				
				if(typeof callback === 'function') callback.call(this);
			}, that.settings.sub_hover_delay);
		}
		
		this.is_visible = function() {
			return visible;
		}
	}
	function getSubHide(el) {
		var sub_hide = $(el).data('subHide');
		if(!sub_hide) $(el).data('subHide', (sub_hide = new subHide(el)));
		return sub_hide;
	}
	
	
	function getSubScroll(el, func) {
		var sub_scroll = $(el).data('ace_scroll');
		if(!sub_scroll) return false;
		if(typeof func === 'string') {
			sub_scroll[func]();
			return true;
		}
		return sub_scroll;
	}	
	
	function adjust_submenu(sub) {
		var $li = $(this);
		var $sub = $(sub);
		sub.style.top = '';
		sub.style.bottom = '';


		var menu_text = null
		if( sidebar_vars['minimized'] && (menu_text = $li.find('.menu-text').get(0)) ) {
			//2nd level items don't have .menu-text
			menu_text.style.marginTop = '';
		}

		var scroll = ace.helper.scrollTop();
		var navbar_height = 0;

		var $scroll = scroll;
		
		if( navbar_fixed ) {
			navbar_height = sidebar.offsetTop;//$navbar.height();
			$scroll += navbar_height + 1;
			//let's avoid our submenu from going below navbar
			//because of chrome z-index stacking issue and firefox's normal .submenu over fixed .navbar flicker issue
		}




		var off = $li.offset();
		off.top = parseInt(off.top);
		
		var extra = 0, parent_height;
		
		sub.style.maxHeight = '';//otherwise scrollHeight won't be consistent in consecutive calls!?
		var sub_h = sub.scrollHeight;
		var parent_height = $li.height();
		if(menu_text) {
			extra = parent_height;
			off.top += extra;
		}
		var sub_bottom = parseInt(off.top + sub_h)

		var move_up = 0;
		var winh = $window.height();


		//if the bottom of menu is going to go below visible window

		var top_space = parseInt(off.top - $scroll - extra);//available space on top
		var win_space = winh;//available window space
		
		var horizontal = sidebar_vars['horizontal'], horizontal_sub = false;
		if(horizontal && this.parentNode == nav_list) {
			move_up = 0;//don't move up first level submenu in horizontal mode
			off.top += $li.height();
			horizontal_sub = true;//first level submenu
		}

		if(!horizontal_sub && (move_up = (sub_bottom - (winh + scroll))) >= 0 ) {
			//don't move up more than available space
			move_up = move_up < top_space ? move_up : top_space;

			//move it up a bit more if there's empty space
			if(move_up == 0) move_up = 20;
			if(top_space - move_up > 10) {
				move_up += parseInt(Math.min(25, top_space - move_up));
			}


			//move it down if submenu's bottom is going above parent LI
			if(off.top + (parent_height - extra) > (sub_bottom - move_up)) {
				move_up -= (off.top + (parent_height - extra) - (sub_bottom - move_up));
			}

			if(move_up > 0) {
				sub.style.top = -(move_up) + 'px';
				if( menu_text ) {
					menu_text.style.marginTop = -(move_up) + 'px';
				}
			}
		}
		if(move_up < 0) move_up = 0;//when it goes below
		
		var pull_up = move_up > 0 && move_up > parent_height - 20;
		if(pull_up) {
			$li.addClass('pull_up');
		}
		else $li.removeClass('pull_up');
		
		
		//flip submenu if out of window width
		if(horizontal) {
			if($li.parent().parent().hasClass('hover-flip')) $li.addClass('hover-flip');//if a parent is already flipped, flip it then!
			else {
				var sub_off = $sub.offset();
				var sub_w = $sub.width();
				var win_w = $window.width();
				if(sub_off.left + sub_w > win_w) {
					$li.addClass('hover-flip');
				}
			}
		}


		//don't add scrollbars if it contains .hover menus
		var has_hover = $li.hasClass('hover') && !sidebar_vars['mobile_view'];
		if(has_hover && $sub.find('> li > .submenu').length > 0) return;

	
		//if(  ) {
			var scroll_height = (win_space - (off.top - scroll)) + (move_up);
			//if after scroll, the submenu is above parent LI, then move it down
			var tmp = move_up - scroll_height;
			if(tmp > 0 && tmp < parent_height) scroll_height += parseInt(Math.max(parent_height, parent_height - tmp));

			scroll_height -= 5;
			
			if(scroll_height < 90) {
				return;
			}
			
			var ace_scroll = false;
			if(!nativeScroll) {
				ace_scroll = getSubScroll(sub);
				if(ace_scroll == false) {
					$sub.ace_scroll({
						//hideOnIdle: true,
						observeContent: true,
						detached: true,
						updatePos: false,
						reset: true,
						mouseWheelLock: true,
						styleClass: self.settings.sub_scroll_style
					});
					ace_scroll = getSubScroll(sub);
					
					var track = ace_scroll.get_track();
					if(track) {
						//detach it from body and insert it after submenu for better and cosistent positioning
						$sub.after(track);
					}
				}
				
				ace_scroll.update({size: scroll_height});
			}
			else {
				$sub
				.addClass('sub-scroll')
				.css('max-height', (scroll_height)+'px')
			}


			lastScrollHeight = scroll_height;
			if(!nativeScroll && ace_scroll) {
				if(scroll_height > 14 && sub_h - scroll_height > 4) {
					ace_scroll.enable()
					ace_scroll.reset();
				}			
				else {
					ace_scroll.disable();
				}

				//////////////////////////////////
				var track = ace_scroll.get_track();
				if(track) {
					track.style.top = -(move_up - extra - 1) + 'px';
					
					var off = $sub.position();
					var left = off.left 
					if( !scroll_right ) {
						left += ($sub.outerWidth() - ace_scroll.track_size());
					}
					else {
						left += 2;
					}
					track.style.left = parseInt(left) + 'px';
					
					if(horizontal_sub) {//first level submenu
						track.style.left = parseInt(left - 2) + 'px';
						track.style.top = parseInt(off.top) + (menu_text ? extra - 2 : 0) + 'px';
					}
				}
			}
		//}


		//again force redraw for safari!
		if( ace.vars['safari'] ) {
			ace.helper.redraw(sub)
		}
   }

}
 
 
 
 /////////////////////////////////////////////
 $.fn.ace_sidebar_hover = function (option, value) {
	var method_call;

	var $set = this.each(function () {
		var $this = $(this);
		var data = $this.data('ace_sidebar_hover');
		var options = typeof option === 'object' && option;

		if (!data) $this.data('ace_sidebar_hover', (data = new Sidebar_Hover(this, options)));
		if (typeof option === 'string' && typeof data[option] === 'function') {
			method_call = data[option](value);
		}
	});

	return (method_call === undefined) ? $set : method_call;
 }
 
  $.fn.ace_sidebar_hover.defaults = {
	'sub_sub_hover_delay': 750,
	'sub_scroll_style': 'no-track scroll-thin'
 }
 

})(window.jQuery);

