/**
 Required. Ace's Basic File to Initiliaze Different Parts and Some Variables.
*/


//some basic variables
(function(undefined) {
	if( !('ace' in window) ) window['ace'] = {}
	if( !('helper' in window['ace']) ) window['ace'].helper = {}
	if( !('vars' in window['ace']) ) window['ace'].vars = {}
	window['ace'].vars['icon'] = ' ace-icon ';
	window['ace'].vars['.icon'] = '.ace-icon';

	ace.vars['touch']	= ('ontouchstart' in window);//(('ontouchstart' in document.documentElement) || (window.DocumentTouch && document instanceof DocumentTouch));
	
	//sometimes the only good way to work around browser's pecularities is to detect them using user-agents
	//though it's not accurate
	var agent = navigator.userAgent
	ace.vars['webkit'] = !!agent.match(/AppleWebKit/i)
	ace.vars['safari'] = !!agent.match(/Safari/i) && !agent.match(/Chrome/i);
	ace.vars['android'] = ace.vars['safari'] && !!agent.match(/Android/i)
	ace.vars['ios_safari'] = !!agent.match(/OS ([4-9])(_\d)+ like Mac OS X/i) && !agent.match(/CriOS/i)
	
	ace.vars['ie'] = window.navigator.msPointerEnabled || (document.all && document.querySelector);//8-11
	ace.vars['old_ie'] = document.all && !document.addEventListener;//8 and below
	ace.vars['very_old_ie']	= document.all && !document.querySelector;//7 and below
	ace.vars['firefox'] = 'MozAppearance' in document.documentElement.style;
	
	ace.vars['non_auto_fixed'] = ace.vars['android'] || ace.vars['ios_safari'];
})();



(function($ , undefined) {
	//sometimes we try to use 'tap' event instead of 'click' if jquery mobile plugin is available
	ace['click_event'] = ace.vars['touch'] && $.fn.tap ? 'tap' : 'click';
})(jQuery);




//document ready function
jQuery(function($) {
	basics();
	enableSidebar();
	
	enableDemoAjax();
	handleScrollbars();
	
	dropdownAutoPos();
	
	navbarHelpers();
	sidebarTooltips();
	
	scrollTopBtn();
	
	someBrowserFix();
	
	bsCollapseToggle();
	smallDeviceDropdowns();
	
	////////////////////////////

	function basics() {
		// for android and ios we don't use "top:auto" when breadcrumbs is fixed
		if(ace.vars['non_auto_fixed']) {
			$('body').addClass('mob-safari');
		}

		ace.vars['transition'] = !!$.support.transition.end
	}
	
	function enableSidebar() {
		//initiate sidebar function
		var $sidebar = $('.sidebar');
		if($.fn.ace_sidebar) $sidebar.ace_sidebar();
		if($.fn.ace_sidebar_scroll) $sidebar.ace_sidebar_scroll({
			//for other options please see documentation
			'include_toggle': false || ace.vars['safari'] || ace.vars['ios_safari'] //true = include toggle button in the scrollbars
		});
		if($.fn.ace_sidebar_hover)	$sidebar.ace_sidebar_hover({
			'sub_hover_delay': 750,
			'sub_scroll_style': 'no-track scroll-thin scroll-margin scroll-visible'
		});
	}

	
	//Load content via ajax
	function enableDemoAjax() {		
		if(!$.fn.ace_ajax) return;
 
		if(window.Pace) {
			window.paceOptions = {
				ajax: true,
				document: true,
				eventLag: false // disabled
				//elements: {selectors: ['.page-content-area']}
			}
		}

		var demo_ajax_options = {
			 'close_active': true,
			 
			 'default_url': $("#_ace").attr("data-path"),//default hash
			 'content_url': function(hash) {
				//***NOTE***
				//this is for Ace demo only, you should change it to return a valid URL
				//please refer to documentation for more info

				if( !hash.match(/^page\//) ) return false;
				var path = document.location.pathname;

				//for example in Ace HTML demo version we convert /ajax/index.html#page/gallery to > /ajax/content/gallery.html and load it
				if(path.match(/(\/ajax\/)(index\.html)?/))
					return path.replace(/(\/ajax\/)(index\.html)?/, '/ajax/content/'+hash.replace(/^page\//, '')+'.html') ;

				//for example in Ace PHP demo version we convert "ajax.php#page/dashboard" to "ajax.php?page=dashboard" and load it
				return path + "?" + hash.replace(/\//, "=");
			  }			  
		}
		   
		//for IE9 and below we exclude PACE loader (using conditional IE comments)
		//for other browsers we use the following extra ajax loader options
		if(window.Pace) {
			demo_ajax_options['loading_overlay'] = 'body';//the opaque overlay is applied to 'body'
		}

		//initiate ajax loading on this element( which is .page-content-area[data-ajax-content=true] in Ace's demo)
		$('[data-ajax-content=true]').ace_ajax(demo_ajax_options)

		//if general error happens and ajax is working, let's stop loading icon & PACE
		$(window).on('error.ace_ajax', function() {
			$('[data-ajax-content=true]').each(function() {
				var $this = $(this);
				if( $this.ace_ajax('working') ) {
					if(window.Pace && Pace.running) Pace.stop();
					$this.ace_ajax('stopLoading', true);
				}
			})
		})
	}

	/////////////////////////////

	function handleScrollbars() {
		//add scrollbars for navbar dropdowns
		var has_scroll = !!$.fn.ace_scroll;
		if(has_scroll) $('.dropdown-content').ace_scroll({reset: false, mouseWheelLock: true})

		//reset scrolls bars on window resize
		if(has_scroll && !ace.vars['old_ie']) {//IE has an issue with widget fullscreen on ajax?!!!
			$(window).on('resize.reset_scroll', function() {
				$('.ace-scroll:not(.scroll-disabled)').not(':hidden').ace_scroll('reset');
			});
			if(has_scroll) $(document).on('settings.ace.reset_scroll', function(e, name) {
				if(name == 'sidebar_collapsed') $('.ace-scroll:not(.scroll-disabled)').not(':hidden').ace_scroll('reset');
			});
		}
	}


	function dropdownAutoPos() {
		//change a dropdown to "dropup" depending on its position
		$(document).on('click.dropdown.pos', '.dropdown-toggle[data-position="auto"]', function() {
			var offset = $(this).offset();
			var parent = $(this.parentNode);

			if ( parseInt(offset.top + $(this).height()) + 50 
					>
				(ace.helper.scrollTop() + ace.helper.winHeight() - parent.find('.dropdown-menu').eq(0).height()) 
				) parent.addClass('dropup');
			else parent.removeClass('dropup');
		});
	}

	
	function navbarHelpers() {
		//prevent dropdowns from hiding when a from is clicked
		/**$(document).on('click', '.dropdown-navbar form', function(e){
			e.stopPropagation();
		});*/


		//disable navbar icon animation upon click
		$('.ace-nav [class*="icon-animated-"]').closest('a').one('click', function(){
			var icon = $(this).find('[class*="icon-animated-"]').eq(0);
			var $match = icon.attr('class').match(/icon\-animated\-([\d\w]+)/);
			icon.removeClass($match[0]);
		});


		//prevent dropdowns from hiding when a tab is selected
		$(document).on('click', '.dropdown-navbar .nav-tabs', function(e){
			e.stopPropagation();
			var $this , href
			var that = e.target
			if( ($this = $(e.target).closest('[data-toggle=tab]')) && $this.length > 0) {
				$this.tab('show');
				e.preventDefault();
				$(window).triggerHandler('resize.navbar.dropdown')
			}
		});
	}

	
	function sidebarTooltips() {
		//tooltip in sidebar items
		$('.sidebar .nav-list .badge[title],.sidebar .nav-list .badge[title]').each(function() {
			var tooltip_class = $(this).attr('class').match(/tooltip\-(?:\w+)/);
			tooltip_class = tooltip_class ? tooltip_class[0] : 'tooltip-error';
			$(this).tooltip({
				'placement': function (context, source) {
					var offset = $(source).offset();

					if( parseInt(offset.left) < parseInt(document.body.scrollWidth / 2) ) return 'right';
					return 'left';
				},
				container: 'body',
				template: '<div class="tooltip '+tooltip_class+'"><div class="tooltip-arrow"></div><div class="tooltip-inner"></div></div>'
			});
		});
		
		//or something like this if items are dynamically inserted
		/**
		$('.sidebar').tooltip({
			'placement': function (context, source) {
				var offset = $(source).offset();

				if( parseInt(offset.left) < parseInt(document.body.scrollWidth / 2) ) return 'right';
				return 'left';
			},
			selector: '.nav-list .badge[title],.nav-list .label[title]',
			container: 'body',
			template: '<div class="tooltip tooltip-error"><div class="tooltip-arrow"></div><div class="tooltip-inner"></div></div>'
		});
		*/
	}
	
	

	function scrollTopBtn() {
		//the scroll to top button
		var scroll_btn = $('.btn-scroll-up');
		if(scroll_btn.length > 0) {
			var is_visible = false;
			$(window).on('scroll.scroll_btn', function() {
				var scroll = ace.helper.scrollTop();
				var h = ace.helper.winHeight();
				var body_sH = document.body.scrollHeight;
				if(scroll > parseInt(h / 4) || (scroll > 0 && body_sH >= h && h + scroll >= body_sH - 1)) {//|| for smaller pages, when reached end of page
					if(!is_visible) {
						scroll_btn.addClass('display');
						is_visible = true;
					}
				} else {
					if(is_visible) {
						scroll_btn.removeClass('display');
						is_visible = false;
					}
				}
			}).triggerHandler('scroll.scroll_btn');

			scroll_btn.on(ace.click_event, function(){
				var duration = Math.min(500, Math.max(100, parseInt(ace.helper.scrollTop() / 3)));
				$('html,body').animate({scrollTop: 0}, duration);
				return false;
			});
		}
	}


	
	function someBrowserFix() {
		//chrome and webkit have a problem here when resizing from 479px to more
		//we should force them redraw the navbar!
		if( ace.vars['webkit'] ) {
			var ace_nav = $('.ace-nav').get(0);
			if( ace_nav ) $(window).on('resize.webkit_fix' , function(){
				ace.helper.redraw(ace_nav);
			});
		}
		
		
		//fix an issue with ios safari, when an element is fixed and an input receives focus
		if(ace.vars['ios_safari']) {
		  $(document).on('ace.settings.ios_fix', function(e, event_name, event_val) {
			if(event_name != 'navbar_fixed') return;

			$(document).off('focus.ios_fix blur.ios_fix', 'input,textarea,.wysiwyg-editor');
			if(event_val == true) {
			  $(document).on('focus.ios_fix', 'input,textarea,.wysiwyg-editor', function() {
				$(window).on('scroll.ios_fix', function() {
					var navbar = $('#navbar').get(0);
					if(navbar) ace.helper.redraw(navbar);
				});
			  }).on('blur.ios_fix', 'input,textarea,.wysiwyg-editor', function() {
				$(window).off('scroll.ios_fix');
			  })
			}
		  }).triggerHandler('ace.settings.ios_fix', ['navbar_fixed', $('#navbar').css('position') == 'fixed']);
		}
	}

	
	
	function bsCollapseToggle() {
		//bootstrap collapse component icon toggle
		$(document).on('hide.bs.collapse show.bs.collapse', function (ev) {
			var panel_id = ev.target.getAttribute('id')
			var panel = $('a[href*="#'+ panel_id+'"]');
			if(panel.length == 0) panel = $('a[data-target*="#'+ panel_id+'"]');
			if(panel.length == 0) return;

			panel.find(ace.vars['.icon']).each(function(){
				var $icon = $(this)

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

				if($icon_down) {
					if(ev.type == 'show') $icon.removeClass($icon_down).addClass($icon_up)
						else $icon.removeClass($icon_up).addClass($icon_down)
						
					return false;//ignore other icons that match, one is enough
				}

			});
		})
	}
	

	
	//in small devices display navbar dropdowns like modal boxes
	function smallDeviceDropdowns() {
	  if(ace.vars['old_ie']) return;
	  
	  $('.ace-nav > li')
	  .on('shown.bs.dropdown.navbar', function(e) {
		adjustNavbarDropdown.call(this);
	  })
	  .on('hidden.bs.dropdown.navbar', function(e) {
		$(window).off('resize.navbar.dropdown');
		resetNavbarDropdown.call(this);
	  })
	 
	  function adjustNavbarDropdown() {
		var $sub = $(this).find('> .dropdown-menu');

		if( $sub.css('position') == 'fixed' ) {
			var win_width = parseInt($(window).width());
			var offset_w = win_width > 320 ? 60 : (win_width > 240 ? 40 : 30);
			var avail_width = parseInt(win_width) - offset_w;
			var avail_height = parseInt($(window).height()) - 30;
			
			var width = parseInt(Math.min(avail_width , 320));
			//we set 'width' here for text wrappings and spacings to take effect before calculating scrollHeight
			$sub.css('width', width);

			var tabbed = false;
			var extra_parts = 0;
			var dropdown_content = $sub.find('.tab-pane.active .dropdown-content.ace-scroll');
			if(dropdown_content.length == 0) dropdown_content = $sub.find('.dropdown-content.ace-scroll');
			else tabbed = true;

			var parent_menu = dropdown_content.closest('.dropdown-menu');
			var scrollHeight = $sub[0].scrollHeight;
			if(dropdown_content.length == 1) {
				//sometimes there's no scroll-content, for example in detached scrollbars
				var content =  dropdown_content.find('.scroll-content')[0];
				if(content) {
					scrollHeight = content.scrollHeight;
				}
			
				extra_parts += parent_menu.find('.dropdown-header').outerHeight();
				extra_parts += parent_menu.find('.dropdown-footer').outerHeight();
				
				var tab_content = parent_menu.closest('.tab-content');
				if( tab_content.length != 0 ) {
					extra_parts += tab_content.siblings('.nav-tabs').eq(0).height();
				}
			}
			

			
			var height = parseInt(Math.min(avail_height , 480, scrollHeight + extra_parts));
			var left = parseInt(Math.abs((avail_width + offset_w - width)/2));
			var top = parseInt(Math.abs((avail_height + 30 - height)/2));

			
			var zindex = parseInt($sub.css('z-index')) || 0;

			$sub.css({'height': height, 'left': left, 'right': 'auto', 'top': top - (!tabbed ? 1 : 3)});
			if(dropdown_content.length == 1) {
				if(!ace.vars['touch']) {
					dropdown_content.ace_scroll('update', {size: height - extra_parts}).ace_scroll('enable').ace_scroll('reset');
				}
				else {
					dropdown_content
					.ace_scroll('disable').css('max-height', height - extra_parts).addClass('overflow-scroll');
				}
			}
			$sub.css('height', height + (!tabbed ? 2 : 7));//for bottom border adjustment and tab content paddings
			
			
			if($sub.hasClass('user-menu')) {
				$sub.css('height', '');//because of user-info hiding/showing at different widths, which changes above 'scrollHeight', so we remove it!
				
				//user menu is re-positioned in small widths
				//but we need to re-position again in small heights as well (modal mode)
				var user_info = $(this).find('.user-info');
				if(user_info.length == 1 && user_info.css('position') == 'fixed') {
					user_info.css({'left': left, 'right': 'auto', 'top': top, 'width': width - 2, 'max-width': width - 2, 'z-index': zindex + 1});
				}
				else user_info.css({'left': '', 'right': '', 'top': '', 'width': '', 'max-width': '', 'z-index': ''});
			}
			
			//dropdown's z-index is limited by parent .navbar's z-index (which doesn't make sense because dropdowns are fixed!)
			//so for example when in 'content-slider' page, fixed modal toggle buttons go above are dropdowns
			//so we increase navbar's z-index to fix this!
			$(this).closest('.navbar.navbar-fixed-top').css('z-index', zindex);
		}
		else {
			if($sub.length != 0) resetNavbarDropdown.call(this, $sub);
		}
		
		var self = this;
		$(window)
		.off('resize.navbar.dropdown')
		.one('resize.navbar.dropdown', function() {
			$(self).triggerHandler('shown.bs.dropdown.navbar');
		})
	  }

	  //reset scrollbars and user menu
	  function resetNavbarDropdown($sub) {
		$sub = $sub || $(this).find('> .dropdown-menu');
	  
	    if($sub.length > 0) {
			$sub
			.css({'width': '', 'height': '', 'left': '', 'right': '', 'top': ''})
			.find('.dropdown-content').each(function() {
				if(ace.vars['touch']) {
					$(this).css('max-height', '').removeClass('overflow-scroll');
				}

				var size = parseInt($(this).attr('data-size') || 0) || $.fn.ace_scroll.defaults.size;
				$(this).ace_scroll('update', {size: size}).ace_scroll('enable').ace_scroll('reset');
			})
			
			if( $sub.hasClass('user-menu') ) {
				var user_info = 
				$(this).find('.user-info')
				.css({'left': '', 'right': '', 'top': '', 'width': '', 'max-width': '', 'z-index': ''});
			}
		}
		
		$(this).closest('.navbar').css('z-index', '');
	  }
	}

});//jQuery document ready





//some ace helper functions
(function($$ , undefined) {//$$ is ace.helper
 $$.unCamelCase = function(str) {
	return str.replace(/([a-z])([A-Z])/g, function(match, c1, c2){ return c1+'-'+c2.toLowerCase() })
 }
 $$.strToVal = function(str) {
	var res = str.match(/^(?:(true)|(false)|(null)|(\-?[\d]+(?:\.[\d]+)?)|(\[.*\]|\{.*\}))$/i);

	var val = str;
	if(res) {
		if(res[1]) val = true;
		else if(res[2]) val = false;
		else if(res[3]) val = null;	
		else if(res[4]) val = parseFloat(str);
		else if(res[5]) {
			try { val = JSON.parse(str) }
			catch (err) {}
		}
	}

	return val;
 }
 $$.getAttrSettings = function(el, attr_list, prefix) {
	var list_type = attr_list instanceof Array ? 1 : 2;
	//attr_list can be Array or Object(key/value)
	var prefix = prefix ? prefix.replace(/([^\-])$/ , '$1-') : '';
	prefix = 'data-' + prefix;

	var settings = {}
	for(var li in attr_list) if(attr_list.hasOwnProperty(li)) {
		var name = list_type == 1 ? attr_list[li] : li;
		var attr_val, attr_name = $$.unCamelCase(name.replace(/[^A-Za-z0-9]{1,}/g , '-')).toLowerCase()

		if( ! ((attr_val = el.getAttribute(prefix + attr_name))  ) ) continue;
		settings[name] = $$.strToVal(attr_val);
	}

	return settings;
 }

 $$.scrollTop = function() {
	return document.scrollTop || document.documentElement.scrollTop || document.body.scrollTop
 }
 $$.winHeight = function() {
	return window.innerHeight || document.documentElement.clientHeight;
 }
 $$.redraw = function(elem, force) {
	var saved_val = elem.style['display'];
	elem.style.display = 'none';
	elem.offsetHeight;
	if(force !== true) {
		elem.style.display = saved_val;
	}
	else {
		//force redraw for example in old IE
		setTimeout(function() {
			elem.style.display = saved_val;
		}, 10);
	}
 }
})(ace.helper);