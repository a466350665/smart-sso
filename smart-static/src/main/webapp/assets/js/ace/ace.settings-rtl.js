/**
<b>RTL</b> (right-to-left direction for Arabic, Hebrew, Persian languages).
It's good for demo only.
You should hard code RTL-specific changes inside your HTML/server-side code.
Dynamically switching to RTL using Javascript is not a good idea.
Please refer to documentation for more info.
*/


(function($ , undefined) {
 //Switching to RTL (right to left) Mode
 $('#ace-settings-rtl').removeAttr('checked').on('click', function(){
	switch_direction();
 });
 
 
 //>>> you should hard code changes inside HTML for RTL direction
 //you shouldn't use this function to switch direction
 //this is only for dynamically switching for demonstration
 //take a look at this function to see what changes should be made
 //also take a look at docs for some tips
 var switch_direction = function() {
	if($('#ace-rtl-stylesheet').length == 0) {
		//let's load RTL stylesheet only when needed!
		var ace_style = $('head').find('link.ace-main-stylesheet');
		if(ace_style.length == 0) {
			ace_style = $('head').find('link[href*="/ace.min.css"],link[href*="/ace-part2.min.css"]');
			if(ace_style.length == 0) {
				ace_style = $('head').find('link[href*="/ace.css"],link[href*="/ace-part2.css"]');
			}
		}
		
		var ace_skins = $('head').find('link#ace-skins-stylesheet');
		var stylesheet_url = ace_style.first().attr('href').replace(/(\.min)?\.css$/i , '-rtl$1.css');
		$.ajax({
			'url': stylesheet_url
		}).done(function() {
			var new_link = jQuery('<link />', {type : 'text/css', rel: 'stylesheet', 'id': 'ace-rtl-stylesheet'})
			if(ace_skins.length > 0) {
				new_link.insertAfter(ace_skins);
			}
			else if(ace_style.length > 0){
				new_link.insertAfter(ace_style.last());
			}
			else new_link.appendTo('head');
		
			new_link.attr('href', stylesheet_url);
			//we set "href" after insertion, for IE to work
			
			applyChanges();
			if(window.Pace && Pace.running)	Pace.stop();
		})		
	}
	else {
		applyChanges();
	}
	
	//in ajax when new content is loaded, we dynamically apply RTL changes again
	//please note that this is only for Ace demo
	//for info about RTL see Ace's docs
	$('.page-content-area[data-ajax-content=true]').on('ajaxscriptsloaded.rtl', function() {
		if( $('body').hasClass('rtl') ) {
			applyChanges(this);
		}
	});

	/////////////////////////
	function applyChanges(el) {
		var $body = $(document.body);
		if(!el) $body.toggleClass('rtl');//el is 'body'

		el = el || document.body;		
		var $container = $(el);
		$container
		//toggle pull-right class on dropdown-menu
		.find('.dropdown-menu:not(.datepicker-dropdown,.colorpicker)').toggleClass('dropdown-menu-right')
		.end()
		//swap pull-left & pull-right
		.find('.pull-right:not(.dropdown-menu,blockquote,.profile-skills .pull-right)').removeClass('pull-right').addClass('tmp-rtl-pull-right')
		.end()
		.find('.pull-left:not(.dropdown-submenu,.profile-skills .pull-left)').removeClass('pull-left').addClass('pull-right')
		.end()
		.find('.tmp-rtl-pull-right').removeClass('tmp-rtl-pull-right').addClass('pull-left')
		.end()
		
		.find('.chosen-select').toggleClass('chosen-rtl').next().toggleClass('chosen-rtl');
		

		function swap_classes(class1, class2) {
			$container
			 .find('.'+class1).removeClass(class1).addClass('tmp-rtl-'+class1)
			 .end()
			 .find('.'+class2).removeClass(class2).addClass(class1)
			 .end()
			 .find('.tmp-rtl-'+class1).removeClass('tmp-rtl-'+class1).addClass(class2)
		}

		swap_classes('align-left', 'align-right');
		swap_classes('no-padding-left', 'no-padding-right');
		swap_classes('arrowed', 'arrowed-right');
		swap_classes('arrowed-in', 'arrowed-in-right');
		swap_classes('tabs-left', 'tabs-right');
		swap_classes('messagebar-item-left', 'messagebar-item-right');//for inbox page
		
		$('.modal.aside-vc').ace_aside('flip').ace_aside('insideContainer');
		
		
		//mirror all icons and attributes that have a "fa-*-right|left" attrobute
		$container.find('.fa').each(function() {
			if(this.className.match(/ui-icon/) || $(this).closest('.fc-button').length > 0) return;
			//skip mirroring icons of plugins that have built in RTL support

			var l = this.attributes.length;
			for(var i = 0 ; i < l ; i++) {
				var val = this.attributes[i].value;
				if(val.match(/fa\-(?:[\w\-]+)\-left/)) 
					this.attributes[i].value = val.replace(/fa\-([\w\-]+)\-(left)/i , 'fa-$1-right')
				 else if(val.match(/fa\-(?:[\w\-]+)\-right/)) 
					this.attributes[i].value = val.replace(/fa\-([\w\-]+)\-(right)/i , 'fa-$1-left')
			}
		});
		
		//browsers are incosistent with horizontal scroll and RTL
		//so let's make our scrollbars LTR and wrap the content inside RTL
		var rtl = $body.hasClass('rtl');
		if(rtl)	{
			$container.find('.scroll-hz').addClass('make-ltr')
			.find('.scroll-content')
			.wrapInner('<div class="make-rtl" />');
			$('.sidebar[data-sidebar-hover=true]').ace_sidebar_hover('changeDir', 'right');
		}
		else {
			//remove the wrap
			$container.find('.scroll-hz').removeClass('make-ltr')
			.find('.make-rtl').children().unwrap();
			$('.sidebar[data-sidebar-hover=true]').ace_sidebar_hover('changeDir', 'left');
		}
		if($.fn.ace_scroll) $container.find('.scroll-hz').ace_scroll('reset') //to reset scrollLeft

		//redraw the traffic pie chart on homepage with a different parameter
		try {
			var placeholder = $('#piechart-placeholder');
			if(placeholder.length > 0) {
				var pos = $body.hasClass('rtl') ? 'nw' : 'ne';//draw on north-west or north-east?
				placeholder.data('draw').call(placeholder.get(0) , placeholder, placeholder.data('chart'), pos);
			}
		}catch(e) {}
		
		
		ace.helper.redraw(el, true);
	}
 }
})(jQuery);


