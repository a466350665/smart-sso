/**
 <b>Select a different skin</b>. It's good for demo only.
 You should hard code skin-specific changes inside your HTML/server-side code.
 Please refer to documentation for more info.
*/

(function($ , undefined) {
  try {
	$('#skin-colorpicker').ace_colorpicker({'auto_pos': false});
  } catch(e) {}

  $('#skin-colorpicker').on('change', function(){
	var skin_class = $(this).find('option:selected').data('skin');

	if($('#ace-skins-stylesheet').length == 0) {
		//let's load skins stylesheet only when needed!
		var ace_style = $('head').find('link.ace-main-stylesheet');
		if(ace_style.length == 0) {
			ace_style = $('head').find('link[href*="/ace.min.css"],link[href*="/ace-part2.min.css"]');
			if(ace_style.length == 0) {
				ace_style = $('head').find('link[href*="/ace.css"],link[href*="/ace-part2.css"]');
			}
		}
		
		var stylesheet_url = ace_style.first().attr('href').replace(/(\.min)?\.css$/i , '-skins$1.css');
		$.ajax({
			'url': stylesheet_url
		}).done(function() {
			var new_link = jQuery('<link />', {type : 'text/css', rel: 'stylesheet', 'id': 'ace-skins-stylesheet'})
			if(ace_style.length > 0){
				new_link.insertAfter(ace_style.last());
			}
			else new_link.appendTo('head');
	
			new_link.attr('href', stylesheet_url);
			//we set "href" after insertion, for IE to work
			
			applyChanges(skin_class);
			if(window.Pace && Pace.running)	Pace.stop();
		})
	}
	else {
		applyChanges(skin_class);
	}


	function applyChanges(skin_class) {
		//skin cookie tip
		var body = $(document.body);
		body.removeClass('no-skin skin-1 skin-2 skin-3');
		//if(skin_class != 'skin-0') {
			body.addClass(skin_class);
			ace.data.set('skin', skin_class);
			//save the selected skin to cookies
			//which can later be used by your server side app to set the skin
			//for example: <body class="<?php echo $_COOKIE['ace_skin']; ?>"
		//} else ace.data.remove('skin');
		
		var skin3_colors = ['red', 'blue', 'green', ''];

		
			//undo skin-1
			$('.ace-nav > li.grey').removeClass('dark');
			
			//undo skin-2
			$('.ace-nav > li').removeClass('no-border margin-1');
			$('.ace-nav > li:not(:last-child)').removeClass('light-pink').find('> a > '+ace.vars['.icon']).removeClass('pink').end().eq(0).find('.badge').removeClass('badge-warning');
			$('.sidebar-shortcuts .btn')
			.removeClass('btn-pink btn-white')
			.find(ace.vars['.icon']).removeClass('white');
			
			//undo skin-3
			$('.ace-nav > li.grey').removeClass('red').find('.badge').removeClass('badge-yellow');
			$('.sidebar-shortcuts .btn').removeClass('btn-primary btn-white')
			var i = 0;
			$('.sidebar-shortcuts .btn').each(function() {
				$(this).find(ace.vars['.icon']).removeClass(skin3_colors[i++]);
			})
		
		

		
		var skin0_buttons = ['btn-success', 'btn-info', 'btn-warning', 'btn-danger'];
		if(skin_class == 'no-skin') {
			var i = 0;
			$('.sidebar-shortcuts .btn').each(function() {
				$(this).attr('class', 'btn ' + skin0_buttons[i++%4]);
			})
			
			$('.sidebar[data-sidebar-scroll=true]').ace_sidebar_scroll('updateStyle', '');
			$('.sidebar[data-sidebar-hover=true]').ace_sidebar_hover('updateStyle', 'no-track scroll-thin');
		}

		else if(skin_class == 'skin-1') {
			$('.ace-nav > li.grey').addClass('dark');
			var i = 0;
			$('.sidebar-shortcuts')
			.find('.btn').each(function() {
				$(this).attr('class', 'btn ' + skin0_buttons[i++%4]);
			})
			
			$('.sidebar[data-sidebar-scroll=true]').ace_sidebar_scroll('updateStyle', 'scroll-white no-track');
			$('.sidebar[data-sidebar-hover=true]').ace_sidebar_hover('updateStyle', 'no-track scroll-thin scroll-white');
		}

		else if(skin_class == 'skin-2') {
			$('.ace-nav > li').addClass('no-border margin-1');
			$('.ace-nav > li:not(:last-child)').addClass('light-pink').find('> a > '+ace.vars['.icon']).addClass('pink').end().eq(0).find('.badge').addClass('badge-warning');
			
			$('.sidebar-shortcuts .btn').attr('class', 'btn btn-white btn-pink')
			.find(ace.vars['.icon']).addClass('white');
			
			$('.sidebar[data-sidebar-scroll=true]').ace_sidebar_scroll('updateStyle', 'scroll-white no-track');
			$('.sidebar[data-sidebar-hover=true]').ace_sidebar_hover('updateStyle', 'no-track scroll-thin scroll-white');
		}

		//skin-3
		//change shortcut buttons classes, this should be hard-coded if you want to choose this skin
		else if(skin_class == 'skin-3') {
			body.addClass('no-skin');//because skin-3 has many parts of no-skin as well
			
			$('.ace-nav > li.grey').addClass('red').find('.badge').addClass('badge-yellow');
			
			var i = 0;
			$('.sidebar-shortcuts .btn').each(function() {
				$(this).attr('class', 'btn btn-primary btn-white');
				$(this).find(ace.vars['.icon']).addClass(skin3_colors[i++]);
			})
			
			$('.sidebar[data-sidebar-scroll=true]').ace_sidebar_scroll('updateStyle', 'scroll-dark no-track');
			$('.sidebar[data-sidebar-hover=true]').ace_sidebar_hover('updateStyle', 'no-track scroll-thin');
		}

		//some sizing differences may be there in skins, so reset scrollbar size
		$('.sidebar[data-sidebar-scroll=true]').ace_sidebar_scroll('reset')
		//$('.sidebar[data-sidebar-hover=true]').ace_sidebar_hover('reset')
		
		if(ace.vars['old_ie']) ace.helper.redraw(document.body, true);
	}

 })
})(jQuery);