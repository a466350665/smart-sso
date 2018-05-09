/**
 <b>Wizard</b>. A wrapper for FuelUX wizard element.
 It's just a wrapper so you still need to include FuelUX wizard script first.
*/
(function($ , undefined) {
	$.fn.aceWizard = $.fn.ace_wizard = function(options) {

		this.each(function() {
			var $this = $(this);
			$this.wizard();
			
			if(ace.vars['old_ie']) $this.find('ul.steps > li').last().addClass('last-child');

			var buttons = (options && options['buttons']) ? $(options['buttons']) : $this.siblings('.wizard-actions').eq(0);
			var $wizard = $this.data('fu.wizard');
			$wizard.$prevBtn.remove();
			$wizard.$nextBtn.remove();
			
			$wizard.$prevBtn = buttons.find('.btn-prev').eq(0).on(ace.click_event,  function(){
				$wizard.previous();
			}).attr('disabled', 'disabled');
			$wizard.$nextBtn = buttons.find('.btn-next').eq(0).on(ace.click_event,  function(){
				$wizard.next();
			}).removeAttr('disabled');
			$wizard.nextText = $wizard.$nextBtn.text();
			
			var step = options && ((options.selectedItem && options.selectedItem.step) || options.step);
			if(step) {
				$wizard.currentStep = step;
				$wizard.setState();
			}
		});

		return this;
	}

})(window.jQuery);
