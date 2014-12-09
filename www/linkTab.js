// Script to change active tab to the Remarks tab.
// Inspired from https://gist.github.com/xiaodaigh/6445698
//
// Copyright 12/2014 Antoine Lizee antoine.lizee@gmail.com

$('#linkToRemarks').click(function() {
tabs = $('.tabbable .nav.nav-tabs li')
tabs.each(function() {
$(this).removeClass('active')
})
$(tabs[1]).addClass('active')
tabsContents = $('.tabbable .tab-content .tab-pane')
tabsContents.each(function() {
$(this).removeClass('active')
})
$(tabsContents[1]).addClass('active')
$('#Remarks').trigger('change').trigger('shown'); 
})