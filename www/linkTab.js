// Script to change active tab to the Remarks tab.
// Inspired from https://gist.github.com/xiaodaigh/6445698
// NOT USED ANYMORE, replaced by the broader linkToTab.js
//
// Copyright 12/2014 Antoine Lizee antoine.lizee@gmail.com

$('#linkToRemarks').click(function() {
tabs = $('.nav li')
tabs.each(function() {
$(this).removeClass('active')
})
$(tabs[3]).addClass('active')
tabsContents = $('.tab-content .tab-pane')
tabsContents.each(function() {
$(this).removeClass('active')
})
$(tabsContents[3]).addClass('active')
$('#Remarks').trigger('change').trigger('shown')
window.scrollTo(0, 0);
})

