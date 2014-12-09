// Script to active links to tab to the Remarks tab.
// Inspired from http://stackoverflow.com/questions/7862233/twitter-bootstrap-tabs-go-to-specific-tab-on-page-reload-or-hyperlink
//
// Copyright 12/2014 Antoine Lizee antoine.lizee@gmail.com

var url = document.location.toString();
if (url.match('#')) {
    $('.nav-tabs a[href=#'+url.split('#')[1]+']').tab('show') ;
    window.scrollTo(0, 0);
} 

// Change hash for page-reload
$('.nav-tabs a').on('shown', function (e) {
    window.location.hash = e.target.hash;
})