// Script to active links to tabs in the url
// Inspired from http://stackoverflow.com/questions/7862233/twitter-bootstrap-tabs-go-to-specific-tab-on-page-reload-or-hyperlink
//
// Copyright 12/2014 Antoine Lizee antoine.lizee@gmail.com

// The biggest issue is that shiny automatically allocates random identifiers for the panels based on a random identifier for the tapsetPanelId (undefined in a navbar)

// getID lookups the corresponding id of the panel objetc, based on the links in the navbar
function getID(hash) {
  return $('.nav a:contains("' + hash + '")')[0].href.split('#')[1]
}

// getHash looks at the href element of the navbar list elements to do the reverse lookup and give the hash based on the tab id.
function getHash(id) {
  var wordsOfTitle = $('.nav a[href='+ id +']').text().split(' ')
  return '#' + wordsOfTitle[wordsOfTitle.length-1]
}

var url = document.location.toString();
if (url.match('#')) {
  var tabName = getID(url.split('#')[1])
    $('.nav a[href=#'+ tabName +']').tab('show') ;
    window.scrollTo(0, 0);
} 

// Change hash for page-reload
$('.nav a').on('shown.bs.tab', function (e) {
    window.location.hash = getHash(e.target.hash);
        window.scrollTo(0, 0);
})