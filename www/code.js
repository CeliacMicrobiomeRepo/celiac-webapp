// Wait for the DOM to be fully loaded
$(document).ready(function() {

  // Define the HTML for the GitHub icon link button.
  var githubIconBtn = '<li>' +
                      '<a href="https://github.com/CeliacMicrobiomeRepo/celiac-repository" target="_blank" style="padding-top: 15px; padding-bottom: 15px;" title="View Source on GitHub">' +
                      '<img src="github-mark-white.svg" height="20px" style="vertical-align: middle;">' +
                      '</a>' +
                      '</li>';

  // Append the generated icon button list to the right side of the navbar.
  // This targets the first ul.nav.navbar-nav found, and places the new ul after it.
  $('.navbar-nav').first().after('<ul class="nav navbar-nav navbar-right">' + githubIconBtn + '</ul>');

}); 