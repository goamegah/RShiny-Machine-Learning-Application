$(document).ready(function() {
  const targetNode = document.getElementsByClassName('nav-tabs-custom')[0];
  const config = { attributes: true, childList: true, subtree: true };

 // Callback function to execute when mutations are observed
 const callback = function(mutationsList, observer) {
     // Use jQuery to get the height of the tab-content
     const tabContentHeight = $('.nav-tabs-custom').height();
     // Use jQuery to get the min-height of the content-wrapper
     const contentWrapperHeight = $('.content-wrapper').css('min-height').replace('px', '');

     // Check if the tab-content height is greater than 888
     if (tabContentHeight +25 > contentWrapperHeight) {
         // Set the min-height of the content-wrapper to the tab-content height plus 101 pixels
         $('.content-wrapper').css('min-height', (tabContentHeight + 33) + 'px');
     }
 };
  const observer = new MutationObserver(callback);
  if (targetNode) {
    observer.observe(targetNode, config);
  }
  // Add an event listener to the window that triggers the callback function when the window is resized:
  // because height of nav-tabs-custom doesn't change when resizing window
    $(window).resize(function() {
      callback();
    });
});
